import argon2 from "argon2";
import { Request, Response, Router } from "express";
import { validationResult } from "express-validator/check";
import { v4 } from "uuid";
import LastAuthorizedModel from "../../../models/LastAuthorized";
import UserModel from "../../../models/user";
import VerificationTokenModel from "../../../models/VerificationToken";
import responses from "../../../responses";
import { currentSemesterYear } from "../../../utils/dateTime";
import { fetchInformaticsStudent } from "../../../utils/fsLogin";
import { vOrg, vPassword, vUsername } from "../../../validators";

const router = Router();

const inputValidator = [vUsername, vPassword, vOrg];

router.post("/", inputValidator, async (req: Request, res: Response) => {
  const errors = validationResult(req);
  if (!errors.isEmpty()) {
    return responses.badRequest(req, res);
  }

  const { username, password, org } = req.body;

  try {
    // Check if user already exists
    const user = await UserModel.get(username);
    const exists = user !== undefined;
    const curSemYear = currentSemesterYear();

    if (exists) {
      // Check that the org (UiB or HVL) matches user's org
      if (user.org !== org) {
        return responses.unauthorized(res);
      }

      // Verify that the student is verified this semester
      const lastAuth = await LastAuthorizedModel.get(user.id);

      if (
        lastAuth.year !== curSemYear.year ||
        lastAuth.semester !== curSemYear.semester
      ) {
        // User is not verified, re-verify the user.
        responses.accepted({}, "user-verification", res);
        return;
      }

      // Verify users password
      if (await argon2.verify(user.hash, password)) {
        // Responding with session token
        responses.session(req, res, user);
      } else {
        responses.unauthorized(res);
      }
    } else {
      // Check if the user already has a token pending.
      let curVerToken = await VerificationTokenModel.get(username);
      if (curVerToken === undefined) {
        // First time this student signs in; check that the student is an informatics student.
        const student = await fetchInformaticsStudent(username, password, org);
        if (student === undefined) {
          // Could not sign into StudentWeb
          return responses.unauthorized(res);
        } else if (!student || student.studies.length === 0) {
          return responses.forbidden({}, res);
        }

        // As of now, we just select the first study to be current studyprogram.
        const study = student.studies[0];

        // Note: we also include the hashed password with the token to verify the user at a later time.
        curVerToken = await VerificationTokenModel.create({
          id: username,
          hash: await argon2.hash(password),
          token: v4(),
          firstName: student.details.firstName,
          lastName: student.details.lastName,
          email: student.details.email,
          org,
          studyProgram: study.program,
          startYear: study.startYear,
          startSemester: study.startSemester,
          year: study.year
        });
      } else {
        // Make sure the password is correct before returing the token
        const passwordOk = await argon2.verify(curVerToken.hash, password);
        if (!passwordOk) {
          return responses.forbidden({}, res);
        }

        // Passwords match, return the token from curVerToken
      }
      responses.accepted(
        {
          token: curVerToken.token,
          email: curVerToken.email
        },
        "first-time-setup",
        res
      );
    }
  } catch (err) {
    responses.unexpectedError(err, res);
  }
});

export default router;

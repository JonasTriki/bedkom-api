import argon2 from "argon2";
import {Request, Response, Router} from "express";
import {body, validationResult} from "express-validator/check";
import {v4} from "uuid";
import LastAuthorizedModel from "../../../models/LastAuthorized";
import UserModel from "../../../models/user";
import VerificationTokenModel from "../../../models/VerificationToken";
import responses from "../../../responses";
import {currentSemesterYear} from "../../../utils/dateTime";
import {fetchInformaticsStudent} from "../../../utils/fsLogin";
import {vOrg, vPassword, vUsername} from "../../../validators";

const router = Router();

const inputValidator = [
  vUsername,
  vPassword,
  vOrg,
  body("verificationToken").isUUID(4).optional(),
  body("email").isEmail().optional(),
];

router.post("/", inputValidator, async (req: Request, res: Response) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return responses.badRequest(req, res);
    }

    const {username, password, org, verificationToken, email} = req.body;

    try {

      // Check if user already exists
      const user = await UserModel.get(username);
      const exists = user !== undefined;
      const curSemYear = currentSemesterYear();

      if (exists) {

        // Verify that the student is verified this semester
        const lastAuth = await LastAuthorizedModel.get(user.id);

        if (lastAuth.year !== curSemYear.year || lastAuth.semester !== curSemYear.semester) {

          // User is not verified, re-verify the user.
          responses.accepted({}, "user-verification", res);
          return;
        }

        // Verify users password
        if (await argon2.verify(user.hash, password)) {
          console.log(user.hash, password);

          // Responding with session token
          responses.jwt(user, res);
        } else {
          responses.unauthorized(res);
        }
      } else {

        // Check if student already is verified, and now is attempting to sign in using new password
        if (verificationToken !== undefined) {

          const vToken = await VerificationTokenModel.get(username);

          // Check if verification token is valid.
          if (vToken === undefined || vToken.token !== verificationToken) {
            responses.badRequest(req, res);
            return;
          }

          // Add user to Users table and remove verification token from its table.
          const newUser = new UserModel({
            id: username,
            firstName: vToken.firstName,
            lastName: vToken.lastName,
            email: email || vToken.email,
            studyProgram: vToken.studyProgram,
            startYear: vToken.startYear,
            startSemester: vToken.startSemester,
            year: vToken.year,
            role: "student",
            hash: await argon2.hash(password),
          });

          // Add last authorized entry
          const lastAuth = new LastAuthorizedModel({
            id: username,
            year: curSemYear.year,
            semester: curSemYear.semester
          });

          // Save user and delete verification token from databaes.
          await Promise.all([
            newUser.save(),
            lastAuth.save(),
            vToken.delete()
          ]);

          // Responding with session token
          responses.jwt(newUser, res);
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
              studyProgram: study.program,
              startYear: study.startYear,
              startSemester: study.startSemester,
              year: study.year,
            });
          } else {

            // Make sure the password is correct before returing the token
            const passwordOk = await argon2.verify(curVerToken.hash, password);
            if (!passwordOk) {
              return responses.forbidden({}, res);
            }

            // Passwords match, return the token from curVerToken
          }
          responses.accepted({
            token: curVerToken.token,
            firstName: curVerToken.firstName,
            email: curVerToken.email,
          }, "first-login-setup", res);
        }
      }
    } catch (err) {
      responses.unexpectedError(err, res);
    }
  }
);

export default router;

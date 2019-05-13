import argon2 from "argon2";
import {Request, Response, Router} from "express";
import {body, validationResult} from "express-validator/check";
import LastAuthorizedModel from "../../../models/LastAuthorized";
import UserModel from "../../../models/user";
import VerificationTokenModel from "../../../models/VerificationToken";
import responses from "../../../responses";
import {currentSemesterYear} from "../../../utils/dateTime";
import {vPassword, vUsername} from "../../../validators";

const router = Router();

const inputValidator = [
  vUsername,
  vPassword,
  body("verificationToken").isUUID(4),
  body("email").isEmail(),
  body("allergies").isString().optional()
];

router.post("/", inputValidator, async (req: Request, res: Response) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return responses.badRequest(req, res);
    }

    const {username, password, verificationToken, email, allergies} = req.body;

    try {

      // Make sure user does not exist
      const user = await UserModel.get(username);
      const exists = user !== undefined;
      if (exists) {
        return responses.badRequest(req, res);
      }

      const curSemYear = currentSemesterYear();
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
        email,
        allergies,
        org: vToken.org,
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
      responses.session(req, res, newUser);
    } catch (err) {
      responses.unexpectedError(err, res);
    }
  }
);

export default router;

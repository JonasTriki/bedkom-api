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
    body("verificationToken").isUUID(4).optional()
];

router.post("/", inputValidator, async (req: Request, res: Response) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
        return responses.badRequest(req, res);
    }

    const {username, password, org, verificationToken} = req.body;

    // Check if user already exists
    const user = await UserModel.get(username);
    const exists = user !== undefined;
    const curSemYear = currentSemesterYear();

    if (exists) {

        // Verify that the student is verified this semester
        const lastAuth = await LastAuthorizedModel.get(user.id);

        if (lastAuth.year !== curSemYear.year || lastAuth.semester !== curSemYear.semester) {

            // User is not verified, re-verify the user.
            responses.accepted({}, "user-verification-required", res);
            return;
        }

        // Verify users password
        if (argon2.verify(user.hash, password)) {

            // Responding with JWT session token
            responses.jwt(user, res);
        } else {
            responses.badRequest(req, res);
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
                email: vToken.email,
                studyProgram: vToken.studyProgram,
                year: vToken.year,
                semester: vToken.semester,
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

            // Responding with JWT session token
            responses.jwt(newUser, res);
        } else {

            // First time this student signs in; check that the student is an informatics student.
            const student = await fetchInformaticsStudent(username, password, org);
            if (!student || student.studies.length === 0) {
                responses.badRequest(req, res);
                return;
            }

            // As of now, we just select the first study to be current studyprogram.
            const study = student.studies[0];

            // Check if the user already has a token pending.
            const curVerToken = await VerificationTokenModel.get(username);
            let token;
            if (curVerToken === undefined) {
                token = v4();
                await VerificationTokenModel.create({
                    id: username,
                    token,
                    firstName: student.details.firstName,
                    lastName: student.details.lastName,
                    email: student.details.email,
                    studyProgram: study.program,
                    year: study.year,
                    semester: study.semester,
                });
            } else {
                token = curVerToken.token;
            }
            responses.accepted({token}, "password-setup", res);
        }
    }
});

export default router;

import { Request, Response, response, Router } from "express";
import { body, validationResult } from "express-validator/check";
import jwt from "jsonwebtoken";
import config from "../../../config";
import LastAuthorized from "../../../models/LastAuthorized";
import UserModel from "../../../models/user";
import responses from "../../../responses";
import { currentSemesterYear } from "../../../utils/dateTime";
import { fetchInformaticsStudent } from "../../../utils/fsLogin";

const router = Router();

const inputValidator = [
    body("username").isString().custom((value) => value.length === 6),
    body("password").isString(),
    body("org").isIn(["uib", "hvl"])
];

router.post("/", inputValidator, async (req: Request, res: Response) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
        return responses.badRequest(req, res);
    }

    const { username, password, org } = req.body;

    // Check that user already exists
    const user = await UserModel.get(username);
    if (user === undefined) {
        responses.badRequest(req, res);
        return;
    }

    // Fetch studentdetails
    const student = await fetchInformaticsStudent(username, password, org);
    if (!student || student.studies.length === 0) {
        responses.badRequest(req, res);
        return;
    }

    // Add last authorized entry
    const curSemYear = currentSemesterYear();
    const lastAuth = new LastAuthorized({
        id: username,
        year: curSemYear.year,
        semester: curSemYear.semester
    });
    await lastAuth.save();

    // Responding with JWT session token
    responses.jwt(user, res);
});

export default router;

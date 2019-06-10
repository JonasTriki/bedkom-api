import { Request, Response, Router } from "express";
import { validationResult } from "express-validator/check";
import LastAuthorized from "../../../models/LastAuthorized";
import UserModel from "../../../models/user";
import responses from "../../../responses";
import { currentSemesterYear } from "../../../utils/dateTime";
import { fetchInformaticsStudent } from "../../../utils/fsLogin";
import { vPassword, vUsername } from "../../../validators";

const router = Router();

const inputValidator = [vUsername, vPassword];

router.post("/", inputValidator, async (req: Request, res: Response) => {
  const errors = validationResult(req);
  if (!errors.isEmpty()) {
    return responses.badRequest(req, res);
  }

  const { username, password } = req.body;

  // Check that user exists
  const user = await UserModel.get(username);
  if (user === undefined) {
    responses.badRequest(req, res);
    return;
  }

  // Fetch student details
  const student = await fetchInformaticsStudent(username, password, user.org);
  if (student === undefined) {
    return responses.unauthorized(res);
  } else if (!student || student.studies.length === 0) {
    return responses.forbidden({}, res);
  }

  // Add last authorized entry
  const curSemYear = currentSemesterYear();
  const lastAuth = new LastAuthorized({
    id: username,
    year: curSemYear.year,
    semester: curSemYear.semester
  });
  await lastAuth.save();

  // Responding with session token
  responses.session(req, res, user);
});

export default router;

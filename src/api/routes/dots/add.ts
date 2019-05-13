import {NextFunction, Request, Response, Router} from "express";
import {validationResult} from "express-validator/check";
import {v4} from "uuid";
import DotModel from "../../../models/Dot";
import {isPermitted} from "../../../models/enums/UserRoles";
import UserModel from "../../../models/user";
import responses from "../../../responses";
import {currentSemesterYear} from "../../../utils/dateTime";
import {vUsername} from "../../../validators";

const router = Router();

const inputValidator = [
  vUsername,
];

router.post("/", inputValidator, async (req: Request, res: Response, next: NextFunction) => {
  const errors = validationResult(req);
  if (!errors.isEmpty()) {
    return responses.badRequest(req, res);
  }
  if (!isPermitted(req, "bedkom")) {
    return responses.badRequest(req, res);
  }
  next();
});

router.post("/", inputValidator, async (req: Request, res: Response) => {
  const {username, comment} = req.body;

  // Check that user already exists
  const user = await UserModel.get(username);
  if (user === undefined) {
    responses.badRequest(req, res);
    return;
  }

  const sy = currentSemesterYear();

  const dotModel = new DotModel({
    id: v4(),
    userId: user.id,
    year: sy.year,
    semester: sy.semester,
    comment
  });

  // Add dot to user
  await dotModel.save();

  responses.ok(`Dots increased for ${user.id}`, res);
});

export default router;

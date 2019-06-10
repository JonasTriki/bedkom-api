import { NextFunction, Request, Response, Router } from "express";
import { isPermitted } from "../../../models/enums/UserRoles";
import UserModel from "../../../models/User";
import responses from "../../../responses";

const router = Router();

router.get("/", async (req: Request, res: Response, next: NextFunction) => {
  if (!isPermitted(req, "bedkom")) {
    return responses.badRequest(req, res);
  }
  next();
});

router.get("/", async (req: Request, res: Response) => {
  try {
    // List out the users
    const hashedUsers = await UserModel.scan().exec();
    const users = hashedUsers.map(({ hash, ...user }) => user);

    // Return with all users
    responses.ok(users, res);
  } catch (err) {
    responses.unexpectedError(req, res);
  }
});

export default router;

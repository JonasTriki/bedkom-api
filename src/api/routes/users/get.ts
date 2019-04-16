import {Request, Response, Router} from "express";
import UserModel from "../../../models/user";
import responses from "../../../responses";

const router = Router();

router.post("/", async (req: Request, res: Response) => {

  // Check that user exists
  const username = req.session.uid;
  const hashedUser = await UserModel.get(username);
  if (hashedUser === undefined) {
    responses.badRequest(req, res);
    return;
  }
  const {hash, ...user} = hashedUser;

  // Responding with user info
  responses.ok({user}, res);
});

export default router;

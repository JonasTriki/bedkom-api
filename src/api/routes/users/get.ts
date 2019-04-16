import {Request, Response, Router} from "express";
import UserModel from "../../../models/user";
import responses from "../../../responses";
import {csrfProtection} from "../../middlewares/csrftoken";

const router = Router();

router.get("/", csrfProtection, async (req: Request, res: Response) => {

  try {
    // Check that user exists
    const username = req.session.uid;
    const hashedUser = await UserModel.get(username);
    if (hashedUser === undefined) {
      responses.badRequest(req, res);
      return;
    }
    const {hash, ...user} = hashedUser;

    // Responding with user info
    responses.ok({user, csrfToken: req.csrfToken()}, res);
  } catch (err) {
    responses.unexpectedError(err, res);
  }
});

export default router;

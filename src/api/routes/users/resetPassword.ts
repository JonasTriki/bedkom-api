import {Request, Response, Router} from "express";
import {validationResult} from "express-validator/check";
import UserModel from "../../../models/user";
import responses from "../../../responses";
import {vUsername} from "../../../validators";

const router = Router();

const inputValidator = [
  vUsername.optional(),
];

router.post("/", inputValidator, async (req: Request, res: Response) => {
  const errors = validationResult(req);
  if (!errors.isEmpty()) {
    return responses.badRequest(req, res);
  }

  try {
    const {username} = req.body;

    let uid;
    if (username) {
      uid = username;
    } else if (req.session.uid) {
      uid = req.session.uid;
    } else {
      return responses.unauthorized(res);
    }

    // Check that user exists
    const user = await UserModel.get(uid);
    if (user === undefined) {
      responses.badRequest(req, res);
      return;
    }
    const email = user.email;

    // TODO: Send mail to user with reset-link.

    responses.ok("Password-reset link send to recipient", res);
  } catch (err) {
    responses.unexpectedError(err, res);
  }
});

export default router;

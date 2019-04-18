import argon2 from "argon2";
import {Request, Response, Router} from "express";
import {body, validationResult} from "express-validator/check";
import UserModel from "../../../models/user";
import responses from "../../../responses";

const router = Router();

const inputValidator = [
  body("currentPassword").isString().isLength({min: 1}),
  body("newPassword").isString().isLength({min: 1}),
];

router.put("/", inputValidator, async (req: Request, res: Response) => {
  const errors = validationResult(req);
  if (!errors.isEmpty()) {
    return responses.badRequest(req, res);
  }

  try {

    const {currentPassword, newPassword} = req.body;

    // Check that user exists
    const username = req.session.uid;
    const user = await UserModel.get(username);
    if (user === undefined) {
      responses.badRequest(req, res);
      return;
    }

    // Verify that old password is correct
    const currentPasswordMatches = await argon2.verify(user.hash, currentPassword);
    if (!currentPasswordMatches) {
      return responses.unauthorized(res);
    }

    // Update with new password
    const newHash = await argon2.hash(newPassword);
    await UserModel.update(username, {hash: newHash});

    responses.ok("Password updated successfully", res);
  } catch (err) {
    responses.unexpectedError(err, res);
  }
});

export default router;

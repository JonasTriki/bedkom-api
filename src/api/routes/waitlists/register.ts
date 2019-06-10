import { NextFunction, Request, Response, Router } from "express";
import { body, validationResult } from "express-validator/check";
import { v4 } from "uuid";
import { isPermitted } from "../../../models/enums/UserRoles";
import PresentationModel from "../../../models/Presentation";
import UserModel from "../../../models/User";
import WaitlistModel from "../../../models/Waitlist";
import responses from "../../../responses";

const router = Router();

const inputValidator = [
  body("userId").isUUID(4),
  body("presentationId").isUUID(4)
];

router.post(
  "/",
  inputValidator,
  (req: Request, res: Response, next: NextFunction) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return responses.badRequest(req, res);
    }
    if (!isPermitted(req, "bedkom")) {
      return responses.badRequest(req, res);
    }
    next();
  }
);

router.post("/", async (req, res) => {
  try {
    const { userId, presentationId } = req.body;

    // Check that user already exists
    const user = await UserModel.get(userId);
    if (user === undefined) {
      responses.badRequest(req, res);
      return;
    }

    // Check that the presentation exists
    const presentation = await PresentationModel.get(presentationId);
    if (presentation === undefined) {
      responses.badRequest(req, res);
      return;
    }

    // Create the waitlist entry
    const waitlist = new WaitlistModel({
      id: v4(),
      userId,
      presentationId
    });
    await waitlist.save();

    responses.ok("Waitlist entry created", res);
  } catch (err) {
    responses.unexpectedError(err, res);
  }
});

export default router;

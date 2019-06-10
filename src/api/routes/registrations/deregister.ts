import dynamoose from "dynamoose";
import { NextFunction, Request, Response, Router } from "express";
import { body, validationResult } from "express-validator/check";
import { isPermitted } from "../../../models/enums/UserRoles";
import { Presentation } from "../../../models/Presentation";
import PresentationModel from "../../../models/Presentation";
import RegistrationModel, { Registration } from "../../../models/Registration";
import responses from "../../../responses";

const router = Router();

const inputValidator = [
  body("userId")
    .isUUID(4)
    .optional(),
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
    next();
  }
);

router.post("/", async (req, res) => {
  try {
    const { userId, presentationId } = req.body;

    // Bedkom-members should be able to deregister students
    let uid;
    if (userId && isPermitted(req, "bedkom")) {
      uid = userId;
    } else {
      uid = req.session.uid;
    }

    // Check that registration already exists
    const registrations = await RegistrationModel.scan({
      presentationId: { eq: presentationId },
      userId: { eq: uid }
    }).exec();
    if (registrations.length > 1) {
      return responses.unexpectedError(
        `Multiple registrations of same id (${uid}) in presentation (${presentationId})`,
        res
      );
    } else if (registrations.length === 0) {
      return responses.badRequest(req, res);
    }
    const registration = registrations[0];
    /*if (registration === undefined) {
      responses.badRequest(req, res);
      return;
    }*/

    // Commit a transaction for de-registering and incrementing the number of registrations
    await dynamoose.transaction<Registration | Presentation, string>([
      RegistrationModel.transaction.delete(registration.id),
      PresentationModel.transaction.update(registration.presentationId, {
        $ADD: { registrations: -1 }
      })
    ]);

    responses.ok("Registration de-registered", res);
  } catch (err) {
    responses.unexpectedError(err, res);
  }
});

export default router;

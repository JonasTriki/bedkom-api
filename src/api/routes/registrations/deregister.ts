import dynamoose from "dynamoose";
import {NextFunction, Request, Response, Router} from "express";
import {body, validationResult} from "express-validator/check";
import {isPermitted} from "../../../models/enums/UserRoles";
import {Presentation} from "../../../models/Presentation";
import PresentationModel from "../../../models/Presentation";
import RegistrationModel, {Registration} from "../../../models/Registration";
import responses from "../../../responses";

const router = Router();

router.post("/", body("id").isUUID(4), (req: Request, res: Response, next: NextFunction) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
        return responses.badRequest(req, res);
    }
    if (!isPermitted(req, "bedkom")) {
        return responses.badRequest(req, res);
    }
    next();
});

router.post("/", async (req, res) => {
    try {
        const {id} = req.body;

        // Check that registration already exists
        const registration = await RegistrationModel.get(id);
        if (registration === undefined) {
            responses.badRequest(req, res);
            return;
        }

        // Commit a transaction for de-registering and incrementing the number of registrations
        await dynamoose.transaction<Registration | Presentation, string>([
            RegistrationModel.transaction.delete(id),
            PresentationModel.transaction.update(registration.presentationId, {$ADD: {registrations: -1}}),
        ]);

        responses.ok("Registration de-registered", res);
    } catch (err) {
        responses.unexpectedError(err, res);
    }
});

export default router;

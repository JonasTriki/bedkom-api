import dynamoose from "dynamoose";
import {NextFunction, Request, Response, Router} from "express";
import {body, validationResult} from "express-validator/check";
import {v4} from "uuid";
import {isPermitted} from "../../../models/enums/UserRoles";
import PresentationModel, {Presentation} from "../../../models/Presentation";
import RegistrationModel, {Registration} from "../../../models/Registration";
import UserModel from "../../../models/User";
import responses from "../../../responses";

const router = Router();

const inputValidator = [
    body("userId").isUUID(4).optional(),
    body("presentationId").isUUID(4),
];

router.post("/", inputValidator, (req: Request, res: Response, next: NextFunction) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
        return responses.badRequest(req, res);
    }
    next();
});

router.post("/", async (req, res) => {
    try {
        const {
            userId,
            presentationId
        } = req.body;

        // Bedkom-members should be able to register students
        let uid;
        if (userId && isPermitted(req, "bedkom")) {
          uid = userId;
        } else {
          uid = req.session.uid;
        }

        // Check that user already exists
        const user = await UserModel.get(uid);
        if (user === undefined) {
            return responses.badRequest(req, res);
        }

        // Check that the presentation exists
        const presentation = await PresentationModel.get(presentationId);
        if (presentation === undefined) {
            return responses.badRequest(req, res);
        }

        // Check if the presentation is full
        if (presentation.registrations + 1 > presentation.capacity) {
            return responses.badRequest(req, res);
        }

        // TODO: Make sure register before the startTime of the presentation

        // Create the registration
        const payload: Registration = {
            id: v4(),
            userId,
            presentationId,
        };

        // Commit a transaction for registering and incrementing the number of registrations
        await dynamoose.transaction<Registration | Presentation, string>([
            RegistrationModel.transaction.create(payload),
            PresentationModel.transaction.update(presentation.id, {$ADD: {registrations: 1}}),
        ]);

        responses.ok("Registration created", res);
    } catch (err) {
        responses.unexpectedError(err, res);
    }
});

export default router;

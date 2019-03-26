import {NextFunction, Request, Response, Router} from "express";
import {body, validationResult} from "express-validator/check";
import {isPermitted} from "../../../models/enums/UserRoles";
import WaitlistModel, {Waitlist} from "../../../models/Waitlist";
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
        // Check that waitlist entry already exists
        const waitlistEntry = await WaitlistModel.get(req.body.id);
        if (waitlistEntry === undefined) {
            responses.badRequest(req, res);
            return;
        }

        // De-register
        await waitlistEntry.delete();

        responses.ok("Waitlist entry de-registered", res);
    } catch (err) {
        responses.unexpectedError(err, res);
    }
});

export default router;

import {NextFunction, Request, Response, Router} from "express";
import {body, validationResult} from "express-validator/check";
import {isPermitted} from "../../../models/enums/UserRoles";
import PresentationModel from "../../../models/Presentation";
import responses from "../../../responses";

const router = Router();

router.delete("/", body("id").isUUID(4), (req: Request, res: Response, next: NextFunction) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
        return responses.badRequest(req, res);
    }
    if (!isPermitted(req, "bedkom")) {
        return responses.badRequest(req, res);
    }
    next();
});

router.delete("/", async (req, res) => {

    // Check if presentation exists
    const presentation = await PresentationModel.get(req.body.id);
    if (presentation === undefined) {
        return responses.badRequest(req, res);
    }

    // TODO: Send mail to participants telling them that the presentation has been cancelled.

    await presentation.delete();
    responses.ok("Presentation deleted", res);
});

export default router;

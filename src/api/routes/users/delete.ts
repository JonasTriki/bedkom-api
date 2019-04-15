import {NextFunction, Request, Response, Router} from "express";
import {validationResult} from "express-validator/check";
import LastAuthorizedModel from "../../../models/LastAuthorized";
import UserModel from "../../../models/user";
import responses from "../../../responses";
import {vUsername} from "../../../validators";

const router = Router();

const inputValidator = [
    vUsername,
];

router.delete("/", inputValidator, async (req: Request, res: Response, next: NextFunction) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
        return responses.badRequest(req, res);
    }

    // TODO: Do we need to compare the session uid and body username?
    if (req.session.uid !== req.body.username) {
        return responses.badRequest(req, res);
    }
    next();
});

router.delete("/", inputValidator, async (req: Request, res: Response) => {
    const {username} = req.body;

    // Check that user already exists
    const user = await UserModel.get(username);
    const lastAuth = await LastAuthorizedModel.get(username);
    if (user === undefined || lastAuth === undefined) {
        responses.badRequest(req, res);
        return;
    }

    // Delete user and last authorized instance.
    await Promise.all([
        lastAuth.delete(),
        user.delete()
    ]);

    responses.ok("User deleted", res);
});

export default router;

import { NextFunction, Request, Response, Router } from "express";
import { body, validationResult } from "express-validator/check";
import LastAuthorizedModel from "../../../models/LastAuthorized";
import UserModel from "../../../models/user";
import responses from "../../../responses";
import { JWTRequest } from "../../middlewares/jwt";

const router = Router();

const inputValidator = [
    body("username").isString().custom((value) => value.length === 6)
];

router.delete("/", inputValidator, async (req: JWTRequest, res: Response, next: NextFunction) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
        return responses.badRequest(req, res);
    }
    if (req.jwt.id !== req.body.username) {
        return responses.badRequest(req, res);
    }
    next();
});

router.delete("/", inputValidator, async (req: JWTRequest, res: Response) => {
    const { username } = req.body;

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

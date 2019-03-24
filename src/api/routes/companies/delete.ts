import {NextFunction, Request, Response, Router} from "express";
import {body, validationResult} from "express-validator/check";
import CompanyModel from "../../../models/Company";
import {isPermitted} from "../../../models/enums/UserRoles";
import responses from "../../../responses";

const router = Router();

const inputValidator = [
    body("id").isUUID(4),
];

router.delete("/", inputValidator, async (req: Request, res: Response, next: NextFunction) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
        return responses.badRequest(req, res);
    }
    if (!isPermitted(req, "bedkom")) {
        return responses.badRequest(req, res);
    }
    next();
});

router.delete("/", inputValidator, async (req: Request, res: Response) => {
    const {id} = req.body;

    // Check that company already exists
    const company = await CompanyModel.get(id);
    if (company === undefined) {
        responses.badRequest(req, res);
        return;
    }

    // TODO: Either delete it without hesetation, or only delete if not in use.
    responses.forbidden("Not yet implemented", res);
});

export default router;

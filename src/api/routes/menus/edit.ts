import {NextFunction, Request, Response, Router} from "express";
import {body, validationResult} from "express-validator/check";
import {isPermitted} from "../../../models/enums/UserRoles";
import MenuModel from "../../../models/Menu";
import responses from "../../../responses";
import {vFoodEntries} from "../../../validators";

const router = Router();

const inputValidator = [
    body("id").isUUID(4),
    body("name").isLength({min: 2}),
    vFoodEntries,
    body("url").isURL().optional()
];

router.put("/", inputValidator, (req: Request, res: Response, next: NextFunction) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
        return responses.badRequest(req, res);
    }
    if (!isPermitted(req, "bedkom")) {
        return responses.badRequest(req, res);
    }
    next();
});

router.put("/", async (req: Request, res: Response) => {
    try {
        const {id, name, foodEntries, url} = req.body;

        // Make sure that menu exists
        const menu = await MenuModel.get(id);
        if (menu === undefined) {
            responses.badRequest(req, res);
            return;
        }

        // Edit menu with new details
        await menu.put({
            id,
            name,
            foodEntries,
            url
        });

        responses.ok("Menu edited", res);
    } catch (err) {
        responses.unexpectedError(err, res);
    }
});

export default router;

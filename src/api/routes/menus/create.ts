import {NextFunction, Request, Response, Router} from "express";
import {body, validationResult} from "express-validator/check";
import {v4} from "uuid";
import {isPermitted} from "../../../models/enums/UserRoles";
import MenuModel from "../../../models/Menu";
import responses from "../../../responses";
import {vFoodEntries} from "../../../validators";

const router = Router();

const inputValidator = [
    body("name").isLength({min: 2}),
    vFoodEntries,
    body("url").isURL().optional()
];

router.post("/", inputValidator, (req: Request, res: Response, next: NextFunction) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
        return responses.badRequest(req, res);
    }
    if (!isPermitted(req, "bedkom")) {
        return responses.badRequest(req, res);
    }
    next();
});

router.post("/", async (req: Request, res: Response) => {
    try {
        const {name, foodEntries, url} = req.body;

        // Create menu model before saving
        const menuModel = new MenuModel({
            id: v4(),
            name,
            foodEntries,
            url
        });
        await menuModel.save();

        responses.ok("Menu created", res);
    } catch (err) {
        responses.unexpectedError(err, res);
    }
});

export default router;

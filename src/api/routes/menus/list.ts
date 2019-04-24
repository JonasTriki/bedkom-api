import {NextFunction, Request, Response, Router} from "express";
import {isPermitted} from "../../../models/enums/UserRoles";
import MenuModel from "../../../models/Menu";
import responses from "../../../responses";

const router = Router();

router.get("/", async (req: Request, res: Response, next: NextFunction) => {
  if (!isPermitted(req, "bedkom")) {
    return responses.badRequest(req, res);
  }
  next();
});

router.get("/", async (req: Request, res: Response) => {
    try {

        // List out the menus
        const menus = await MenuModel.scan().exec();

        // Return with all menus
        responses.ok(menus, res);
    } catch (err) {
        responses.unexpectedError(req, res);
    }
});

export default router;

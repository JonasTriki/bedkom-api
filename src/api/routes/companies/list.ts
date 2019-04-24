import {NextFunction, Request, Response, Router} from "express";
import CompanyModel from "../../../models/Company";
import {isPermitted} from "../../../models/enums/UserRoles";
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

        // List out the companies
        const companies = await CompanyModel.scan().exec();

        // Return with all companies
        responses.ok(companies, res);
    } catch (err) {
        responses.unexpectedError(req, res);
    }
});

export default router;

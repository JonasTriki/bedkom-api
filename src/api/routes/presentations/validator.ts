import {NextFunction, Request, Response} from "express";
import CompanyModel from "../../../models/Company";
import MenuModel from "../../../models/Menu";
import UserModel from "../../../models/User";
import responses from "../../../responses";

const presentationsBodyValidator = async (req: Request, res: Response, next: NextFunction) => {
    try {
        const {
            companyId,
            startTime,
            endTime,
            responsible,
            menuId,
        } = req.body;

        // Make sure the endtime of the presentation is before the starttime.
        if (endTime < startTime) {
            return responses.badRequest(req, res);
        }

        // Check that company already exists
        const company = await CompanyModel.get(companyId);
        if (company === undefined) {
            return responses.badRequest(req, res);
        }

        // Check that the responsible students exists
        const students = responsible.map(async (uid: string) => await UserModel.get(uid));
        for await (const student of students) {
            if (student === undefined) {
                return responses.badRequest(req, res);
            }
        }

        // Check that menu exists if given
        if (menuId) {
            const menu = await MenuModel.get(menuId);
            if (menu === undefined) {
                return responses.badRequest(req, res);
            }
        }

        // Body is valid.
        next();
    } catch (err) {
        responses.unexpectedError(err, res);
    }
};

export default presentationsBodyValidator;

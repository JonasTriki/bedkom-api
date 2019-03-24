import {NextFunction, Request, Response, Router} from "express";
import {body, validationResult} from "express-validator/check";
import {v4} from "uuid";
import {uploadToS3} from "../../../aws";
import CompanyModel from "../../../models/Company";
import {isPermitted} from "../../../models/enums/UserRoles";
import MenuModel from "../../../models/Menu";
import PresentationModel, {Presentation} from "../../../models/Presentation";
import UserModel from "../../../models/User";
import responses from "../../../responses";
import {getSemesterYear} from "../../../utils/dateTime";
import {vCapacity} from "../../../validators";
import upload from "../../middlewares/multer";

const router = Router();

const inputValidator = [
    body("companyId").isUUID(4),
    vCapacity,
    body("minStudyYear").isNumeric().custom((num) => num >= -1),
    body("startTime").isNumeric(),
    body("endTime").isNumeric(),
    body("responsible").isArray(),
    body("contract"),
    body("menuId").isUUID(4).optional(),
    body("description").isString(),
];

router.post("/", (req: Request, res: Response, next: NextFunction) => {

    // Capture banner-image using multer
    upload.single("contract")(req, res, (err) => {
        if (err) {
            responses.unexpectedError(err, res);
            return;
        }
        next();
    });
});

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

router.post("/", async (req, res) => {
    try {
        const {
            companyId,
            capacity,
            minStudyYear,
            startTime,
            endTime,
            responsible,
            menuId,
            description,
        } = req.body;
        const contract = req.file;

        // Make sure the endtime of the presentation is before the starttime.
        if (endTime < startTime) {
            return responses.badRequest(req, res);
        }
        const sy = getSemesterYear(startTime);

        // Check that company already exists
        const company = await CompanyModel.get(companyId);
        if (company === undefined) {
            responses.badRequest(req, res);
            return;
        }

        // Check that the responsible students exists
        const students = responsible.map(async (uid: string) => await UserModel.get(uid));
        for await (const student of students) {
            if (student === undefined) {
                responses.badRequest(req, res);
                return;
            }
        }

        // Construct payload
        const presentationId = v4();
        const payload: Presentation = {
            id: presentationId,
            companyId,
            registrations: 0,
            capacity: +capacity,
            minStudyYear: +minStudyYear,
            year: sy.year,
            semester: sy.semester,
            startTime: +startTime,
            endTime: +endTime,
            responsible,
            description,
        };

        // Check that menu exists if given
        if (menuId) {
            const menu = await MenuModel.get(menuId);
            if (menu === undefined) {
                responses.badRequest(req, res);
                return;
            }
            payload.menuId = menuId;
        }

        // Upload contract if given
        if (contract) {
            payload.contractUrl = await uploadToS3(contract, "contracts/" + presentationId);
        }

        // Everything is good! Save the presentation
        const presentationModel = new PresentationModel(payload);
        await presentationModel.save();

        responses.ok("Presentation created", res);
    } catch (err) {
        responses.unexpectedError(err, res);
    }
});

export default router;

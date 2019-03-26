import {NextFunction, Request, Response, Router} from "express";
import {validationResult} from "express-validator/check";
import {v4} from "uuid";
import {uploadToS3} from "../../../aws";
import CompanyModel from "../../../models/Company";
import {isPermitted} from "../../../models/enums/UserRoles";
import MenuModel from "../../../models/Menu";
import PresentationModel, {Presentation} from "../../../models/Presentation";
import UserModel from "../../../models/User";
import responses from "../../../responses";
import {getSemesterYear} from "../../../utils/dateTime";
import {vPresentations} from "../../../validators";
import upload from "../../middlewares/multer";
import presentationsBodyValidator from "./validator";

const router = Router();

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

router.post("/", vPresentations, (req: Request, res: Response, next: NextFunction) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
        return responses.badRequest(req, res);
    }
    if (!isPermitted(req, "bedkom")) {
        return responses.badRequest(req, res);
    }
    next();
});

router.post("/", presentationsBodyValidator, async (req, res) => {
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
        const sy = getSemesterYear(startTime);

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

        // Add menu if given
        if (menuId) {
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

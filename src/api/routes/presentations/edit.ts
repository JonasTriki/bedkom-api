import {NextFunction, Request, Response, Router} from "express";
import {body, validationResult} from "express-validator/check";
import {uploadToS3} from "../../../aws";
import {isPermitted} from "../../../models/enums/UserRoles";
import PresentationModel from "../../../models/Presentation";
import {Presentation} from "../../../models/Presentation";
import responses from "../../../responses";
import {getSemesterYear} from "../../../utils/dateTime";
import {vPresentations} from "../../../validators";
import upload from "../../middlewares/multer";
import presentationsBodyValidator from "./validator";

const router = Router();

router.put("/", (req: Request, res: Response, next: NextFunction) => {

    // Capture banner-image using multer
    upload.single("contract")(req, res, (err) => {
        if (err) {
            responses.unexpectedError(err, res);
            return;
        }
        next();
    });
});

// Here we have the same body as "create" endpoint, but we add ID.
router.put("/", [
    body("id").isUUID(4),
    ...vPresentations,
], (req: Request, res: Response, next: NextFunction) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
        return responses.badRequest(req, res);
    }
    if (!isPermitted(req, "bedkom")) {
        return responses.badRequest(req, res);
    }
    next();
});

router.put("/", presentationsBodyValidator, async (req, res) => {
    try {
        const {
            id,
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

        // Check if presentation exists
        const presentation = await PresentationModel.get(id);
        if (presentation === undefined) {
            return responses.badRequest(req, res);
        }

        // Construct payload
        const payload: any = {
            id,
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

        // TODO: If start- and/or endtime changes, send email to participants?

        // Add menu if given
        if (menuId) {
            payload.menuId = menuId;
        }

        // Upload contract if given
        if (contract) {
            payload.contractUrl = await uploadToS3(contract, "contracts/" + id);
        }

        // Everything is good! Save the presentation
        await PresentationModel.update(id, payload);
        responses.ok("Presentation edited", res);
    } catch (err) {
        responses.unexpectedError(err, res);
    }
});

export default router;

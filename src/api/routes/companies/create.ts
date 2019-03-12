import {NextFunction, Request, Response, Router} from "express";
import {body, validationResult} from "express-validator/check";
import {v4} from "uuid";
import {uploadToS3} from "../../../aws";
import CompanyModel from "../../../models/Company";
import {isPermitted} from "../../../models/enums/UserRoles";
import responses from "../../../responses";
import upload from "../../middlewares/multer";

const router = Router();

const inputValidator = [
    body("name").isLength({min: 2}),
    body("description").isString(),
    body("website").isURL(),
    body("bannerImg")
];

router.post("/", (req: Request, res: Response, next: NextFunction) => {

    // Capture banner-image using multer
    upload.single("bannerImg")(req, res, (err) => {
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

router.post("/", async (req: Request, res: Response) => {

    const {name, description, website} = req.body;
    const bannerFile = req.file;

    try {

        // Upload banner to S3 and recieve the download url
        const bannerImgUrl = await uploadToS3(bannerFile);

        // Create the company
        const companyModel = new CompanyModel({
            id: v4(),
            name,
            description,
            website,
            bannerImgUrl,
        });

        // .. and save it.
        await companyModel.save();

        responses.ok("Company created", res);
    } catch (err) {
        responses.unexpectedError(err, res);
    }
});

export default router;

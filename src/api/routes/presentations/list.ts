import {Request, Response, Router} from "express";
import PresentationModel from "../../../models/Presentation";
import responses from "../../../responses";

const router = Router();

router.get("/", async (req: Request, res: Response) => {
    try {

        // List out the presentations
        const presentations = await PresentationModel.scan().exec();

        // Return with all presentations
        responses.ok(presentations, res);
    } catch (err) {
        responses.unexpectedError(req, res);
    }
});

export default router;

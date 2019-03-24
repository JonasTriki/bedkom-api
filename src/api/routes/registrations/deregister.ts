import { Router } from "express";
import responses from "../../../responses";
const router = Router();

router.post("/", (req, res) => {

    // TODO: Implement
    responses.forbidden("Not implemented yet", res);
});

export default router;

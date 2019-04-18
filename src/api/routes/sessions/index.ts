import {Router} from "express";
import get from "./get";

const router = Router();

router.use("/get", get);

export default router;

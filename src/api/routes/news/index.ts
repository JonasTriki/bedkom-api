import {Router} from "express";
import list from "./list";

const router = Router();

router.use("/list", list);

export default router;

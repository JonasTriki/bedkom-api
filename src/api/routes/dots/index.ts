import { Router } from "express";
import verifySession from "../../middlewares/session";
import add from "./add";
const router = Router();

router.use(verifySession);
router.use("/add", add);

export default router;

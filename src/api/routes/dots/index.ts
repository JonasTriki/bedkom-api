import { Router } from "express";
import verifyJWT from "../../middlewares/jwt";
import add from "./add";
const router = Router();

router.use(verifyJWT);
router.use("/add", add);

export default router;

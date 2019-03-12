import { Router } from "express";
import verifyJWTToken from "../../middlewares/jwt";
import add from "./add";
const router = Router();

router.use(verifyJWTToken);
router.use("/add", add);

export default router;

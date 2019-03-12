import { Router } from "express";
import verifyJWTToken from "../../middlewares/jwt";
import create from "./create";
const router = Router();

router.use(verifyJWTToken);
router.use("/create", create);

export default router;

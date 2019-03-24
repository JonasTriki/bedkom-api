import { Router } from "express";
import verifyJWTToken from "../../middlewares/jwt";
import register from "./register";
import unregister from "./unregister";
const router = Router();

router.use(verifyJWTToken);
router.use("/register", register);
router.use("/unregister", unregister);

export default router;

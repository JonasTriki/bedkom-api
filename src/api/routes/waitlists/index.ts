import { Router } from "express";
import verifyJWTToken from "../../middlewares/jwt";
import deregister from "./deregister";
import register from "./register";
const router = Router();

router.use(verifyJWTToken);
router.use("/register", register);
router.use("/deregister", deregister);

export default router;

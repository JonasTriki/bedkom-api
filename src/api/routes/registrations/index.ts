import { Router } from "express";
import verifyJWT from "../../middlewares/jwt";
import deregister from "./deregister";
import register from "./register";
const router = Router();

router.use(verifyJWT);
router.use("/register", register);
router.use("/deregister", deregister);

export default router;

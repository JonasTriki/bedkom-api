import { Router } from "express";
import verifySession from "../../middlewares/session";
import deregister from "./deregister";
import register from "./register";
const router = Router();

router.use(verifySession);
router.use("/register", register);
router.use("/deregister", deregister);

export default router;

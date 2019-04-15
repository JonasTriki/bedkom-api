import { Router } from "express";
import verifySession from "../../middlewares/session";
import _delete from "./delete";
import login from "./login";
import setup from "./setup";
import verify from "./verify";
const router = Router();

router.use("/login", login);
router.use("/setup", setup);
router.use("/verify", verify);

router.use(verifySession);
router.use("/delete", _delete);

export default router;

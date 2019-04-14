import { Router } from "express";
import verifyJWT from "../../middlewares/jwt";
import _delete from "./delete";
import login from "./login";
import verify from "./verify";
const router = Router();

router.use("/login", login);
router.use("/verify", verify);

router.use(verifyJWT);
router.use("/delete", _delete);

export default router;

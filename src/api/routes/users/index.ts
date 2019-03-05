import { Router } from "express";
import verifyJWTToken from "../../middlewares/jwt";
import _delete from "./delete";
import login from "./login";
import verify from "./verify";
const router = Router();

router.use("/login", login);
router.use("/verify", verify);

router.use(verifyJWTToken);
router.use("/delete", _delete);

export default router;

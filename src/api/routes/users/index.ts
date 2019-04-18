import {Router} from "express";
import verifySession from "../../middlewares/session";
import bedkom from "./bedkom";
import changePassword from "./changePassword";
import _delete from "./delete";
import edit from "./edit";
import login from "./login";
import logout from "./logout";
import resetPassword from "./resetPassword";
import setup from "./setup";
import verify from "./verify";

const router = Router();

router.use("/login", login);
router.use("/setup", setup);
router.use("/verify", verify);
router.use("/bedkom", bedkom);

router.use(verifySession);
router.use("/edit", edit);
router.use("/change-password", changePassword);
router.use("/reset-password", resetPassword);
router.use("/logout", logout);
router.use("/delete", _delete);

export default router;

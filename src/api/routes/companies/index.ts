import { Router } from "express";
import verifySession from "../../middlewares/session";
import contact from "./contact";
import create from "./create";
import _delete from "./delete";
import edit from "./edit";
const router = Router();

router.use(verifySession);
router.use("/contact", contact);
router.use("/create", create);
router.use("/edit", edit);
router.use("/delete", _delete);

export default router;

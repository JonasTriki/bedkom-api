import { Router } from "express";
import verifySession from "../../middlewares/session";
import create from "./create";
import _delete from "./delete";
import edit from "./edit";
import list from "./list";
const router = Router();

router.use(verifySession);
router.use("/create", create);
router.use("/edit", edit);
router.use("/delete", _delete);
router.use("/list", list);

export default router;

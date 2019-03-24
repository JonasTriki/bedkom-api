import { Router } from "express";
import verifyJWTToken from "../../middlewares/jwt";
import create from "./create";
import _delete from "./delete";
import edit from "./edit";
const router = Router();

router.use(verifyJWTToken);
router.use("/create", create);
router.use("/edit", edit);
router.use("/delete", _delete);

export default router;

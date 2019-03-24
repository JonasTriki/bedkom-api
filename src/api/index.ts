import { Router } from "express";
import companies from "./routes/companies";
import dots from "./routes/dots";
import menus from "./routes/menus";
import users from "./routes/users";
const router = Router();

router.use("/users", users);
router.use("/dots", dots);
router.use("/companies", companies);
router.use("/menus", menus);

export default router;

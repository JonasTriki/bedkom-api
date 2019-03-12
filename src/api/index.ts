import { Router } from "express";
import companies from "./routes/companies";
import dots from "./routes/dots";
import users from "./routes/users";
const router = Router();

router.use("/users", users);
router.use("/dots", dots);
router.use("/companies", companies);

export default router;

import { Router } from "express";
const router = Router();

import companies from "./routes/companies";
import dots from "./routes/dots";
import menus from "./routes/menus";
import presentations from "./routes/presentations";
import registrations from "./routes/registrations";
import users from "./routes/users";
import waitlists from "./routes/waitlists";

router.use("/users", users);
router.use("/dots", dots);
router.use("/companies", companies);
router.use("/menus", menus);
router.use("/presentations", presentations);
router.use("/registrations", registrations);
router.use("/waitlists", waitlists);

export default router;

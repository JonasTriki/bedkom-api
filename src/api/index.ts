import {Router} from "express";
import about from "./routes/about";
import companies from "./routes/companies";
import dots from "./routes/dots";
import menus from "./routes/menus";
import news from "./routes/news";
import presentations from "./routes/presentations";
import registrations from "./routes/registrations";
import sessions from "./routes/sessions";
import users from "./routes/users";
import waitlists from "./routes/waitlists";

const router = Router();

router.use("/users", users);
router.use("/sessions", sessions);
router.use("/dots", dots);
router.use("/companies", companies);
router.use("/menus", menus);
router.use("/presentations", presentations);
router.use("/registrations", registrations);
router.use("/waitlists", waitlists);
router.use("/about", about);
router.use("/news", news);

export default router;

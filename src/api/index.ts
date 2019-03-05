import { Router } from "express";
import users from "./routes/users";
const router = Router();

router.use("/users", users);

export default router;

import {NextFunction, Request, Response, Router} from "express";
import {body, validationResult} from "express-validator/check";
import responses from "../../../responses";

const router = Router();

const inputValidator = [];

router.post("/"/*, inputValidator*/, (req: Request, res: Response, next: NextFunction) => {
  const errors = validationResult(req);
  if (!errors.isEmpty()) {
    return responses.badRequest(req, res);
  }
  next();
});

router.post("/", async (req, res) => {

  // TODO: Implement mail sending
  responses.ok("Mail sent to recipient!", res);
});

export default router;

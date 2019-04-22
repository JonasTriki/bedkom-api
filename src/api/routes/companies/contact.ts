import {NextFunction, Request, Response, Router} from "express";
import {body, validationResult} from "express-validator/check";
import responses from "../../../responses";

const router = Router();

const inputValidator = [
  body("company").isString().isLength({min: 1}),
  body("name").isString().isLength({min: 1}),
  body("email").isEmail(),
  body("message").isString().isLength({min: 1}),
];

router.post("/", inputValidator, (req: Request, res: Response, next: NextFunction) => {
  const errors = validationResult(req);
  if (!errors.isEmpty()) {
    return responses.badRequest(req, res);
  }
  next();
});

router.post("/", async (req, res) => {
  try {
    const {company, name, email, message} = req.body;

    // TODO: Implement mail sending
    responses.ok("Mail sent to recipient!", res);
  } catch (err) {
    responses.unexpectedError(err, res);
  }
});

export default router;

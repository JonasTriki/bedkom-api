import {NextFunction, Request, Response, Router} from "express";
import {body, validationResult} from "express-validator/check";
import {isPermitted} from "../../../models/enums/UserRoles";
import UserModel from "../../../models/User";
import responses from "../../../responses";
// import {csrfProtection} from "../../middlewares/csrftoken";

const router = Router();

const inputValidator = [
  body("id").isUUID(4).optional(),
  body("firstName").isLength({min: 2}),
  body("lastName").isLength({min: 2}),
  body("email").isEmail(),

  // TODO: Add more admin-only fields
];

router.put("/"/*, csrfProtection*/, inputValidator, (req: Request, res: Response, next: NextFunction) => {
  const errors = validationResult(req);
  if (!errors.isEmpty()) {
    return responses.badRequest(req, res);
  }
  next();
});

router.put("/", async (req: Request, res: Response) => {
  try {
    const {id, firstName, lastName, email} = req.body;

    // Check if we use id or session uid.
    let uid;
    if (isPermitted(req, "bedkom") && id) {
      uid = id;
    } else {
      uid = req.session.uid;
    }

    // Make sure that user exists
    const oldUser = await UserModel.get(uid);
    if (oldUser === undefined) {
      responses.badRequest(req, res);
      return;
    }

    // Update user with new details
    const userHashed = await UserModel.update(uid, {firstName, lastName, email});
    const {hash, ...user} = userHashed;

    responses.ok({user}, res);
  } catch (err) {
    responses.unexpectedError(err, res);
  }
});

export default router;

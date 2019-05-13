import {Request, Response, Router} from "express";
import RegistrationModel from "../../../models/Registration";
import UserModel from "../../../models/user";
import responses from "../../../responses";

const router = Router();

router.get("/", async (req: Request, res: Response) => {

  try {
    const token = req.csrfToken();

    // Check that user exists
    const username = req.session.uid;
    if (username === undefined) {

      // User not authenticated
      return responses.csrfToken(req, res, token);
    }
    const hashedUser = await UserModel.get(username);
    if (hashedUser === undefined) {
      return responses.csrfToken(req, res, token);
    }

    // Fetch all registrations
    const registrations = await RegistrationModel.scan({userId: {eq: username}}).exec();
    const presentations = registrations.map((reg) => reg.presentationId);

    // Responding with CSRF-token and user info
    responses.csrfToken(req, res, token, hashedUser, presentations);
  } catch (err) {
    responses.unexpectedError(err, res);
  }
});

export default router;

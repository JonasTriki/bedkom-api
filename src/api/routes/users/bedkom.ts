import {Request, Response, Router} from "express";
import UserModel from "../../../models/user";
import responses from "../../../responses";

const router = Router();

router.get("/", async (req: Request, res: Response) => {

  try {

    // List out the users
    const bedkomMembers = await UserModel.scan({role: {contains: "bedkom"}}).exec();

    // Only include relevant fields
    const publicList = bedkomMembers.map((user) => ({
      imgUrl: user.imgUrl,
      firstName: user.firstName,
      lastName: user.lastName,
      position: user.committeePosition,
    }));

    // Responding with public list
    responses.ok(publicList, res);
  } catch (err) {
    responses.unexpectedError(err, res);
  }
});

export default router;

import { Request, Response, Router } from "express";
import responses from "../../../responses";

const router = Router();

router.post("/", async (req: Request, res: Response) => {
  try {
    req.session.destroy(err => {
      if (err) {
        return responses.unexpectedError(err, res);
      }
      responses.ok({}, res);
    });
  } catch (err) {
    responses.unexpectedError(err, res);
  }
});

export default router;

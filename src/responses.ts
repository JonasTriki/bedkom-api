import {Request, Response} from "express";
import {UserHashed} from "./models/User";

const unexpectedError = (err: any, res: Response) => {
  console.log(err);
  res.status(500).send("Unexpected error occurred");
};

const forbidden = (data: any, res: Response) => {
  res.status(403).send({message: "forbidden", data});
};

const unauthorized = (res: Response) => {
  res.status(401).send("Unauthorized");
};

const badRequest = (req: Request, res: Response) => {
  console.log("Bad request body?: ", req.body);
  res.status(400).send("Bad request");
};

const accepted = (data: any, sendMessage: string, res: Response) => {
  res.status(202).send({message: sendMessage, data});
};

const message = (sendMessage: any, data: any, res: Response) => {
  res.send({message: sendMessage, data});
};

const ok = (data: any, res: Response) => {
  message("ok", data, res);
};

const csrfToken = (req: Request, res: Response, token: string, userHashed?: UserHashed, presentations?: string[]) => {
  if (userHashed) {
    const {hash, ...user} = userHashed;
    req.session.uid = user.id;
    req.session.role = user.role;
    ok({user: {...user, presentations}, csrfToken: token}, res);
  } else {
    ok({csrfToken: token}, res);
  }
};

const session = (req: Request, res: Response, userHashed?: UserHashed) => {
  const {hash, ...user} = userHashed;
  req.session.uid = user.id;
  req.session.role = user.role;
  ok({user}, res);
};

export default {
  unexpectedError,
  unauthorized,
  forbidden,
  badRequest,
  accepted,
  message,
  ok,
  csrfToken,
  session,
};

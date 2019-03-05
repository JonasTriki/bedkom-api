import { Request, Response } from "express";
import jsonwebtoken from "jsonwebtoken";
import config from "./config";
import { UserHashed } from "./models/User";

const unexpectedError = (err: any, res: Response) => {
    console.log(err);
    res.status(500).send("Unexpected error occurred");
};

const forbidden = (data: any, res: Response) => {
    res.status(403).send({ status: "forbidden", data });
};

const unauthorized = (res: Response) => {
    res.status(401).send("Unauthorized");
};

const badRequest = (req: Request, res: Response) => {
    console.log("Bad request body?: ", req.body);
    res.status(400).send("Bad request");
};

const accepted = (data: any, sendStatus: string, res: Response) => {
    res.status(202).send({ status: sendStatus, data });
};

const status = (sendStatus: any, data: any, res: Response) => {
    res.send({ status: sendStatus, data });
};

const ok = (data: any, res: Response) => {
    status("ok", data, res);
};

const jwt = (user: UserHashed, res: Response) => {
    const token = jsonwebtoken.sign({ id: user.id, role: user.role }, config.jwtSecret);
    ok({ token }, res);
};

export default {
    unexpectedError,
    unauthorized,
    forbidden,
    badRequest,
    accepted,
    status,
    ok,
    jwt,
};

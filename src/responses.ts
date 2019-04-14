import { Request, Response } from "express";
import jsonwebtoken from "jsonwebtoken";
import config from "./config";
import { UserHashed } from "./models/User";

const unexpectedError = (err: any, res: Response) => {
    console.log(err);
    res.status(500).send("Unexpected error occurred");
};

const forbidden = (data: any, res: Response) => {
    res.status(403).send({ message: "forbidden", data });
};

const unauthorized = (res: Response) => {
    res.status(401).send("Unauthorized");
};

const badRequest = (req: Request, res: Response) => {
    console.log("Bad request body?: ", req.body);
    res.status(400).send("Bad request");
};

const accepted = (data: any, sendMessage: string, res: Response) => {
    res.status(202).send({ message: sendMessage, data });
};

const message = (sendMessage: any, data: any, res: Response) => {
    res.send({ message: sendMessage, data });
};

const ok = (data: any, res: Response) => {
    message("ok", data, res);
};

const jwt = (userHashed: UserHashed, res: Response) => {
    const token = jsonwebtoken.sign({ id: userHashed.id, role: userHashed.role }, config.jwtSecret);
    const {hash, ...user} = userHashed;
    ok({ token, user }, res);
};

export default {
    unexpectedError,
    unauthorized,
    forbidden,
    badRequest,
    accepted,
    message,
    ok,
    jwt,
};

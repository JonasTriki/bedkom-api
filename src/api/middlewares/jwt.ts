import { NextFunction, Request, Response } from "express";
import jwt from "jsonwebtoken";

import config from "../../config";
import responses from "../../responses";

export interface JWTRequest extends Request {
    jwt: JWTPayload;
}

const verifyJWTToken = (req: JWTRequest, res: Response, next: NextFunction) => {
    if (!req.headers.authorization) {
        responses.unauthorized(res);
        return;
    }
    const authHeader = req.headers.authorization.split(" ");
    if (authHeader.length !== 2) {
        responses.unauthorized(res);
        return;
    }
    const token = authHeader[1];
    jwt.verify(token, config.jwtSecret, (err, decodedToken) => {
        if (err || !decodedToken) {
            responses.unauthorized(res);
            return;
        }
        req.jwt = decodedToken as JWTPayload;
        next();
    });
};

export default verifyJWTToken;

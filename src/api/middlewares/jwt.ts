import { NextFunction, Request, Response } from "express";
import jwt from "jsonwebtoken";

import config from "../../config";
import responses from "../../responses";

declare global {
    namespace Express {
        interface Request {
            jwt: JWTPayload;
        }
    }
}

const verifyJWTToken = (req: Request, res: Response, next: NextFunction) => {
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

    // Check if we bypass JWT verification.
    if ((process.env.NODE_ENV === "dev" || process.env.NODE_ENV === "local") && token === config.devMasterToken) {
        req.jwt = config.devMasterJWTPayload as JWTPayload;
        next();
        return;
    }
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

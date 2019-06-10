import { NextFunction, Request, Response } from "express";
import config from "../../config";
import responses from "../../responses";

declare global {
  namespace Express {
    interface Session {
      uid: string;
      role: string;
    }
  }
}

const verifySession = (req: Request, res: Response, next: NextFunction) => {
  // Check if we bypass session in development.
  if (
    (process.env.NODE_ENV === "dev" || process.env.NODE_ENV === "local") &&
    req.headers.authorization
  ) {
    const authHeader = req.headers.authorization.split(" ");
    if (authHeader.length === 2) {
      const token = authHeader[1];
      if (token === config.devMasterToken) {
        req.session.uid = config.devMasterSession.uid;
        req.session.role = config.devMasterSession.role;
      }
    }
  }

  if (req.session.uid && req.session.role) {
    next();
  } else {
    responses.unauthorized(res);
  }
};

export default verifySession;

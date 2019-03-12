import {Request} from "express";

const roles = [
    "student",
    "company",
    "bedkom",
    "bedkom-admin",
    "super-admin",
];

export const isPermitted = (req: Request, minRole: string): boolean => {

    // Check if the request has a JWT payload.
    if (!req.jwt) {
        return false;
    }

    // Request contains JWT payload, use it to compare to other.
    const role = req.jwt.role;
    if (role === minRole) {
        return true;
    }
    const aIdx = roles.indexOf(role);
    const bIdx = roles.indexOf(minRole);
    return aIdx > bIdx;
};

export default roles;

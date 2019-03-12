import { body } from "express-validator/check";

const vUsername = body("username").isString().custom((value) => value.length === 6);
const vPassword = body("password").isString();
const vOrg = body("org").isIn(["uib", "hvl"]);

export {
    vUsername,
    vPassword,
    vOrg,
};

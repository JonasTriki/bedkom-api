import {body} from "express-validator/check";
import jsonschema from "jsonschema";
import FoodEntries from "./jsonschemas/FoodEntries";

const vUsername = body("username").isString().custom((value) => value.length === 6);
const vPassword = body("password").isString();
const vOrg = body("org").isIn(["uib", "hvl"]);
const vFoodEntries = body("foodEntries").isArray().custom((json) => {
    const result = jsonschema.validate(json, FoodEntries);
    if (!result.valid) {
        throw new Error();
    }
    return true;
});

export {
    vUsername,
    vPassword,
    vOrg,
    vFoodEntries
};

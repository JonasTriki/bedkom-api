import { body } from "express-validator/check";
import jsonschema from "jsonschema";
import ContactPersons from "./jsonschemas/ContactPersons";
import FoodEntries from "./jsonschemas/FoodEntries";
import Semesters from "./models/enums/Semesters";

const vUsername = body("username")
  .trim()
  .isString()
  .custom(value => value.length === 6);
const vPassword = body("password")
  .isString()
  .isLength({ min: 1 });
const vOrg = body("org").isIn(["uib", "hvl"]);

const vFoodEntries = body("foodEntries")
  .isArray()
  .custom(json => {
    const result = jsonschema.validate(json, FoodEntries);
    if (!result.valid) {
      throw new Error();
    }
    return true;
  });

const vYear = body("year").matches(/^\d{4}$/);
const vSemester = body("semester").isIn(Semesters);
const vPresentations = [
  body("companyId").isUUID(4),
  body("capacity")
    .isNumeric()
    .custom(num => num > 0),
  body("minStudyYear")
    .isNumeric()
    .custom(num => num >= -1),
  body("startTime").isNumeric(),
  body("endTime").isNumeric(),
  body("responsible").isArray(),
  body("menuId")
    .isUUID(4)
    .optional(),
  body("description").isString()
];

const vContactPersons = body("contactPersons")
  .isArray()
  .custom(json => {
    const result = jsonschema.validate(json, ContactPersons);
    if (!result.valid) {
      throw new Error();
    }
    return true;
  });

export {
  vUsername,
  vPassword,
  vOrg,
  vFoodEntries,
  vContactPersons,
  vYear,
  vSemester,
  vPresentations
};

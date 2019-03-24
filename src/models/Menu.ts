import { Schema } from "dynamoose";
import jsonschema from "jsonschema";
import db from "../db";
import FoodEntries from "../jsonschemas/FoodEntries";

interface FoodEntry {
    name: string;
    details: string;
}

interface Menu {
    id: string;
    name: string;
    foodEntries: [FoodEntry];
    url: string;
}

const MenuModel = db.model<Menu, string>("bedkom-menus", new Schema({
    id: {
        type: String,
        required: true,
        hashKey: true
    },
    name: {
        type: String,
        required: true,
    },
    foodEntries: {
        type: [Object],
        required: true,
        validate: (vals: object[]) => {

            // Makes sure that our food entry is valid.
            return jsonschema.validate(vals, FoodEntries).valid;
        }
    },
    url: {
        type: String,
    }
}));

export default MenuModel;

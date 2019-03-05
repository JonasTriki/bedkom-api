import { Model, Schema } from "dynamoose";
import db from "../db";
import Semesters from "./enums/Semesters";

interface LastAuthorized {
    id: string;
    year: number;
    semester: string;
}

const LastAuthorizedModel = db.model<LastAuthorized, string>("bedkom-last-authorized-users", new Schema({
    id: {
        type: String,
        required: true,
        hashKey: true
    },
    year: {
        type: Number,
        required: true
    },
    semester: {
        type: String,
        required: true,
        enum: Semesters
    }
}));

export default LastAuthorizedModel;

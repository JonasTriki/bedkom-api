import { Schema } from "dynamoose";
import db from "../db";
import Semesters from "./enums/Semesters";

interface Dot {
    id: string;
    userId: string;
    year: number;
    semester: string;
    comment: string;
}

const DotModel = db.model<Dot, string>("bedkom-dots", new Schema({
    id: {
        type: String,
        required: true,
        hashKey: true
    },
    userId: {
        type: String,
        required: true,
    },
    year: {
        type: Number,
        required: true
    },
    semester: {
        type: String,
        required: true,
        enum: Semesters
    },
    comment: {
        type: String,
        required: true
    }
}));

export default DotModel;

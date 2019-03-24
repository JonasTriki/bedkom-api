import { Schema } from "dynamoose";
import db from "../db";
import Semesters from "./enums/Semesters";

export interface Presentation {
    id: string;
    companyId: string;
    registrations: number;
    capacity: number;
    minStudyYear: number;
    year: number;
    semester: string;
    startTime: number;
    endTime: number;
    responsible: string[];
    contractUrl?: string;
    menuId?: string;
    description: string;
}

const PresentationModel = db.model<Presentation, string>("bedkom-presentations", new Schema({
    id: {
        type: String,
        required: true,
        hashKey: true
    },
    companyId: {
        type: String,
        required: true
    },
    registrations: {
        type: Number,
        required: true,
    },
    capacity: {
        type: Number,
        required: true,
    },
    minStudyYear: {
        type: Number,
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
    startTime: {
        type: Date,
        required: true,
    },
    endTime: {
        type: Date,
        required: true,
    },
    responsible: {
        type: [String],
        required: true,
    },
    contractUrl: {
        type: String,
    },
    menuId: {
        type: String,
    },
    description: {
        type: String,
        required: true,
    },
}));

export default PresentationModel;

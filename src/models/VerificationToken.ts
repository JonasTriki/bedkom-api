import {Schema} from "dynamoose";
import db from "../db";
import Semesters from "./enums/Semesters";
import StudyPrograms from "./enums/StudyPrograms";
import {User} from "./User";

interface VerificationToken extends User {
    token: string;
}

const VerificationTokenModel = db.model<VerificationToken, string>("bedkom-verification-tokens",
    new Schema({
        id: {
            type: String,
            required: true,
            hashKey: true,
        },
        token: {
            type: String,
            required: true
        },
        firstName: {
            type: String,
            required: true
        },
        lastName: {
            type: String,
            required: true
        },
        email: {
            type: String,
            required: true
        },
        studyProgram: {
            type: String,
            required: true,
            enum: StudyPrograms
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
    }, {expires: 24 * 60 * 60})); /* Tokens expire after 1 day */

export default VerificationTokenModel;

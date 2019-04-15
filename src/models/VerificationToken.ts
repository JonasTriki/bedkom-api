import {Schema} from "dynamoose";
import db from "../db";
import Semesters from "./enums/Semesters";
import StudyPrograms from "./enums/StudyPrograms";
import {User} from "./User";

interface VerificationToken extends User {
  token: string;
  hash: string;
}

const VerificationTokenModel = db.model<VerificationToken, string>("bedkom-verification-tokens",
  new Schema({
    id: {
      type: String,
      required: true,
      hashKey: true,
    },
    hash: {
      type: String,
      required: true,
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
    org: {
      type: String,
      required: true
    },
    studyProgram: {
      type: String,
      required: true,
      enum: StudyPrograms
    },
    startYear: {
      type: Number,
      required: true
    },
    startSemester: {
      type: String,
      required: true,
      enum: Semesters
    },
    year: {
      type: Number,
      required: true
    }
  }, {expires: 24 * 60 * 60})); /* Tokens expire after 1 day */

export default VerificationTokenModel;

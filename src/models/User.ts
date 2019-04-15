import {Schema} from "dynamoose";
import db from "../db";
import Semesters from "./enums/Semesters";
import StudyPrograms from "./enums/StudyPrograms";
import UserRoles from "./enums/UserRoles";

export interface User {
  id: string;
  firstName: string;
  lastName: string;
  email: string;
  org: string;
  studyProgram: string;
  startYear: number;
  startSemester: string;
  year: number;
}

export interface UserHashed extends User {
  role: string;
  hash: string;
}

const UserModel = db.model<UserHashed, string>("bedkom-users", new Schema({
  id: {
    type: String,
    required: true,
    hashKey: true
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
  },
  role: {
    type: String,
    required: true,
    enum: UserRoles
  },
  hash: {
    type: String,
    required: true
  }
}));

export default UserModel;

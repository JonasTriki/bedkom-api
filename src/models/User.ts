import {Schema} from "dynamoose";
import db from "../db";
import Semesters from "./enums/Semesters";
import StudyPrograms from "./enums/StudyPrograms";
import UserRoles from "./enums/UserRoles";
import CommitteePosition from "./types/CommitteePosition";

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
  imgUrl?: string;
  committeePosition?: CommitteePosition;
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
  },
  imgUrl: {
    type: String
  },
  committeePosition: {
    type: String
  },
}));

export default UserModel;

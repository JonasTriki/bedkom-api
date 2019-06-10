import { Schema } from "dynamoose";
import db from "../db";

export interface Registration {
  id: string;
  userId: string;
  presentationId: string;
}

const RegistrationModel = db.model<Registration, string>(
  "bedkom-registrations",
  new Schema({
    id: {
      type: String,
      required: true,
      hashKey: true
    },
    userId: {
      type: String,
      required: true
    },
    presentationId: {
      type: String,
      required: true
    }
  })
);

export default RegistrationModel;

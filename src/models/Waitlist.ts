import { Schema } from "dynamoose";
import db from "../db";

export interface Waitlist {
  id: string;
  userId: string;
  presentationId: string;
}

const WaitlistModel = db.model<Waitlist, string>(
  "bedkom-waitlists",
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

export default WaitlistModel;

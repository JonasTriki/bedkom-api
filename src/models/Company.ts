import {Schema} from "dynamoose";
import jsonschema from "jsonschema";
import db from "../db";
import ContactPersons from "../jsonschemas/ContactPersons";

interface ContactPerson {
  name: string;
  position: string;
  email: string;
  phone: string;
}

export interface Company {
  id: string;
  name: string;
  description: string;
  website: string;
  bannerImgUrl: string;
  contactPersons: ContactPerson[];
}

const CompanyModel = db.model<Company, string>("bedkom-companies", new Schema({
  id: {
    type: String,
    required: true,
    hashKey: true
  },
  name: {
    type: String,
    required: true,
  },
  description: {
    type: String,
    required: true
  },
  website: {
    type: String,
    required: true,
  },
  bannerImgUrl: {
    type: String,
    required: true
  },
  contactPersons: {
    type: [Object],
    required: true,
    validate: (vals: object[]) => {

      // Makes sure that our contact person is valid.
      return jsonschema.validate(vals, ContactPersons).valid;
    }
  },
}));

export default CompanyModel;

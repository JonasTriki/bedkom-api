import { Schema } from "dynamoose";
import db from "../db";

interface Company {
    id: string;
    name: string;
    description: string;
    website: string;
    bannerImgUrl: string;
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
    }
}));

export default CompanyModel;

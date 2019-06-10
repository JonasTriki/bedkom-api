import { Schema } from "dynamoose";
import db from "../db";

export interface Comment {
  id: string;
  authorId: string;
  date: Date;
  content: string;
}

export interface Article {
  id: string;
  authorId: string;
  datePublished: Date;
  dateModified: Date;
  headline: string;
  description: string;
  content: string;
  comments?: Comment[];
}

const ArticleModel = db.model<Article, string>(
  "bedkom-news",
  new Schema({
    id: {
      type: String,
      required: true,
      hashKey: true
    },
    authorId: {
      type: String,
      required: true
    },
    datePublished: {
      type: Date,
      required: true
    },
    dateModified: {
      type: Date,
      required: true
    },
    headline: {
      type: String,
      required: true
    },
    description: {
      type: String,
      required: true
    },
    content: {
      type: String,
      required: true
    },
    comments: {
      type: [Object]
    }
  })
);

export default ArticleModel;

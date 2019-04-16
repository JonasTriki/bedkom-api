import cors from "cors";
import express from "express";
import morgan from "morgan";
import serverless from "serverless-http";

// NOTE: Connect DynamoDb does not have Type definitions.
// @ts-ignore
import connectDynamoDb from "connect-dynamodb";
import session from "express-session";
import {v4} from "uuid";
import api from "./api";
import config from "./config";
import logger from "./logger";
import responses from "./responses";

// Configuring session store
const DynamoDBStore = connectDynamoDb({session});
const storeOpts = {
  table: config.awsPrefix + "bedkom-sessions",
  AWSConfigJSON: config.awsConfig
};
const sessionOpts: session.SessionOptions = {
  name: "sid",
  secret: config.sessionSecret,
  genid: () => v4(),
  store: new DynamoDBStore(storeOpts),
  resave: false,
  saveUninitialized: false,
};

const app = express();
const port = process.env.PORT || 8080;

// Setup middlewares
app.use(cors({credentials: true, origin: "http://localhost:3000"}));
app.use(morgan("dev"));
app.use(express.json());
app.use(express.urlencoded({extended: false}));
app.use(session(sessionOpts));

// Setup routes
app.use(api);

// All unknown routes are unauthorized by default
app.use((req, res) => responses.unauthorized(res));

app.listen(port, () => {
  logger.info(`Server started at http://localhost:${port}`);
});

export default serverless(app);

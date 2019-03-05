import cors from "cors";
import express from "express";
import morgan from "morgan";
import serverless from "serverless-http";

import api from "./api";
import logger from "./logger";
import responses from "./responses";

const app = express();
const port = process.env.PORT || 8080;

// Setup middlewares
app.use(cors());
app.use(morgan("dev"));
app.use(express.json());
app.use(express.urlencoded({ extended: false }));

// Setup routes
app.use(api);

// All unknown routes are unauthorized by default
app.use((req, res) => responses.unauthorized(res));

app.listen(port, () => {
    logger.info(`Server started at http://localhost:${port}`);
});

export default serverless(app);

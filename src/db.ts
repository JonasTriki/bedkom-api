import dynamoose from "dynamoose";
import config from "./config";
import logger from "./logger";

dynamoose.AWS.config.update(config.awsConfig);

dynamoose.setDefaults({
  create: true,
  update: true,
  prefix: config.awsPrefix
});

logger.info("DynamoDB connected!");

export default dynamoose;

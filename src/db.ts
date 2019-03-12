import dynamoose from "dynamoose";
import config from "./config";
import logger from "./logger";

dynamoose.AWS.config.update({
    accessKeyId: config.awsAccessKeyId,
    secretAccessKey: config.awsSecretAccessKey,
    region: config.awsRegion
});

dynamoose.setDefaults({
    create: true,
    update: true,
    prefix: config.awsPrefix
});

logger.info("DynamoDB connected!");

export default dynamoose;

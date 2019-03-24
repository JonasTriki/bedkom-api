// Create a copy of this file (name it config.ts) and replace all "TODO" fields
// with respected values.

const dev = {

    // AWS DynamoDb URL
    awsPrefix: "dev-",
};

const prod = {

    // AWS DynamoDb URL
    awsPrefix: "prod-",
};

const config = process.env.NODE_ENV === "prod" ? prod : dev;

export default {
    ...config,

    awsAccessKeyId: "TODO",
    awsRegion: "TODO",
    awsSecretAccessKey: "TODO",

    // Generated from: https://www.grc.com/passwords.htm
    jwtSecret: "TODO",

    devMasterToken: "TODO",
    devMasterJWTPayload: {
        id: "abc123",
        role: "super-admin",
    },
};

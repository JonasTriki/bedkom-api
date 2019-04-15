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

  awsConfig: {
    accessKeyId: "TODO",
    secretAccessKey: "TODO",
    region: "TODO",
  },

  // Generated from: https://www.grc.com/passwords.htm
  sessionSecret: "TODO",

  // Bypassing sessionized auth endpoints in dev
  devMasterToken: "TODO",
  devMasterSession: {
    uid: "abc123",
    role: "super-admin",
  },
};

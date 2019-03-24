import {S3} from "aws-sdk";
import {Express} from "express";
import path from "path";
import {v4} from "uuid";
import config from "./config";

const bucketName = config.awsPrefix + "bedkom-resources";
const s3 = new S3({
    credentials: {
        accessKeyId: config.awsAccessKeyId,
        secretAccessKey: config.awsSecretAccessKey
    },
});

const uploadToS3 = (file: Express.Multer.File, name?: string): Promise<string> => {
    return new Promise((resolve, reject) => {
        const filename = (name || v4()) + path.extname(file.originalname);

        s3.upload({
            ACL: "public-read",
            Bucket: bucketName,
            Key: filename,
            Body: file.buffer,
            CacheControl: "max-age=31536000", // TODO: Change this from 1 year to a reasonable cache age?
        }, (err, data) => {
            if (err) {
                reject(err);
                return;
            }

            // Return with url of uploaded file.
            resolve(data.Location);
        });

        // TODO: What if bucket doesn't exist?
        /*s3.waitFor("bucketExists", {Bucket: bucketName}, async (existsErr) => {
            if (existsErr) {

                // Create the bucket
                const bucketCreated = await s3.createBucket({
                    Bucket: bucketName,
                    CreateBucketConfiguration: {
                        LocationConstraint: "eu-central-1"
                    }
                }).promise();

                if (bucketCreated.$response.error) {
                    reject(bucketCreated.$response.error);
                }
            }

        });*/
    });
};

export {
    uploadToS3
};

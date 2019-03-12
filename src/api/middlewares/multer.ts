import multer from "multer";

const upload = multer({
    storage: multer.memoryStorage(),
    limits: {
        fileSize: 5 * 1024 * 1024 // Max 5MB, due to AWS Lambda constraints
    },
});

export default upload;

export default {
  type: "array",
  items: {
    type: "object",
    properties: {
      name: {
        type: "string"
      },
      position: {
        type: "string"
      },
      email: {
        type: "string",
        format: "email"
      },
      phone: {
        type: "string",
        format: "phone"
      }
    },
    required: ["name", "position", "email", "phone"]
  }
};

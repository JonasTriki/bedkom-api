export default {
  type: "array",
  items: {
    type: "object",
    properties: {
      name: {
        type: "string",
      },
      details: {
        type: "string",
      },
    },
    required: ["name", "details"],
  },
};

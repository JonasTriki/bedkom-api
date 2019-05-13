import {Router} from "express";
import {Article} from "../../../models/Article";
import responses from "../../../responses";

const router = Router();

router.get("/", (req, res) => {

  // TODO: Implement other endpoints (such as create, remove, edit, comment, etc.).
  // For now, just display static data.
  const published = new Date(2019, 4, 29, 11, 0, 0);
  const modified = new Date(2019, 4, 29, 11, 23, 30);
  const news: Article[] = [
    {
      id: "article-0",
      headline: "God sommer fra oss i styret!",
      description: "Kos dere med masse is og sol!",
      authorId: "jtr008",
      datePublished: published,
      dateModified: modified,
      content: "😎",
    },
    {
      id: "article-1",
      headline: "Nytt semester!",
      description: "Nye spennende bedrifter kommer på besøk.",
      authorId: "jtr008",
      datePublished: published,
      dateModified: modified,
      content: "Masse tekst her...",
    }
  ];

  // Respond with articles
  responses.ok(news, res);
});

export default router;

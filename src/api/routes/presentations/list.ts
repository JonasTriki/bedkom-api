import {Request, Response, Router} from "express";
import CompanyModel from "../../../models/Company";
import {isPermitted} from "../../../models/enums/UserRoles";
import MenuModel from "../../../models/Menu";
import PresentationModel, {Presentation} from "../../../models/Presentation";
import responses from "../../../responses";

const router = Router();

router.get("/", async (req: Request, res: Response) => {
  try {
    // const bedkom = isPermitted(req, "bedkom");

    // List out the presentations
    const menus = await MenuModel.scan().exec();
    const companies = await CompanyModel.scan().exec();
    const presentations: Presentation[] = await PresentationModel.scan().exec();

    // Map companies to presentations
    const mapped = presentations.map((presentation) => {

      // Find company and menu attached to presentation
      const company = companies.find((c) => c.id === presentation.companyId);
      const menu = menus.find((m) => m.id === presentation.menuId);

      return {
        ...presentation,
        company: company ? {
          name: company.name,
          bannerImgUrl: company.bannerImgUrl,
          website: company.website,
        } : null,
        menu: menu ? {
          url: menu.url,
          foodEntries: menu.foodEntries,
        } : null,
      };

      // Bedkom members get access to all information.
      /*if (bedkom) {
        return {...presentation, company, menu};
      } else {

      }*/
    });

    // Return with all presentations
    responses.ok(mapped, res);
  } catch (err) {
    responses.unexpectedError(req, res);
  }
});

export default router;

import cheerio from "cheerio";
import request from "request-promise-native";
import studyPrograms from "../models/enums/studyPrograms";

const rp = request.defaults({ followAllRedirects: true });

interface Study {
    program: string;
    year: number;
    semester: string;
}

interface ProfileDetails {
    firstName: string;
    lastName: string;
    email: string;
}

interface Student {
    details: ProfileDetails;
    studies: Study[];
}

const onlyInformaticsStudies = (program: string) => studyPrograms.indexOf(program) > -1;

const parseProfileDetails = ($: CheerioStatic): ProfileDetails | null => {

    try {
        // This assumes that the student must have atleast 1 firstname and 1 lastname.
        const profileWrapper = $('p:contains("studentnummer:")').text().trim().split("\n");
        const fullName = profileWrapper[0].trim().split(" "); // First line contains fullname
        const firstName = fullName.slice(0, fullName.length - 1).join(" ");
        const lastName = fullName.slice(-1).join("").trim();

        // Fetch email from div
        const email = $("div[class=korr-epost]").text().trim();

        return {
            firstName,
            lastName,
            email
        };
    } catch (err) {
        console.log("Could not parse student: ", err);
        return null;
    }
};

const parseContainers = ($: CheerioStatic): Study[] => {
    const containers = $("a[class=studieContainer]");
    return containers
        .toArray()
        .map($) // Selects containers
        .filter((container) => container.html().indexOf("Aktiv") > -1) // Only active studies allowed
        .map((container) => {
            const params = new URLSearchParams(container.attr("href").split("?")[1]);

            return {
                program: params.get("studprog"),
                year: Number(params.get("arstall")),
                semester: params.get("termin").toLowerCase() === "høst" ? "spring" : "autumn"
            };
        });
};

const scrapStudent = async (feidename: string, password: string, org: string): Promise<Student | null> => {
    const cookieJar = rp.jar();

    // First, we fetch the fields we need to send to the login.
    const loginRes = await rp.get({
        url: "https://fsweb.no/studentweb/login.jsf?inst=FS" + org.toUpperCase(),
        resolveWithFullResponse: true,
        jar: cookieJar
    });
    const loginResParsed = cheerio.load(loginRes.body, { decodeEntities: false });
    const feideModuleBoxHtml = loginResParsed("section[data-flap-name=feide]").html();
    const feideModuleBox = cheerio.load(feideModuleBoxHtml);
    const hiddenInputs = feideModuleBox("input[type=hidden]")
        .map((i, el) => {
            const it = feideModuleBox(el);
            return { [it.attr("name")]: it.attr("value") };
        }).get();
    const extraRe = /mojarra\.jsfcljs\(.*{'(.*)':'(.*)'}.*\)/g;
    const extraFields = extraRe.exec(feideModuleBoxHtml);
    let fields = { [extraFields[1]]: extraFields[2] };
    hiddenInputs.forEach((field) => {
        fields = { ...fields, ...field };
    });

    // Then we send the post request to the login jsf
    const loginJsfRes = await rp.post({
        url: "https://fsweb.no/studentweb/login.jsf",
        form: fields,
        resolveWithFullResponse: true,
        jar: cookieJar
    });
    const loginJsfResParsed = cheerio.load(loginJsfRes.body);
    const asLen = loginJsfResParsed("input[name=asLen]").attr("value");
    const AuthState = loginJsfResParsed("input[name=AuthState]").attr("value");
    const finalReqUrl = loginJsfRes.request.uri.href;

    // Now we assemble the login post payload
    const loginPostPayload = {
        asLen,
        AuthState,
        org: org + ".no",
        has_js: "true",
        inside_iframe: "0",
        feidename,
        password
    };
    const loginPostRes = await rp.post({
        url: finalReqUrl,
        form: loginPostPayload,
        resolveWithFullResponse: true,
        jar: cookieJar
    });
    const loginPostResParsed = cheerio.load(loginPostRes.body);
    const samlVal = loginPostResParsed("input[name=SAMLResponse]").attr("value");

    // Check if we logged in successfully
    const isLoggedIn = samlVal !== undefined;
    if (!isLoggedIn) {

        // Handle wrong username/password login attempt.
        return undefined;
    }

    // Needed to update the cookie jar with signed in credentials.
    await rp.post({
        url: "https://fsweb.no/studentweb/samlsso.jsf",
        form: { SAMLResponse: samlVal },
        jar: cookieJar,
    });

    // Fetch students profile data (firstname, lastname, email)
    const myProfileRes = await rp.get({
        url: "https://fsweb.no/studentweb/minprofil.jsf",
        jar: cookieJar,
    });
    const myProfileParsed = cheerio.load(myProfileRes, { decodeEntities: false });
    const myProfile = parseProfileDetails(myProfileParsed);
    if (myProfile === null) {

        // Something strange happened, could not parse students profile.
        return null;
    }

    // Now we fetch the current studies
    const currentStudies = await rp.get({
        url: "https://fsweb.no/studentweb/studier.jsf",
        jar: cookieJar,
    });
    const currentStudiesParsed = cheerio.load(currentStudies, { decodeEntities: false });

    // At last, we parse each study container (each square you see when you navigate to studies)
    // and check if it is active. If so, we add it to the array.
    const studies = parseContainers(currentStudiesParsed);

    return {
        details: myProfile,
        studies: studies.filter(({ program }) => onlyInformaticsStudies(program))
    };
};

export const fetchInformaticsStudent = (username: string, password: string, org: string): Promise<Student | null> => {
    return scrapStudent(username, password, org);
};

import configparser
import re

import requests
from bs4 import BeautifulSoup

configParser = configparser.ConfigParser()
configParser.read("me.ini")
config = configParser["DEFAULT"]
username = config["feidename"]
password = config["password"]


def writeHtml(source):
    f = open("test.html", "w+")
    f.write(source)
    f.close()


def extractFieldNameValue(field):
    return field["name"], field["value"]


session = requests.Session()

# Fetch login html
fswebUrl = "https://fsweb.no/studentweb/login.jsf?inst=FSUIB"
fswebHtml = session.get(fswebUrl).text
fswebSoup = BeautifulSoup(fswebHtml, features="html.parser")

# Scrap form hidden inputs and prepare payload for feide login.
feideModuleBox = fswebSoup.find("section", {"data-flap-name": "feide"})
hiddenFields = feideModuleBox.findAll("input", {"type": "hidden"})
hiddenNameValues = dict(map(extractFieldNameValue, hiddenFields))

# We also need to scrap one additional field set using js
additionalField = re.search(
    r"mojarra\.jsfcljs\(.*{'(.*)':'(.*)'}.*\)", str(feideModuleBox)).groups()
hiddenNameValues[additionalField[0]] = additionalField[1]

# Then, we made a POST request to get the Feide url
fswebLoginUrl = "https://fsweb.no/studentweb/login.jsf"
fswebLogin = session.post(fswebLoginUrl, data=hiddenNameValues)
fswebLoginSoup = BeautifulSoup(fswebLogin.text, features="html.parser")
asLen = fswebLoginSoup.find(attrs={"name": "asLen"})["value"]
authState = fswebLoginSoup.find(attrs={"name": "AuthState"})["value"]

fswebLoginPostPayload = {
    "asLen": asLen,
    "AuthState": authState,
    "org": "uib.no",
    "has_js": "true",
    "inside_iframe": "0",
    "feidename": username,
    "password": password
}
fswebLoginPost = session.post(fswebLogin.url, data=fswebLoginPostPayload)
fswebLoginPostSoup = BeautifulSoup(fswebLoginPost.text, features="html.parser")

finalPostUrl = "https://fsweb.no/studentweb/samlsso.jsf"
finalPostPayload = {
    "SAMLResponse": fswebLoginPostSoup.find(attrs={"name": "SAMLResponse"})["value"]
}
studentWeb = session.post(finalPostUrl, data=finalPostPayload)

studiesUrl = "https://fsweb.no/studentweb/studier.jsf"
currentStudies = session.get(studiesUrl)
currentStudiesSoup = BeautifulSoup(currentStudies.text, features="html.parser")

study = currentStudiesSoup.find("span", {"class": "studieTittel"}).text

print(study)

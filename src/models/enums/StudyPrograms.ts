// Here we define what study years the programs may have (bachelors degree, masters degree etc.).
type Degree = "bachelor" | "3rd-year-only" | "master";

interface StudyPrograms {
  [key: string]: Degree;
}

export const studyDegrees: StudyPrograms = {
  "BAMN-DTEK": "bachelor",
  "BAMN-DSIK": "bachelor",
  "BAMN-DVIT": "bachelor",
  "BAMN-BINF": "bachelor",
  "BATF-IMOE": "bachelor",
  "BASV-IKT": "bachelor",
  "BASV-KOGNI": "3rd-year-only", // Only 3rd year KOGVIT
  "MAMN-INF": "master",
  "MAMN-PROG": "master"
};

/*

export default [
    "BAMN-DTEK",
    "BAMN-DSIK",
    "BAMN-DVIT",
    "BAMN-BINF",
    "BATF-IMOE",
    "BASV-IKT",
    "BASV-KOGNI",
    "MAMN-INF",
    "MAMN-PROG",
];

 */

export default Object.keys(studyDegrees);

const semesterYear = (date: Date) => {
  const month = date.getMonth();
  return {
    year: date.getFullYear(),
    semester: month > 7 ? "spring" : "autumn"
  };
};

export const getSemesterYear = (unixTime: number) => {
  return semesterYear(new Date(unixTime * 1000));
};

export const currentSemesterYear = () => {
  return semesterYear(new Date());
};

export const currentSemesterYear = () => {
    const now = new Date();
    const month = now.getMonth();
    return {
        year: now.getFullYear(),
        semester: month > 7 ? "spring" : "autumn"
    };
};

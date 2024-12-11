function loadInput() {
  const file = Deno.readTextFileSync("./input.txt");

  const reports: number[][] = [];

  for (const line of file.split("\n")) {
    reports.push(line.split(" ").map((s) => Number(s)));
  }

  reports.pop(); // empty list at end
  return reports;
}

const isReportValid = (report: number[], increasing?: boolean): boolean => {
  const [first, second, ...rest] = report;
  if (first === undefined) return true;
  if (second === undefined) return true;
  if (increasing === undefined) {
    increasing = first < second;
  }
  const isValid = increasing ? first + 3 >= second && first < second : first - 3 <= second && first > second;
  return rest === undefined ? isValid : isValid && isReportValid([second, ...rest], increasing);
};

function part1(reports: number[][]) {
  const results = reports.map((report) => isReportValid(report));
  return results.filter((res) => res === true).length;
}

// reports is number[][]
// [
//  [...],
//  ...
// ]

function part2(reports: number[][]) {
  const generateReportsWithOneNumberRemoved = (report: number[]): number[][] => {
    return report.map((_, i) => report.filter((_, j) => i !== j));
  };

  // reportsPlusOmissions is numbers[][][]
  // [
  //  [[...], ...]
  //  ...
  // ]

  const reportsPlusOmissions = reports.map((report) => [report].concat(generateReportsWithOneNumberRemoved(report)));

  // turn it into boolean[]
  // [
  //  true/false,
  //  ...
  // ]

  return reportsPlusOmissions
    .flatMap((reports) => reports.map((r) => isReportValid(r)).reduce((a, b) => a || b))
    .filter((v) => v).length;
}

function main() {
  const reports = loadInput();
  console.log("Part 1:", part1(reports));
  console.log("Part 2:", part2(reports));
}

main();

function loadInput() {
  const file = Deno.readTextFileSync("./input.txt");

  const reports: number[][] = [];

  for (const line of file.split("\n")) {
    reports.push(line.split(" ").map((s) => Number(s)));
  }

  reports.pop(); // empty list at end
  return reports;
}

function part1(reports: number[][]) {
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

  const results = reports.map((report) => isReportValid(report));
  return results.filter((res) => res === true).length;
}

// part 2

function main() {
  const reports = loadInput();
  console.log("Part 1:", part1(reports));
  console.log("Part 2:");
}

main();

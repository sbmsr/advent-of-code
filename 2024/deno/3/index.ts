function parseInput(): string {
  return Deno.readTextFileSync("./input.txt");
}

function part1(input: string): number {
  const regex = /mul\((\d+(?:,\d+)*)\)/g;

  let acc = 0;
  for (const match of input.matchAll(regex)) {
    const [first, second] = match[1].split(",");
    acc += Number(first) * Number(second);
  }

  return acc;
}

function part2(input: string): number {
  const regex = /mul\((\d+(?:,\d+)*)\)|do\(\)|don\'t\(\)/g;

  let doing = true;
  let acc = 0;

  for (const match of input.matchAll(regex)) {
    if (doing && match[0].includes(",")) {
      const [first, second] = match[1].split(",");
      acc += Number(first) * Number(second);
    }

    if (match[0] === "do()") {
      doing = true;
    }
    
    if (match[0] === "don't()") {
      doing = false;
    }
  }
  return acc;
}

function main() {
  const input = parseInput();
  console.log("part 1:", part1(input));
  console.log("part 2:", part2(input));
}

main();

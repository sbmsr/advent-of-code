import { loadInput } from "../lib.ts";

async function part1(left: number[], right: number[]): Promise<number> {
  const sortedLeft = [...left].sort();
  const sortedRight = [...right].sort();

  let accumulator = 0;

  for (let x = 0; x < Math.min(sortedLeft.length, sortedRight.length); x++) {
    accumulator += Math.abs(sortedLeft[x] - sortedRight[x]);
  }

  return accumulator;
}

async function part2(left: number[], right: number[]): Promise<number> {
  // similiarity score = sum(num * occurences in right)

  const counts: Record<string, number> = {};

  for (let x = 0; x < right.length; x++) {
    const key = right[x].toString();
    if (counts[key] === undefined) {
      counts[key] = 1;
    } else {
      counts[key] += 1;
    }
  }

  let accumulator = 0;

  for (let x = 0; x < left.length; x++) {
    const key = left[x].toString();
    if (counts[key] !== undefined) {
      accumulator += left[x] * counts[key];
    }
  }

  return accumulator;
}

async function main() {
  const [left, right] = await loadInput("input.txt");

  console.log("Part 1:", await part1(left, right));
  console.log("Part 2:", await part2(left, right));
}

main();

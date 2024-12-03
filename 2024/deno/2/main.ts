import { loadInput } from "../lib.ts";

const [left, right] = await loadInput("../input.txt");

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
    accumulator += left[x] * counts[left[x].toString()];
  }
}

console.log(accumulator);

import { loadInput } from "../lib.ts";

const [left, right] = await loadInput("../input.txt");

left.sort();
right.sort();

let accumulator = 0;

for (let x = 0; x < Math.min(left.length, right.length); x++) {
  accumulator += Math.abs(left[x] - right[x]);
}

console.log(accumulator);

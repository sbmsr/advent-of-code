function parseInput() {
  const data: { pairs: number[][]; lists: number[][] } = { pairs: [], lists: []};
  Deno.readTextFileSync("./input.txt")
    .split("\n")
    .forEach((line) => {
      if (line.includes("|")) {
        const [left, right] = line.split("|").map(Number);
        data.pairs.push([left, right]);
      } else if (line.includes(",")) {
        data.lists.push(line.split(",").map(x => Number(x)))
      }
    });

    return data;
}

function part1(data: { pairs: number[][]; lists: number[][] }) {
  const precedenceMap: Record<number, Set<number>> = {};

  // 1. Build the map
  for (const [left, right] of data.pairs) {
    precedenceMap[left] === undefined ? (precedenceMap[left] = new Set([right])) : precedenceMap[left].add(right);
  }

  console.log(precedenceMap)

  // 2. use map to determine valid lists
  const validIdxs: number[] = [];

  for (const [listIdx, list] of data.lists.entries()) {
    console.log("working on list:", list)
    const seenSet = new Set();
    let failed = false;
    
    for (let idx = 0; idx < list.length; idx++) {
      const listItem = list[idx];
      if (Array.from(precedenceMap[listItem] || []).some(x => seenSet.has(x))) {
        console.log(`List invalid because ${listItem} must come before ${Array.from(precedenceMap[listItem].intersection(seenSet)).join(",")}`);
        failed = true;
        break;
      } else {
        seenSet.add(listItem);
      }
    }

    if (!failed) {
      validIdxs.push(listIdx);
    }
  }

  // 3. for each valid list, sum middle elements
  return Array.from(data.lists.entries())
    .filter(([idx]) => validIdxs.includes(idx))
    .map(([_, val]) => val[Math.floor(val.length / 2)])
    .reduce((x, y) => x + y, 0);
}

function main() {
  const input = parseInput();
  console.log("part1:", part1(input));
}

main();

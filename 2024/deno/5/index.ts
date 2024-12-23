type Input = { pairs: number[][]; lists: number[][] };

function parseInput() {
  const data: Input = { pairs: [], lists: [] };
  Deno.readTextFileSync("./input.txt")
    .split("\n")
    .forEach((line) => {
      if (line.includes("|")) {
        const [left, right] = line.split("|").map(Number);
        data.pairs.push([left, right]);
      } else if (line.includes(",")) {
        data.lists.push(line.split(",").map((x) => Number(x)));
      }
    });

  return data;
}

function buildDependencyMap(pairs: Input["pairs"]): Map<number, Set<number>> {
  const dependencyMap: Map<number, Set<number>> = new Map();

  for (const [left, right] of pairs) {
    dependencyMap.get(left) === undefined
      ? dependencyMap.set(left, new Set([right]))
      : dependencyMap.get(left)?.add(right);
  }

  return dependencyMap;
}

function getListIdxs(lists: Input["lists"], dependencyMap: Map<number, Set<number>>, valid = true) {
  // use map to determine valid lists
  const idxs: number[] = [];

  for (const [listIdx, list] of lists.entries()) {
    // console.debug("working on list:", list);
    const seenSet = new Set();
    let failed = false;

    for (let idx = 0; idx < list.length; idx++) {
      const listItem = list[idx];
      if (Array.from(dependencyMap.get(listItem) || []).some((x) => seenSet.has(x))) {
        // console.debug(
        //   `List invalid because ${listItem} must come before ${Array.from(
        //     dependencyMap[listItem].intersection(seenSet)
        //   ).join(",")}`
        // );
        failed = true;
        break;
      } else {
        seenSet.add(listItem);
      }
    }

    if (valid && !failed) {
      idxs.push(listIdx);
    } else if (!valid && failed) {
      idxs.push(listIdx);
    }
  }

  return idxs;
}

function part1(inputData: Input) {
  const dependencyMap = buildDependencyMap(inputData.pairs);

  const validListIdxs = getListIdxs(inputData.lists, dependencyMap, true);

  // for each valid list, sum middle elements
  return Array.from(inputData.lists.entries())
    .filter(([idx]) => validListIdxs.includes(idx))
    .map(([_, val]) => val[Math.floor(val.length / 2)])
    .reduce((x, y) => x + y, 0);
}

function part2({ pairs, lists }: Input) {
  // find the invalid lists
  const dependencyMap = buildDependencyMap(pairs);
  const invalidListIdxs = getListIdxs(lists, dependencyMap, false);

  // for each one, fix the list

  const fixedLists = [];

  for (const idx of invalidListIdxs) {
    fixedLists.push(topologicalSort(lists[idx], dependencyMap));
  }

  // sum middle values
  return fixedLists.map((l) => l[Math.floor(l.length / 2)]).reduce((l, r) => l + r);
}

function topologicalSort(list: number[], dependencyMap: Map<number, Set<number>>) {
  // kahn's algorithm

  const result: number[] = [];
  const depCounts: Map<number, number> = new Map(); // {number}: {depCount}

  for (const num of list) {
    depCounts.set(num, 0);
  }

  // Build depCounts map for numbers in this list
  for (const num of list) {
    // count relevant dependencyMap entries
    let predecesorCount = 0;
    for (const [from, to] of dependencyMap.entries()) {
      if (to.has(num) && list.includes(from)) {
        predecesorCount++;
      }
    }
    depCounts.set(num, predecesorCount);
  }

  while (result.length !== list.length) {
    for (const [num, depCount] of depCounts) {
      if (depCount === 0) {
        result.push(num);
        for (const dependency of dependencyMap.get(num)?.values() ?? []) {
          const currentCount = depCounts.get(dependency);
          if (currentCount !== undefined) {
            depCounts.set(dependency, currentCount - 1);
          }
        }
        depCounts.delete(num);
      }
    }
  }

  return result;
}

function main() {
  const input = parseInput();
  console.log("part1:", part1(input));
  console.log("part2:", part2(input));
}

main();

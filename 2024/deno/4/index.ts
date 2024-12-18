function parseInput(): string[][] {
  return Deno.readTextFileSync("./input.txt")
    .split("\n")
    .map((i) => i.split(""))
    .filter((i) => i.length > 0);
}

function checkPositionForXMAS(input: string[][], x: number, y: number) {
  let acc = 0;
  const spaceNeeded = "XMAS".length - 1;
  // up
  if (y - spaceNeeded >= 0) {
    // up check is possible
    if (input[x][y] === "X") {
      if (input[x][y - 1] === "M") {
        if (input[x][y - 2] === "A") {
          if (input[x][y - 3] === "S") {
            acc += 1;
          }
        }
      }
    }
  }

  // down
  if (y + spaceNeeded <= input.length - 1) {
    // down check is possible
    if (input[x][y] === "X") {
      if (input[x][y + 1] === "M") {
        if (input[x][y + 2] === "A") {
          if (input[x][y + 3] === "S") {
            acc += 1;
          }
        }
      }
    }
  }

  // left
  if (x - spaceNeeded >= 0) {
    // left check is possible
    if (input[x][y] === "X") {
      if (input[x - 1][y] === "M") {
        if (input[x - 2][y] === "A") {
          if (input[x - 3][y] === "S") {
            acc += 1;
          }
        }
      }
    }
  }

  // right
  if (x + spaceNeeded <= input.length - 1) {
    // right check is possible
    if (input[x][y] === "X") {
      if (input[x + 1][y] === "M") {
        if (input[x + 2][y] === "A") {
          if (input[x + 3][y] === "S") {
            acc += 1;
          }
        }
      }
    }
  }

  // up left
  if (x - spaceNeeded >= 0 && y - spaceNeeded >= 0) {
    // up left check is possible
    if (input[x][y] === "X") {
      if (input[x - 1][y - 1] === "M") {
        if (input[x - 2][y - 2] === "A") {
          if (input[x - 3][y - 3] === "S") {
            acc += 1;
          }
        }
      }
    }
  }

  // up right
  if (x + spaceNeeded <= input.length - 1 && y - spaceNeeded >= 0) {
    // up right check is possible
    if (input[x][y] === "X") {
      if (input[x + 1][y - 1] === "M") {
        if (input[x + 2][y - 2] === "A") {
          if (input[x + 3][y - 3] === "S") {
            acc += 1;
          }
        }
      }
    }
  }

  // down left
  if (x - spaceNeeded >= 0 && y + spaceNeeded <= input.length - 1) {
    // down left check is possible
    if (input[x][y] === "X") {
      if (input[x - 1][y + 1] === "M") {
        if (input[x - 2][y + 2] === "A") {
          if (input[x - 3][y + 3] === "S") {
            acc += 1;
          }
        }
      }
    }
  }

  // down right
  if (x + spaceNeeded <= input.length - 1 && y + spaceNeeded <= input.length - 1) {
    // down right check is possible
    if (input[x][y] === "X") {
      if (input[x + 1][y + 1] === "M") {
        if (input[x + 2][y + 2] === "A") {
          if (input[x + 3][y + 3] === "S") {
            acc += 1;
          }
        }
      }
    }
  }

  return acc;
}

function part1(input: string[][]) {
  // need to perform a search at each spot
  let acc = 0;
  for (let x = 0; x < input.length; x++) {
    for (let y = 0; y < input[x].length; y++) {
      acc += checkPositionForXMAS(input, x, y);
    }
  }
  return acc;
}

function isTwoMASInShapeOfX(input: string[][], x: number, y: number) {
  // M . S    S . M    S . S    M . M
  // . A . or . A . or . A . or . A .
  // M . S    S . M    M . M    S . S

  const center = input[x]?.[y];
  const topLeft = input[x - 1]?.[y - 1];
  const topRight = input[x + 1]?.[y - 1];
  const bottomLeft = input[x - 1]?.[y + 1];
  const bottomRight = input[x + 1]?.[y + 1];

  // back diagonal (\)

  const backDiagonal = `${topLeft}${center}${bottomRight}`;

  // forward diagonal (/)

  const forwardDiagonal = `${bottomLeft}${center}${topRight}`;

  return (backDiagonal === "MAS" || backDiagonal === "SAM") && (forwardDiagonal === "MAS" || forwardDiagonal === "SAM");
}

function part2(input: string[][]) {
  // need to perform a search at each spot
  let acc = 0;
  for (let x = 0; x < input.length; x++) {
    for (let y = 0; y < input[x].length; y++) {
      if (isTwoMASInShapeOfX(input, x, y)) {
        acc += 1;
      }
    }
  }
  return acc;
}

function main() {
  const input = parseInput();
  console.log("part1", part1(input));
  console.log("part2", part2(input));
}

main();

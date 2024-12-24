type Op = "AND" | "OR" | "XOR";

type Connection = {
  a: string;
  op: Op;
  b: string;
};

function parseInput() {
  const input = Deno.readTextFileSync("./input.txt");
  const initialValues: Record<string, number> = {};
  const connections: Record<string, Connection> = {};
  for (const line of input.split("\n").filter((l) => l.trim() !== "")) {
    if (line.includes(":")) {
      // this is a initial value
      const [wire, value] = line.split(":").map((x) => x.trim());
      initialValues[wire] = Number(value);
    } else {
      // this is a connection
      const [a, operation, b, _, output] = line.split(" ").map((x) => x.trim());
      connections[output] = { a, op: operation as Op, b };
    }
  }

  return { initialValues, connections };
}

type Expression = {
  left: number | Expression;
  op: Op;
  right: number | Expression;
};

function buildFullExpression(
  connection: Connection,
  {
    initialValues,
    connections,
  }: {
    initialValues: Record<string, number>;
    connections: Record<string, Connection>;
  }
): Expression {
  const { a, b, op } = connection;
  let left, right;

  if (initialValues[a] !== undefined) {
    left = initialValues[a];
  }

  if (initialValues[b] !== undefined) {
    right = initialValues[b];
  }

  return {
    left: left ?? buildFullExpression(connections[a], { initialValues, connections }),
    op: op,
    right: right ?? buildFullExpression(connections[b], { initialValues, connections }),
  };
}

function computeExpression(expression: Expression): number {
  const leftNumber = typeof expression.left === "number" ? expression.left : computeExpression(expression.left);

  const rightNumber = typeof expression.right === "number" ? expression.right : computeExpression(expression.right);

  switch (expression.op) {
    case "AND":
      return leftNumber && rightNumber;
    case "OR":
      return leftNumber || rightNumber;
    case "XOR":
      return Number(leftNumber !== rightNumber);
  }
}

function main() {
  const input = parseInput();

  // 1. build fully expanded expression for each zXX
  const zs: Record<string, Expression> = {};

  for (let x = 0; ; x++) {
    const zKey = `z${x.toString().padStart(2, "0")}`;
    if (input.connections[zKey] === undefined) {
      break;
    }
    zs[zKey] = buildFullExpression(input.connections[zKey], input);
  }

  // 2. collapse each expression into a value
  const results: Record<string, number> = {};
  for (const [zKey, expression] of Object.entries(zs)) {
    results[zKey] = computeExpression(expression);
  }

  // 3. convert the binary value into a number
  let binaryNum = "";
  for (const num of Object.values(results)) {
    binaryNum = num + binaryNum;
  }

  console.log("part1", parseInt(binaryNum, 2));
}

main();

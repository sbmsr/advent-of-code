export async function loadInput(inputFilePath: string): Promise<[number[], number[]]> {
  const text = await Deno.readTextFile(inputFilePath);

  const lines = text.split("\n");
  lines.pop(); // remove last line ("")

  const left: Array<number> = [];
  const right: Array<number> = [];

  lines
    .map((line) => line.split("   "))
    .map(([l, r]) => {
      left.push(Number(l));
      right.push(Number(r));
    });

  return [left, right];
}

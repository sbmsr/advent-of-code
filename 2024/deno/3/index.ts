function parseInput() : string {
    return Deno.readTextFileSync("./input.txt")
}

function part1(input: string): number {
    const regex = /mul\((\d+(?:,\d+)*)\)/g;

    let acc = 0
    for (const match of input.matchAll(regex)) {
        const [first,second] = match[1].split(","); 
        acc += Number(first) * Number(second)
    }

    return acc
}

function part2(input: string) : number {
    return 0
}

function main() {
    const input = parseInput()
    console.log("part 1:", part1(input))
    console.log("part 2:", part2(input))
}

main()

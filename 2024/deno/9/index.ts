
function part1(input: number[]) {
    // construct string thingy
    const stringThingy = []
    for (const [idx, n] of input.entries()) {
        if (idx % 2 == 0) {
            for (let i = 0; i < n; i++) {
                stringThingy.push(String(idx/2))
            }
        } else {
            for (let i = 0; i < n; i++) {
                stringThingy.push(".")
            }
        }
    }

    // swap
    let [left, right] = [0, stringThingy.length]

    while (left < right) {
        if (!Number.isNaN(Number(stringThingy[left]))) {
            left += 1
            continue
        }
        if (Number.isNaN(Number(stringThingy[right]))) {
            right -= 1
            continue;
        }

        const temp: string = stringThingy[left]
        stringThingy[left] = stringThingy[right] 
        stringThingy[right] = temp
        left += 1
        right -= 1
    }

    Deno.writeTextFileSync("thingy.txt", stringThingy.join(""))

    // get checksum
    let acc = 0

    for (const [idx, n] of stringThingy.entries()) {
        if (n !== ".") {
            acc += Number(n) * Number(idx)
        }
    }

    console.log(acc)
}

function main() {
    const input = [...Deno.readTextFileSync("./input.txt")].map(x => Number(x))
    part1(input)
}

main()

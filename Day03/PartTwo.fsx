open System

let collectNumber x (line: string) =
    line
    |> Seq.skip x
    |> Seq.takeWhile Char.IsDigit
    |> Seq.toArray
    |> String

let parseLine (line: string) =
    let rec imp (numbers, gears) y x (line: string) =
        if x = line.Length then
            List.rev numbers, List.rev gears
        else if Char.IsDigit(line[x]) then
            let number = collectNumber x line
            let newAcc = ((x, y), number) :: numbers, gears
            let newX = x + number.Length
            imp newAcc y newX line
        else if line[x] = '*' then
            let newAcc = numbers, (x, y) :: gears
            imp newAcc y (x + 1) line
        else
            imp (numbers, gears) y (x + 1) line

    fun y -> imp ([], []) y 0 line

let getGearBorder (x, y) = (x - 1, y - 1), (x + 1, y + 1)

let isBetween (x1, x2) x = x1 <= x && x <= x2

let solve lines =
    let numbers, gears =
        lines
        |> Seq.indexed
        |> Seq.map (fun (y, line) -> parseLine line y)
        |> Seq.fold
            (fun (numbersSoFar, gearsSoFar) (numbers, gears) -> numbersSoFar @ numbers, gearsSoFar @ gears)
            ([], [])

    gears
    |> List.sumBy
        (fun (gx, gy) ->
            let (x1, y1), (x2, y2) = getGearBorder (gx, gy)
            let partNumbers =
                numbers
                |> List.filter
                    (fun ((x, y), number) ->
                        let (nx1, nx2) = x, x + number.Length - 1
                        isBetween (y1, y2) y && (isBetween (x1, x2) nx1 || isBetween (x1, x2) nx2))

            if partNumbers.Length = 2 then
                let gearRatio = partNumbers |> List.map (snd >> int64) |> List.reduce (*)
                gearRatio
            else
                0)

let input = @"467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."

input.Split("\n") |> solve

open System.IO

File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
|> solve
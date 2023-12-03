open System

let collectNumber x (line: string) =
    line
    |> Seq.skip x
    |> Seq.takeWhile Char.IsDigit
    |> Seq.toArray
    |> String

collectNumber 0 "467..114.."
collectNumber 5 "467..114.."
collectNumber 3 "467..114.."

let parseLine (line: string) =
    let rec imp (numbers, symbols) y x (line: string) =
        if x = line.Length then
            List.rev numbers, List.rev symbols
        else if Char.IsDigit(line[x]) then
            let number = collectNumber x line
            let newAcc = ((x, y), number) :: numbers, symbols
            let newX = x + number.Length
            imp newAcc y newX line
        else if line[x] = '.' then
            imp (numbers, symbols) y (x + 1) line
        else
            let newAcc = numbers, (x, y) :: symbols
            imp newAcc y (x + 1) line

    fun y -> imp ([], []) y 0 line

parseLine ".....2" 0
parseLine "467..114.." 0 // [ ((0,0), "467"); ((5,0), "114") ]
parseLine "...*......" 1 // [ ((3,1), "*") ]
parseLine "..35..633." 2 // [ ((2,2), "35"); ((6,2), "633")]
parseLine "........................313............444....................................662..........................956.....714...................935" 140

let getBorder (number: string) (x, y) =
    (x - 1, y - 1), (x + number.Length, y + 1)

getBorder "467" (0, 0)
getBorder "35" (2, 2)
getBorder "633" (6, 2)

let isBetween (x1, x2) x =
    x1 <= x && x <= x2

let adjacentToSymbol number (x, y) =
    let (x1, y1), (x2, y2) = getBorder number (x, y)
    fun (sx, sy) ->
        isBetween (x1, x2) sx && isBetween (y1, y2) sy

let solve lines =
    let numbers, symbols =
        lines
        |> Seq.indexed
        |> Seq.map (fun (y, line) -> parseLine line y)
        |> Seq.fold
            (fun (numbersSoFar, symbolsSoFar) (numbers, symbols) -> numbersSoFar @ numbers, symbolsSoFar @ symbols)
            ([], [])

    numbers
    |> List.filter (fun ((x, y), number) -> symbols |> List.exists (adjacentToSymbol number (x, y)))
    |> List.sumBy (snd >> int)

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
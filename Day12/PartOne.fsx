let (|D|O|U|) =
    function
    | '.' -> O
    | '#' -> D
    | '?' -> U
    | chr -> failwithf "Invalid character '%c'" chr

let rec nextContiguousGroup springs =
    match springs with
    | O :: left -> nextContiguousGroup left
    | _ -> springs

let countIfNotAnyDamaged left =
    if List.contains '#' left |> not then 1 else 0

let rec arrangements damagedSprings springs =
    let rec imp d ds springs =
        match d, springs, ds with
        | 0, _, _ -> invalidOp "Should never happen"
        | _, [], _ -> 0
        | _, O :: _, _ -> 0
        | 1, [ D ], [] -> 1
        | 1, [ D ], _ -> 0
        | 1, D :: D :: _, _ -> 0
        | 1, D :: O :: left, [] -> countIfNotAnyDamaged left
        | 1, D :: U :: left, [] -> countIfNotAnyDamaged left
        | 1, D :: O :: left, _ -> arrangements ds left
        | 1, D :: U :: left, _ -> arrangements ds left
        | 1, [ U ], [] -> 1
        | 1, [ U ], _ -> 0
        | 1, U :: D :: _, _ -> 0
        | 1, U :: O :: left, [] -> countIfNotAnyDamaged left
        | 1, U :: U :: left, [] -> countIfNotAnyDamaged left
        | 1, U :: O :: left, _ -> arrangements ds left
        | 1, U :: U :: left, _ -> arrangements ds left
        | _, D :: left, _ -> imp (d - 1) ds left
        | _, U :: left, _ -> imp (d - 1) ds left

    match damagedSprings, springs with
    | [], _ -> 0
    | _, [] -> 0
    | _, O :: left -> arrangements damagedSprings (nextContiguousGroup left)
    | d :: ds, D :: _ -> imp d ds springs
    | d :: ds, U :: left -> imp d ds springs + arrangements damagedSprings left

let parse (line: string) =
    let [| springs; damagedSpringsPart |] = line.Split(" ")
    let damagedSprings =
        damagedSpringsPart.Split(",") |> Seq.map int |> Seq.toList

    Seq.toList springs, damagedSprings

let solvePartOne lines =
    lines
    |> Seq.sumBy (fun (springs, damagedSprings) -> arrangements damagedSprings springs)

let sample = @"???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"

sample.Split("\n") |> Seq.map parse |> solvePartOne

open System.IO

#time
File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
|> Seq.map parse
|> solvePartOne
#time
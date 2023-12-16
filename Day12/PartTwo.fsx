open System
open System.IO

let rec collectDamagedOrUnknown count springs =
    if count = 0 then
        Some springs
    else
        match springs with
        | '#' :: others -> collectDamagedOrUnknown (count - 1) others
        | '?' :: others -> collectDamagedOrUnknown (count - 1) others
        | _ -> None

let rec containsDamaged count springs =
    if count = 0 then
        false
    else
        match springs with
        | [] -> false
        | '#' :: _ -> true
        | '?' :: left -> containsDamaged (count - 1) left
        | _ -> invalidOp "Should not happen"

let collectCandidates damagedSprings springs =
    let rec imp acc damagedSprings springs =
        // printfn " %i, %s" damagedSprings (springs |> List.toArray |> String)

        match springs with
        | [] -> acc
        | '.' :: left -> imp acc damagedSprings left
        | '#' :: _ ->
            match collectDamagedOrUnknown damagedSprings springs with
            | Some [] -> [] :: acc
            | Some ('#' :: _) -> acc
            | Some ('?' :: xs) -> xs :: acc
            | Some ('.' :: xs) -> xs :: acc
            | None -> acc
        | '?' :: left ->
            match collectDamagedOrUnknown damagedSprings springs with
            | Some [] -> [] :: acc
            | Some ('#' :: _) -> imp acc damagedSprings left
            | Some ('?' :: xs) -> imp (xs :: acc) damagedSprings left
            | Some ('.' :: xs) when containsDamaged damagedSprings springs -> xs :: acc
            | Some ('.' :: xs) -> imp (xs :: acc) damagedSprings xs
            | None -> imp acc damagedSprings left

    imp [] damagedSprings springs |> List.rev

let arrangements damagedSpringCounts springs =
    let rec imp (acc: char list list) damagedSpringCounts =
        printfn "%A, %A" damagedSpringCounts (acc |> List.map (List.toArray >> String))

        match damagedSpringCounts with
        | [] -> 0
        | d :: [] ->
            acc
            |> List.sumBy (
                collectCandidates d
                >> List.filter (List.contains '#' >> not)
                >> List.length)
        | d :: ds ->
            let newAcc = acc |> List.collect (collectCandidates d)
            imp newAcc ds

    imp [ springs ] damagedSpringCounts

let parse (line: string) =
    let [| springs; damagedSpringsPart |] = line.Split(" ")

    let damagedSprings =
        damagedSpringsPart.Split(",")
        |> Seq.map int
        |> Seq.toList

    Seq.toList springs, damagedSprings

let solvePartOne lines =
    lines
    |> Seq.sumBy (fun (springs, damagedSprings) -> arrangements damagedSprings springs)

#time
"???.###????.###????.###????.###????.### 1,1,3,1,1,3,1,1,3,1,1,3,1,1,3"
|> parse
|> fun (springs, damagedSprings) -> arrangements damagedSprings springs
#time

let sample =
    @"???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"

sample.Split("\n")
|> Seq.map parse
|> solvePartOne

#time

File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
|> Seq.map parse
|> solvePartOne

#time
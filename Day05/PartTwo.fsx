open System

type Map = { Source: string; Destination: string }

type Description =
    { DestinationRangeStart: int64
      SourceRangeStart: int64
      RangeLength: int64 }

type Range = { Start: int64; End: int64 }

let create startValue endValue = { Start = startValue; End = endValue }

let isBetween range value =
    if value < range.Start then -1
    else if range.Start <= value && value <= range.End then 0
    else 1

let move increment range =
    { Start = range.Start + increment; End = range.End + increment }

let merge first second =
    match isBetween first second.Start, isBetween first second.End with
    | -1, -1 -> None
    | -1, 0 -> Some (create second.Start (max first.End second.End))
    | -1, 1 -> Some second
    | 0, 0 -> Some (create (min first.Start second.Start) (max first.End second.End))
    | 0, 1 -> Some (create (min first.Start second.Start) second.End)
    | 1, _ -> None
    | _ -> failwithf "Could not handle merge for %A and %A" first second

let rec mergeRanges ranges =
    let rec imp range ranges =
        let merged, unmerged =
            ranges
            |> List.fold
                (fun (rangeSoFar, unmerged) range ->
                    match merge rangeSoFar range with
                    | Some merged -> merged, unmerged
                    | None -> rangeSoFar, range :: unmerged)
                (range, [])

        match unmerged with
        | [] -> [ merged ]
        | r :: rs -> merged :: imp r rs

    match ranges with
    | r :: rs -> imp r rs 
    | [] -> []

let splitUp first second =
    match isBetween first second.Start, isBetween first second.End with
    | -1, -1 -> None, [ second ]
    | -1, 0 -> Some (create first.Start second.End), [ create second.Start (first.Start - 1L) ]
    | -1, 1 -> Some first, [ create second.Start (first.Start - 1L); create (first.End + 1L) second.End ]
    | 0, 0 -> Some second, []
    | 0, 1 -> Some (create second.Start first.End), [ create (first.End + 1L) second.End ]
    | 1, _ -> None, [ second ]
    | _ -> failwithf "Could not handle split up for %A and %A" first second

let splitUpRange description range =
    let sourceRange = create description.SourceRangeStart (description.SourceRangeStart + description.RangeLength - 1L)
    let overlapped, left = splitUp sourceRange range
    let increment = description.DestinationRangeStart - description.SourceRangeStart
    let moved = overlapped |> Option.map (move increment)
    moved, left

let rec splitUpRanges ranges descriptions =
    match descriptions with
    | d :: ds ->
        let moved, left =
            ranges
            |> List.map (splitUpRange d)
            |> List.fold
                (fun (movedSoFar, leftSoFar) (moved, left) ->
                    let newMoved =
                        moved
                        |> Option.map (fun r -> r :: movedSoFar)
                        |> Option.defaultValue movedSoFar

                    let newLeft = left @ leftSoFar
                    newMoved, newLeft)
                ([], [])

        moved @ (splitUpRanges (mergeRanges left) ds)
    | [] -> ranges

let parseSeeds (input: string) =
    input.Split(":")
    |> Array.last
    |> _.Split(" ", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int64

let parseMapping (input: string) =
    let lines = input.Split("\n")
    let [| source; _; destination |] = (lines[0].Split(" ")[0]).Split("-")
    let map = { Source = source; Destination = destination }
    let ranges =
        lines[1..]
        |> Seq.map
            (fun line ->
                let [| destinationRangeStart; sourceRangeStart; rangeLength |] =
                    line.Split(" ") |> Array.map int64
                
                { DestinationRangeStart = destinationRangeStart
                  SourceRangeStart = sourceRangeStart
                  RangeLength = rangeLength })
        |> Seq.toList

    map, ranges

let parse (input: string) =
    let parts = input.Split(String.replicate 2 "\n")
    let seeds = parseSeeds parts[0]
    let mappings =
        parts[1..]
        |> Seq.map parseMapping
        |> Map.ofSeq

    seeds, mappings

let step mappings source ranges =
    let map = mappings |> Map.findKey (fun map _ -> map.Source = source)
    let descriptions = Map.find map mappings
    let newRanges = splitUpRanges ranges descriptions

    map.Destination, newRanges

let lowestLocationNumber mappings range =
    let _, ranges =
        ("seed", [ range ])
        ||> step mappings
        ||> step mappings
        ||> step mappings
        ||> step mappings
        ||> step mappings
        ||> step mappings
        ||> step mappings
    
    ranges |> List.map _.Start |> List.min

let solve input =
    let seeds, mappings = parse input
    let initialSeedNumbers =
        seeds
        |> Seq.toList
        |> List.chunkBySize 2
        |> List.map (fun [ start; length ] -> create start (start + length - 1L))

    initialSeedNumbers
    |> List.map (lowestLocationNumber mappings)
    |> List.min

let input = @"seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"

solve input

open System.IO

#time
File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
|> solve
#time
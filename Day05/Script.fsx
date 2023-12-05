open System

type Map = { Source: string; Destination: string }

type Range =
    { DestinationRangeStart: int64
      SourceRangeStart: int64
      RangeLength: int64 }

let isBetween (rangeStart, rangeEnd) value =
    rangeStart <= value && value <= rangeEnd

let correspondsTo range value =
    let sourceRangeEnd = range.SourceRangeStart + range.RangeLength - 1L

    if isBetween (range.SourceRangeStart, sourceRangeEnd) value then
        let diff = value - range.SourceRangeStart
        Some (range.DestinationRangeStart + diff)
    else
        None

correspondsTo { DestinationRangeStart = 50; SourceRangeStart = 98; RangeLength = 2 } 80
correspondsTo { DestinationRangeStart = 52; SourceRangeStart = 50; RangeLength = 48 } 53

let parseSeeds (input: string) =
    input.Split(":")
    |> Array.last
    |> _.Split(" ", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int64

parseSeeds "seeds: 79 14 55 13"

let parseMapping (newLine: string) (input: string) =
    let lines = input.Split(newLine)
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

parseMapping "\n" @"seed-to-soil map:
50 98 2
52 50 48"

let parse (newLine: string) (input: string) =
    let parts = input.Split(String.replicate 2 newLine)
    let seeds = parseSeeds parts[0]
    let mappings =
        parts[1..]
        |> Seq.map (parseMapping newLine)
        |> Map.ofSeq

    seeds, mappings

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

parse input

let findLocation mappings seed =
    let rec imp mappings source value =
        match Map.tryFindKey (fun map _ -> map.Source = source) mappings with
        | Some mapping ->
            let ranges = Map.find mapping mappings
            let result =
                ranges
                |> List.tryPick (fun range -> correspondsTo range value)
                |> Option.defaultValue value

            imp mappings mapping.Destination result
        | None -> value

    imp mappings "seed" seed

let solve newLine input =
    let seeds, mappings = parse newLine input
    
    seeds
    |> Seq.map (findLocation mappings)
    |> Seq.min

solve "\n" input

open System.IO

File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
|> solve Environment.NewLine
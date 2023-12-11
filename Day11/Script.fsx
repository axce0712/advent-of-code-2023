open System

type GalaxyPosition = { mutable X: int64; mutable Y: int64 }

type Pair = { Source: GalaxyPosition; Destination: GalaxyPosition }

let parseGalaxies (input: string) =
    input.Split("\n")
    |> Seq.indexed
    |> Seq.collect
        (fun (y, line) ->
            line
            |> Seq.indexed
            |> Seq.choose (fun (x, c) -> if c = '#' then Some { X = x; Y = y } else None))
    |> Seq.toList

let expandHorizontal increment galaxies =
    let maxX =
        galaxies
        |> List.map (fun galaxy -> galaxy.X)
        |> List.max

    for x in maxX .. -1L .. 0 do
        if List.forall (fun galaxy -> galaxy.X <> x) galaxies then
            galaxies
            |> List.filter (fun galaxy -> galaxy.X > x)
            |> List.iter (fun galaxy -> galaxy.X <- galaxy.X + increment - 1L)

let expandVertical increment galaxies =
    let maxY =
        galaxies
        |> List.map (fun galaxy -> galaxy.Y)
        |> List.max

    for y in maxY .. -1L .. 0 do
        if List.forall (fun galaxy -> galaxy.Y <> y) galaxies then
             galaxies
            |> List.filter (fun galaxy -> galaxy.Y > y)
            |> List.iter (fun galaxy -> galaxy.Y <- galaxy.Y + increment - 1L)

let expand increment galaxies =
    expandHorizontal increment galaxies
    expandVertical increment galaxies
    
    galaxies

let pairs galaxies =
    let rec imp acc galaxies =
        match galaxies with
        | [] -> List.rev acc
        | g1 :: gs ->
            let newPairs = gs |> List.map (fun g2 -> { Source = g1; Destination = g2 })
            let newAcc = newPairs @ acc
            imp newAcc gs
    
    imp [] galaxies

let path pair = abs (pair.Destination.X - pair.Source.X) + abs (pair.Destination.Y - pair.Source.Y)

let solve increment galaxies =
    let pairs =
        galaxies |> expand increment |> pairs

    pairs |> List.sumBy path

let solvePartOne galaxies = solve 2 galaxies

let solvePartTwo galaxies = solve 1_000_000 galaxies

let sample = @"...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."

parseGalaxies sample |> solvePartOne

open System.IO

let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))

#time
input |> parseGalaxies |> solvePartOne
#time

parseGalaxies sample |> solve 10
parseGalaxies sample |> solve 100

#time
input |> parseGalaxies |> solvePartTwo
#time
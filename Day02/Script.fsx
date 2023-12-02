
type Cube = Red | Green | Blue

type Set = Set of Cube list

type Game = { Id: int; Sets: Set list }

let parseCube =
    function
    | "blue" -> Blue
    | "red" -> Red
    | "green" -> Green
    | invalid -> failwithf "Cannot parse cube with value '%s'" invalid

let parseCubes (input: string) =
    let [| countPart; cubePart |] = input.Split(" ")
    List.replicate (int countPart) (parseCube cubePart)
    
parseCubes "3 blue"

let parseSet (input: string) =
    input.Split(", ")
    |> Seq.collect parseCubes
    |> Seq.toList

parseSet "3 blue, 4 red"

let parseGame (input: string) =
    let [| idPart; setsPart |] = input.Split(": ")
    let id = idPart.Split(" ") |> Seq.last |> int
    let sets =
        setsPart.Split("; ")
        |> Seq.map (parseSet >> Set)
        |> Seq.toList

    { Id = id; Sets = sets }

let wouldBePossible (redCount, greenCount, blueCount) (game: Game) : bool =
    let mapping cube =
        let maxCount =
            game.Sets
            |> List.map (fun (Set cubes) -> cubes |> List.filter ((=) cube) |> List.length)
            |> List.max

        cube, maxCount
    
    let counted =
        [ Red; Green; Blue ]
        |> List.map mapping
        |> Map.ofSeq

    counted[Red] <= redCount
    && counted[Green] <= greenCount
    && counted[Blue] <= blueCount

parseGame "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
|> wouldBePossible (12, 13, 14)

let solvePartOne (redCount, greenCount, blueCount) games =
    games
    |> Seq.sumBy (fun game ->
        if wouldBePossible (redCount, greenCount, blueCount) game then
            game.Id
        else
            0)

let input = @"Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

input.Split("\n")
|> Seq.map parseGame
|> solvePartOne (12, 13, 14)

open System.IO

let lines = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
let games = lines |> Seq.map parseGame |> Seq.toList

solvePartOne (12, 13, 14) games

let fewestNumberOfCubes game =
    [ Red; Green; Blue ]
    |> List.map (fun cube ->
        let fewestNumberOfCubes =
            game.Sets
            |> List.map (fun (Set cubes) -> cubes |> List.filter ((=) cube) |> List.length)
            |> List.max

        cube, fewestNumberOfCubes)
    |> Map.ofSeq

let power =
    Map.fold (fun acc _ number -> acc * number) 1

let solvePartTwo games =
    games
    |> List.sumBy (fewestNumberOfCubes >> power)

solvePartTwo games
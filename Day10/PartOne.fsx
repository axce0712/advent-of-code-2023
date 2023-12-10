open System.IO

(*
The pipes are arranged in a two-dimensional grid of tiles:

| is a vertical pipe connecting north and south.
- is a horizontal pipe connecting east and west.
L is a 90-degree bend connecting north and east.
J is a 90-degree bend connecting north and west.
7 is a 90-degree bend connecting south and west.
F is a 90-degree bend connecting south and east.
. is ground; there is no pipe in this tile.
S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
*)

module Array2D =
    let tryFindIndex predicate (arr2D: _ [,]) =
        let rec imp x y =
            if predicate arr2D[y, x] then
                Some(x, y)
            else if x < Array2D.length2 arr2D - 1 then
                imp (x + 1) y
            else if y < Array2D.length1 arr2D - 1 then
                imp 0 (y + 1)
            else
                None

        imp 0 0

let canConnectNorth =
    function
    | '|'
    | '7'
    | 'F' -> true
    | _ -> false

let canConnectWest =
    function
    | '-'
    | 'L'
    | 'F' -> true
    | _ -> false

let canConnectSouth =
    function
    | '|'
    | 'J'
    | 'L' -> true
    | _ -> false

let canConnectEast =
    function
    | '-'
    | 'J'
    | '7' -> true
    | _ -> false

let tryMoveWest (x, y) = if x > 0 then Some(x - 1, y) else None

let tryMoveNorth (x, y) = if y > 0 then Some(x, y - 1) else None

let getPipe (x, y) (area: char [,]) = area[y, x]

let tryMoveEast (x, y) (area: char [,]) =
    if x < Array2D.length2 area - 1 then
        Some(x + 1, y)
    else
        None

let tryMoveSouth (x, y) (area: char [,]) =
    if y < Array2D.length1 area - 1 then
        Some(x, y + 1)
    else
        None

let noneIfVisited =
    function
    | 'V' -> None
    | chr -> Some chr

let tryWest (x, y) (area: char [,]) =
    tryMoveWest (x, y)
    |> Option.bind (fun pos ->
        getPipe pos area
        |> noneIfVisited
        |> Option.bind (fun pipe ->
            if canConnectWest pipe then
                Some pos
            else
                None))

let tryNorth (x, y) (area: char [,]) =
    tryMoveNorth (x, y)
    |> Option.bind (fun pos ->
        getPipe pos area
        |> noneIfVisited
        |> Option.bind (fun pipe ->
            if canConnectNorth pipe then
                Some pos
            else
                None))

let tryEast (x, y) (area: char [,]) =
    tryMoveEast (x, y) area
    |> Option.bind (fun pos ->
        getPipe pos area
        |> noneIfVisited
        |> Option.bind (fun pipe ->
            if canConnectEast pipe then
                Some pos
            else
                None))

let trySouth (x, y) (area: char [,]) =
    tryMoveSouth (x, y) area
    |> Option.bind (fun pos ->
        getPipe pos area
        |> noneIfVisited
        |> Option.bind (fun pipe ->
            if canConnectSouth pipe then
                Some pos
            else
                None))

let nextDirections =
    function
    | '|' -> [ tryNorth; trySouth ]
    | '-' -> [ tryWest; tryEast ]
    | 'L' -> [ tryNorth; tryEast ]
    | 'J' -> [ tryWest; tryNorth ]
    | '7' -> [ tryWest; trySouth ]
    | 'F' -> [ tryEast; trySouth ]
    | 'S' -> [ tryWest; tryNorth; tryEast; trySouth ]
    | chr -> failwithf "Unexpected value %c" chr

let step (area: char [,]) (x, y) =
    getPipe (x, y) area
    |> nextDirections
    |> List.choose (fun f -> f (x, y) area)

let steps (area: char [,]) positions =
    positions
    |> List.map (step area)
    |> List.reduce (@)

let markVisited (area: char [,]) (x, y) = area[y, x] <- 'V'

let solvePartOne (area: char [,]) =
    let rec imp area positions value =
        match steps area positions with
        | [] -> value
        | newPositions ->
            positions |> List.iter (markVisited area)
            imp area newPositions (value + 1)

    let area = Array2D.copy area
    let sx, sy = Array2D.tryFindIndex ((=) 'S') area |> Option.get
    imp area [ sx, sy ] 0

let sample =
    @"..F7.
.FJ|.
SJ.L7
|F--J
LJ..."

let area = array2D (sample.Split("\n"))

solvePartOne area

#time
File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
|> array2D
|> solvePartOne
#time

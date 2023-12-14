open System.IO

type Direction =
    | West
    | North
    | East
    | South

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

let nextDirection origin pile =
    match origin, pile with
    | East, '-' -> West
    | East, 'L' -> North
    | East, 'F' -> South
    | South, '|' -> North
    | South, '7' -> West
    | South, 'F' -> East
    | West, '-' -> East
    | West, 'J' -> North
    | West, '7' -> South
    | North, '|' -> South
    | North, 'J' -> West
    | North, 'L' -> East
    | _ -> failwithf "Invalid sequence of origin '%A' and pile '%c'" origin pile

let inverseDirection =
    function
    | West -> East
    | North -> South
    | East -> West
    | South -> North

let move direction (x, y) =
    match direction with
    | West -> x - 1, y
    | North -> x, y - 1
    | East -> x + 1, y
    | South -> x, y + 1

let tryGetElement (area: char [,]) (x, y) =
    if x < 0
       || y < 0
       || y >= Array2D.length1 area
       || x >= Array2D.length2 area then
        None
    else
        Array2D.get area y x |> Some

let getElement (area: char [,]) (x, y) = Array2D.get area y x

let step (area: char [,]) (origin, pos) =
    let newDirection =
        getElement area pos |> nextDirection origin

    inverseDirection newDirection, move newDirection pos

let steps (area: char [,]) directions = directions |> List.map (step area)

let choosePipeWest area (x, y) =
    match tryGetElement area (x - 1, y) with
    | Some '-'
    | Some 'L'
    | Some 'F' -> Some(East, (x - 1, y))
    | _ -> None

let choosePipeNorth area (x, y) =
    match tryGetElement area (x - 1, y) with
    | Some '|'
    | Some '7'
    | Some 'L' -> Some(South, (x, y - 1))
    | _ -> None

let choosePipeEast area (x, y) =
    match tryGetElement area (x + 1, y) with
    | Some '-'
    | Some 'J'
    | Some '7' -> Some(West, (x + 1, y))
    | _ -> None

let choosePipeSouth area (x, y) =
    match tryGetElement area (x, y + 1) with
    | Some '|'
    | Some 'J'
    | Some 'L' -> Some(North, (x, y + 1))
    | _ -> None

let getDirections area (x, y) =
    [ choosePipeWest area
      choosePipeNorth area
      choosePipeEast area
      choosePipeSouth area ]
    |> List.choose ((|>) (x, y))

let solvePartOne (area: char [,]) =
    let rec imp area positions value =
        match steps area positions with
        | newPositions when
            newPositions
            |> Seq.map snd
            |> Seq.pairwise
            |> Seq.forall (fun (p1, p2) -> p1 = p2)
            ->
            value
        | newPositions -> imp area newPositions (value + 1)

    let area = Array2D.copy area
    let sx, sy = Array2D.tryFindIndex ((=) 'S') area |> Option.get
    let startPositions = getDirections area (sx, sy)
    imp area startPositions 0

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

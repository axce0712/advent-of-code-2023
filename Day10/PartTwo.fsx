open System.IO
open System

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

let getElement (area: char [,]) (x, y) =
    Array2D.get area y x

let tryGetElement (area: char [,]) (x, y) =
    if x < 0
       || y < 0
       || y >= Array2D.length1 area
       || x >= Array2D.length2 area then
        None
    else
        Array2D.get area y x |> Some

let rec findWestCorner area (x, y) =
    match getElement area (x, y) with
    | '-' -> findWestCorner area (x - 1, y)
    | 'L' | 'J' | 'F' | '7' -> (x, y)
    | chr -> invalidOp (sprintf "Unable to find left corner for '%c'" chr)

let rec findEastCorner area (x, y) =
    match getElement area (x, y) with
    | '-' -> findEastCorner area (x + 1, y)
    | 'L' | 'J' | 'F' | '7' -> (x, y)
    | chr -> invalidOp (sprintf "Unable to find right corner for '%c'" chr)

let rec findNorthCorner area (x, y) =
    match getElement area (x, y) with
    | '|' -> findNorthCorner area (x, y - 1)
    | 'L' | 'J' | 'F' | '7' -> (x, y)
    | chr -> invalidOp (sprintf "Unable to find north corner for '%c'" chr)

let rec findSouthCorner area (x, y) =
    match getElement area (x, y) with
    | '|' -> findSouthCorner area (x, y + 1)
    | 'L' | 'J' | 'F' | '7' -> (x, y)
    | chr -> invalidOp (sprintf "Unable to find south corner for '%c'" chr)

let rec hitWest area (x, y) =
    match tryGetElement area (x, y) with
    | Some '|' ->
        let northCorner = findNorthCorner area (x, y)
        let southCorner = findSouthCorner area (x, y)

        match getElement area northCorner, getElement area southCorner with
        | 'F', 'L' -> Some 1
        | '7', _ -> hitWest area northCorner
        | _, 'J' -> hitWest area southCorner
        | nc, sc -> invalidOp (sprintf "Unable to handle north corner '%c' (%A) and southCorner corner '%c' (%A)" nc northCorner sc southCorner)
    | Some _ -> hitWest area (x - 1, y)
    | None -> None

let rec hitNorth area (x, y) =
    match tryGetElement area (x, y) with
    | Some '-' ->
        let westCorner = findWestCorner area (x, y)
        let eastCorner = findEastCorner area (x, y)

        match getElement area westCorner, getElement area eastCorner with
        | 'F', '7' -> Some 1
        | 'L', _ -> hitNorth area westCorner
        | _, 'J' -> hitNorth area eastCorner
        | lc, rc -> invalidOp (sprintf "Unable to handle left corner '%c' (%A) and right corner '%c' (%A)" lc westCorner rc eastCorner)
    | Some _ -> hitNorth area (x, y - 1)
    | None -> None

let rec hitEast area (x, y) =
    match tryGetElement area (x, y) with
    | Some '|' ->
        let northCorner = findNorthCorner area (x, y)
        let southCorner = findSouthCorner area (x, y)

        match getElement area northCorner, getElement area southCorner with
        | '7', 'J' -> Some 1
        | 'F', _ -> hitEast area northCorner
        | _, 'L' -> hitEast area southCorner
        | nc, sc -> invalidOp (sprintf "Unable to handle north corner '%c' (%A) and southCorner corner '%c' (%A)" nc northCorner sc southCorner)
    | Some _ -> hitEast area (x + 1, y)
    | None -> None
let rec hitSouth area (x, y) =
    match tryGetElement area (x, y) with
    | Some '-' ->
        let westCorner = findWestCorner area (x, y)
        let eastCorner = findEastCorner area (x, y)

        match getElement area westCorner, getElement area eastCorner with
        | 'L', 'J' -> Some 1
        | 'F', _ -> hitSouth area westCorner
        | _, '7' -> hitSouth area eastCorner
        | lc, rc -> invalidOp (sprintf "Unable to handle left corner '%c' (%A) and right corner '%c' (%A)" lc westCorner rc eastCorner)
    | Some _ -> hitSouth area (x, y + 1)
    | None -> None

let fillArea area =
    area
    |> Array2D.mapi (fun y x ->
        function
        | '.' ->
            let hits =
                [ hitWest area (x, y)
                  hitNorth area (x, y)
                  hitEast area (x, y)
                  hitSouth area (x, y) ]
                |> List.reduce (Option.map2 (+))

            hits
            |> Option.map (fun h -> if h % 2 = 0 then 'I' else '0')
            |> Option.defaultValue 'O'
        | chr -> chr)

let sample =
    @".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJF7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ..."

let area = array2D (sample.Split("\n"))
area[1, 1] <- 'F'

hitSouth area (2, 6)
hitWest area (2, 6)
hitNorth area (2, 6)
hitNorth area (0, 6)
hitWest area (2, 8)

let printArea (area: char [,]) =
    "\n"
    + String.concat "\n" [ 
        for row in 0..Array2D.length1 area-1  do
            yield area.[row,*] |> String
    ]

fillArea area |> printArea
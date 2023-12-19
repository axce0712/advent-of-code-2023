let totalLoad platform =
    let mutable score = 0
    let lengthY = Array2D.length1 platform

    for x in 0 .. Array2D.length2 platform - 1 do
        let mutable offsetY = 0

        for y in 0 .. lengthY - 1 do
            match Array2D.get platform y x with
            | '#' -> offsetY <- y + 1
            | 'O' ->
                score <- score + (lengthY - offsetY)
                offsetY <- offsetY + 1
            | _ -> ()

    score

let sample =
    @"O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."

#time
sample.Split("\n") |> array2D |> totalLoad
#time

open System.IO

#time

File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
|> array2D
|> totalLoad

#time

let calculateLoad n l = n * l - (l - (l + 1) % 2) * (l / 2)

let totalLoad platform =
    let rec imp platform loadSoFar countSoFar y x =
        if x < 0 then
            loadSoFar
        else if y < 0 then
            let newLoad =
                loadSoFar
                + calculateLoad (Array2D.length1 platform - y - 1) countSoFar

            imp platform newLoad 0 (Array2D.length1 platform - 1) (x - 1)
        else
            match Array2D.get platform y x with
            | '#' ->
                let newLoad =
                    loadSoFar
                    + calculateLoad (Array2D.length1 platform - y - 1) countSoFar

                imp platform newLoad 0 (y - 1) x
            | 'O' -> imp platform loadSoFar (countSoFar + 1) (y - 1) x
            | '.' -> imp platform loadSoFar countSoFar (y - 1) x
            | chr -> failwithf $"Unknown character '%c{chr}'"

    imp platform 0 0 (Array2D.length1 platform - 1) (Array2D.length2 platform - 1)

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

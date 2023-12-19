open System
open System.Collections.Generic

let north platform =
    for x in 0 .. Array2D.length2 platform - 1 do
        let mutable offsetY = 0

        for y in 0 .. Array2D.length1 platform - 1 do
            match Array2D.get platform y x with
            | '#' -> offsetY <- y + 1
            | 'O' ->
                Array2D.set platform y x '.'
                Array2D.set platform offsetY x 'O'
                offsetY <- offsetY + 1
            | _ -> ()

let west platform =
    for y in 0..Array2D.length1 platform - 1 do
        let mutable offsetX = 0

        for x in 0..Array2D.length2 platform - 1 do
            match Array2D.get platform y x with
            | '#' -> offsetX <- x + 1
            | 'O' ->
                Array2D.set platform y x '.'
                Array2D.set platform y offsetX 'O'
                offsetX <- offsetX + 1
            | _ -> ()

let south platform =
    let maxY = Array2D.length1 platform - 1
    for x in 0 .. Array2D.length2 platform - 1 do
        let mutable offsetY = maxY

        for y in maxY .. -1 .. 0 do
            match Array2D.get platform y x with
            | '#' -> offsetY <- y - 1
            | 'O' ->
                Array2D.set platform y x '.'
                Array2D.set platform offsetY x 'O'
                offsetY <- offsetY - 1
            | _ -> ()

let east platform =
    let maxX = Array2D.length2 platform - 1
    for y in 0..Array2D.length1 platform - 1 do
        let mutable offsetX = maxX

        for x in maxX .. -1 .. 0 do
            match Array2D.get platform y x with
            | '#' -> offsetX <- x - 1
            | 'O' ->
                Array2D.set platform y x '.'
                Array2D.set platform y offsetX 'O'
                offsetX <- offsetX - 1
            | _ -> ()

let cycle platform =
    north platform
    west platform
    south platform
    east platform

let toString platform =
    platform |> Seq.cast<char> |> Seq.toArray |> String

let findRepetition platform =
    let current = Array2D.copy platform
    let cache = Dictionary<string, int>()
    let mutable tries = 0
    let mutable key = toString current
    while not <| cache.ContainsKey(key) do
        cache.Add(key, tries)
        cycle current
        key <- toString current
        tries <- tries + 1

    current, cache[key], tries

let totalLoad platform =
    let mutable score = 0
    let lengthY = Array2D.length1 platform

    for x in 0 .. Array2D.length2 platform - 1 do
        for y in 0 .. lengthY - 1 do
            match Array2D.get platform y x with
            | '#' -> ()
            | 'O' -> score <- score + (lengthY - y)
            | _ -> ()

    score

let solvePartTwo totalCycles platform =
    let current, repeatStart, repeatEnd = findRepetition platform
    let repetitionLength = repeatEnd - repeatStart
    let remainingCycles = (totalCycles - repeatStart) % repetitionLength
    for _ in 1..remainingCycles do
        cycle current

    totalLoad current

let sample = @"O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."
    
let platform = sample.Split("\n") |> array2D
solvePartTwo 1_000_000_000 platform

open System.IO

#time

File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
|> array2D
|> solvePartTwo 1_000_000_000

#time
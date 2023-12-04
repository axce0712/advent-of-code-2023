open System

let parseCards (input: string) =
    let [| winningNumbersPart; availableNumbersPart |] =
        (input.Split(": ") |> Array.last).Split("|")
        |> Array.map (_.Trim() >> _.Split(' ', StringSplitOptions.RemoveEmptyEntries) >> Array.map int)

    Set.ofArray winningNumbersPart, Set.ofArray availableNumbersPart

let cardValue (winningNumbers, availableNumbers) =
    let winning = Set.intersect availableNumbers winningNumbers
    let cardValue =
        if Set.isEmpty winning then
            0
        else
            List.replicate (winning.Count - 1) 2 |> List.fold (*) 1
    
    cardValue

parseCards "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53" |> cardValue
parseCards "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36" |> cardValue

let solve entries =
    entries |> Seq.sumBy cardValue

let input = @"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

input.Split("\n") |> Array.map parseCards |> solve

open System.IO

File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
|> Seq.map parseCards
|> solve
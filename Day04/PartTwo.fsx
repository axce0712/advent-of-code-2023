open System

type Card =
    {
        Id: int
        WinningNumbers: Set<int>
        AvailableNumbers: Set<int>
    }

let parseCards (input: string) =
    let [| cardPart; numbersPart |] = input.Split(":")
    let [| winningNumbersPart; availableNumbersPart |] =
        numbersPart.Split("|")
        |> Array.map (_.Trim() >> _.Split(' ', StringSplitOptions.RemoveEmptyEntries) >> Array.map int)

    let cardId = cardPart.Replace("Card ", "") |> int
    let winningNumbers = Set.ofArray winningNumbersPart
    let availableNumbers = Set.ofArray availableNumbersPart

    {
        Id = cardId
        WinningNumbers = winningNumbers
        AvailableNumbers = availableNumbers
    }

let solve cards =
    let rec imp acc cards =
        match cards with
        | [] -> Map.values acc |> Seq.sum
        | card :: left ->
            let matchingNumbers =
                Set.intersect card.AvailableNumbers card.WinningNumbers
                |> Set.count
            
            let nextCards = cards[1..matchingNumbers]
            let increment = acc[card.Id]
            let newAcc = 
                (acc, nextCards)
                ||> List.fold (fun acc card -> acc |> Map.change card.Id (Option.map ((+) increment)))

            imp newAcc left

    let initial = cards |> List.map (fun card -> card.Id, 1) |> Map.ofList
    imp initial cards

let input = @"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

input.Split("\n") |> Seq.map parseCards |> Seq.toList |> solve

open System.IO

File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
|> Seq.map parseCards
|> Seq.toList
|> solve
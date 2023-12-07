open System

type Type =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard

let fiveOfAKind map =
    if Map.count map = 1 then
        Some FiveOfAKind
    else
        None

let fourOfAKind map =
    if Map.count map = 2 && Map.exists (fun _ count -> count = 4) map then
        Some FourOfAKind
    else
        None

let fullHouse map =
    if Map.count map = 2 && Map.exists (fun _ count -> count = 3) map then
        Some FullHouse
    else
        None

let threeOfAKind map =
    if Map.count map = 3 && Map.exists (fun _ count -> count = 3) map then
        Some ThreeOfAKind
    else
        None

let twoPair map =
    if Map.count map = 3 && Map.values map |> Seq.filter ((=) 2) |> Seq.length = 2 then
        Some TwoPair
    else
        None

let onePair map =
    if Map.count map = 4 then
        Some OnePair
    else
        None

let highCard map =
    if Map.count map = 5 then
        Some HighCard
    else
        None

let getType tryFindType (hand: string) =
    let grouped =
        hand
        |> Seq.countBy id
        |> Map.ofSeq

    tryFindType grouped |> Option.get

let tryFindType =
    [ fiveOfAKind; fourOfAKind; fullHouse; threeOfAKind; twoPair; onePair; highCard ]
    |> List.reduce (fun f g -> fun x -> match f x with | Some y -> Some y | None -> g x)

getType tryFindType "AAAAA"
getType tryFindType "AA8AA"
getType tryFindType "23332"
getType tryFindType "TTT98"
getType tryFindType "23432"
getType tryFindType "A23A4"
getType tryFindType "23456"

let rankingTable =
    "AKQJT98765432"
    |> Seq.rev
    |> Seq.mapi (fun i x -> x, i)
    |> Map.ofSeq

let compareCards
    (rankingTable: Map<char, int>)
    (firstCard: string)
    (secondCard: string) =
    let rec imp (rankingTable: Map<char, int>) (firstCard: string) (secondCard: string) index =
        let firstRank = Map.find firstCard[index] rankingTable
        let secondRank = Map.find secondCard[index] rankingTable
        
        match compare firstRank secondRank with
        | 0 -> imp rankingTable firstCard secondCard (index + 1)
        | i -> i
    
    imp rankingTable firstCard secondCard 0

compareCards rankingTable "KK677" "KTJJT"

["32T3K";"T55J5";"KK677";"KTJJT";"QQQJA"]
|> List.groupBy (getType tryFindType)
|> List.sortByDescending fst
|> List.collect (snd >> List.sortWith (compareCards rankingTable))

type CamelCard = { Hand: string; Bid: int }

let parse (input: string) =
    let [| handPart; bidPart |] = input.Split(" ", StringSplitOptions.RemoveEmptyEntries)

    { Hand = handPart; Bid = int bidPart }
    
parse "32T3K 765"

let solve cards =
    cards
    |> Seq.groupBy (fun card -> getType tryFindType card.Hand)
    |> Seq.sortByDescending fst
    |> Seq.collect
        (fun (_, cards) ->
            cards
            |> Seq.sortWith (fun first second -> compareCards rankingTable first.Hand second.Hand))
    |> Seq.mapi (fun index card -> card.Bid * (index + 1))
    |> Seq.reduce (+)

let sample = @"32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"

sample.Split("\n")
|> Seq.map parse
|> solve

open System.IO

File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
|> Seq.map parse
|> solve
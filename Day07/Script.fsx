open System

type CamelCard = { Hand: string; Bid: int }

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

let tryFindType =
    [ fiveOfAKind; fourOfAKind; fullHouse; threeOfAKind; twoPair; onePair; highCard ]
    |> List.reduce (fun f g -> fun x -> match f x with | Some y -> Some y | None -> g x)

let getType (hand: string) =
    let grouped =
        hand
        |> Seq.countBy id
        |> Map.ofSeq

    tryFindType grouped |> Option.get

getType "AAAAA"
getType "AA8AA"
getType "23332"
getType "TTT98"
getType "23432"
getType "A23A4"
getType "23456"

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

let parse (input: string) =
    let [| handPart; bidPart |] = input.Split(" ", StringSplitOptions.RemoveEmptyEntries)

    { Hand = handPart; Bid = int bidPart }
    
parse "32T3K 765"

let solve getType ranking cards =
    let rankingTable =
        ranking
        |> Seq.rev
        |> Seq.mapi (fun i x -> x, i)
        |> Map.ofSeq

    cards
    |> Seq.groupBy (fun card -> getType card.Hand)
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
|> solve getType "AKQJT98765432"

open System.IO

File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
|> Seq.map parse
|> solve getType "AKQJT98765432"

let wildCard getType (hand: string) =
    if hand.Contains('J') && hand <> "JJJJJ" then
        printfn "%s" hand
        hand
        |> String.filter ((<>) 'J')
        |> Seq.map (fun newChar -> hand.Replace('J', newChar))
        |> Seq.sortBy getType
        |> Seq.head
    else
        hand

wildCard getType "QQQJA"

sample.Split("\n")
|> Seq.map parse
|> solve (wildCard getType >> getType) "AKQT98765432J"

File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
|> Seq.map parse
|> solve (wildCard getType >> getType) "AKQT98765432J"
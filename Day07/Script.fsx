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
    "AKQJT98765432" |> Seq.mapi (fun i x -> x, i) |> Map.ofSeq

let compareCards rankingTable (firstCard: string) (secondCard: string) =
    let rec imp rankingTable (firstCard: string) (secondCard: string) index =
        let firstRank = Map.find firstCard[index] rankingTable
        let secondRank = Map.find firstCard[index] rankingTable
        
        if firstRank < secondRank then
            -1
        else if firstRank > secondRank then
            1
        else
            imp rankingTable firstRank secondCard (index + 1)
    
    imp rankingTable firstCard secondCard 0

["32T3K";"T55J5";"KK677";"KTJJT";"QQQJA"]
|> List.groupBy (getType tryFindType)
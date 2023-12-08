type Node = Node of string

type Element = { Left: Node; Right: Node }

type Network = { Instructions: string; Lookup: Map<Node, Element> }

let nextIndex length index =
    (index + 1) % length
    
let (|Left|Right|) chr =
    match chr with
    | 'L' -> Left
    | 'R' -> Right
    | _ -> invalidArg (nameof chr) (sprintf "invalid character '%c'" chr)

let step network node index =
    let newNode =
        match network.Instructions[index] with
        | Left -> Map.find node network.Lookup |> _.Left
        | Right -> Map.find node network.Lookup |> _.Right

    let newIndex = nextIndex network.Instructions.Length index
    newNode, newIndex

let solvePartOne network =
    let rec imp network target stepsSoFar node index =
        let nextNode, nextIndex = step network node index

        if nextNode = target then
            stepsSoFar + 1
        else
            imp network target (stepsSoFar + 1) nextNode nextIndex

    imp network (Node "ZZZ") 0 (Node "AAA") 0

let parseLookup (line: string) =
    let [| node; instructionPart |] = line.Split(" = ")
    let [| left; right |] = instructionPart[1..^1] |> _.Split(", ")
    let instruction = { Left = Node left; Right = Node right }
    Node node, instruction

let parse (input: string) =
    let [| instruction; lookupPart |] = input.Split("\n\n")
    let lookup =
        lookupPart.Split("\n")
        |> Seq.map parseLookup
        |> Map.ofSeq

    { Instructions = instruction; Lookup = lookup}

let sample1 = @"RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"

parse sample1 |> solvePartOne

let sample2 = @"LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"

parse sample2 |> solvePartOne

open System.IO

let network =
    File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
    |> parse

solvePartOne network

let rec greatestCommonDivisor a b =
    match a, b with
    | x, 0L -> x
    | 0L, y -> y
    | a, b -> greatestCommonDivisor b (a % b)

let leastCommonMultiple a b =
    a * b / (greatestCommonDivisor a b)

let solvePartTwo network =
    let rec imp network untilPredicate stepsSoFar node index =
        let nextNode, nextIndex = step network node index

        if untilPredicate nextNode then
            stepsSoFar + 1L
        else
            imp network untilPredicate (stepsSoFar + 1L) nextNode nextIndex

    let steps =
        network.Lookup
        |> Map.filter (fun (Node node) _ -> node.EndsWith("A"))
        |> Map.map
            (fun node _ ->
                imp network (fun (Node node) -> node.EndsWith("Z")) 0 node 0)

    Map.values steps |> Seq.reduce leastCommonMultiple

@"LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"
|> parse
|> solvePartTwo

#time
solvePartTwo network
#time
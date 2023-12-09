let parseHistory (input: string) =
    input.Split(" ") |> Seq.map int |> Seq.toList

let step history =
    let rec imp acc historySoFar =
        match historySoFar with
        | x :: y :: zs -> imp ((y - x) :: acc) (y :: zs)
        | _ -> List.rev acc

    imp [] history

let extrapolate steps =
    let rec imp stepsSoFar valueSoFar =
        match stepsSoFar with
        | [] -> valueSoFar
        | steps :: left -> imp left (List.last steps + valueSoFar)

    imp steps 0

let extrapolateBackwards steps =
    let rec imp stepsSoFar valueSoFar =
        match stepsSoFar with
        | [] -> valueSoFar
        | steps :: left -> imp left (List.head steps - valueSoFar)

    imp steps 0

let diff history =
    let rec imp acc history =
        let next = step history
        
        match List.forall ((=) 0) next with
        | true -> acc
        | false -> imp (next :: acc) next

    imp [ history ] history

let solvePartOne histories =
    histories
    |> Seq.sumBy (diff >> extrapolate)

let solvePartTwo histories =
    histories
    |> Seq.sumBy (diff >> extrapolateBackwards)

open System.IO

#time
File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
|> Seq.map parseHistory
|> solvePartOne
#time

#time
File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
|> Seq.map parseHistory
|> solvePartTwo
#time
open System

let findDigit (input: string) =
    if Char.IsDigit(input[0]) then
        Some(int input[0] - int '0')
    else
        None

let mapping =
    Map.ofList
        [ "one", 1
          "two", 2
          "three", 3
          "four", 4
          "five", 5
          "six", 6
          "seven", 7
          "eight", 8
          "nine", 9
          "zero", 0 ]

let findDigitByWord (input: string) =
    mapping
    |> Map.tryPick (fun target value -> if input.StartsWith(target) then Some value else None)

let tryPick f g x =
    match f x with
    | Some value -> Some value
    | None -> g x

let findFirst fs (input: string) =
    Seq.init input.Length (fun length -> input[length..])
    |> Seq.pick (List.reduce tryPick fs)

let findLast fs (input: string) =
    Seq.init input.Length (fun length -> input[input.Length - 1 - length ..])
    |> Seq.pick (List.reduce tryPick fs)

let getCalibrationValue fs (input: string) =
    findFirst fs input * 10 + findLast fs input

let solve fs lines =
    lines |> Seq.sumBy (getCalibrationValue fs)

open System.IO

let lines = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
solve [ findDigit ] lines
solve [ findDigit; findDigitByWord ] lines

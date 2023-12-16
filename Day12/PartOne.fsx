open System.IO

let arrangements groups map =
    let rec loopDot map groups index =
        if index + List.sum groups + List.length groups - 1 > String.length map then
            0
        else if index >= String.length map then
            1
        else
            match groups with
            | [] -> if map.IndexOf('#', index) < 0 then 1 else 0
            | length :: left ->
                let count =
                    match map[index] with
                    | '.' -> loopDot map groups (index + 1)
                    | '#' -> loopHash map left (length - 1) (index + 1)
                    | '?' ->
                        loopHash map left (length - 1) (index + 1)
                        + loopDot map groups (index + 1)
                    | chr -> failwithf $"Unexpected character '%c{chr}' at index %i{index} of map '%s{map}'"

                count

    and loopHash map groups length index =
        let endIndex = index + length
        if endIndex > String.length map then
            0
        else
            let nextDot =
                match map.IndexOf('.', index) with
                | -1 -> map.Length
                | n -> n

            if nextDot >= endIndex then
                if endIndex = String.length map then
                    if List.isEmpty groups then 1 else 0
                else if map[endIndex] = '#' then
                    0
                else
                    loopDot map groups (endIndex + 1)
            else
                0

    loopDot map groups 0

let parse (line: string) =
    let map, groupsPart =
        match line.Split(" ") with
        | [| springs; damagedSpringsPart |] -> springs, damagedSpringsPart
        | _ -> failwith $"Unexpected parts with length of %i{line.Length}"

    let groups = groupsPart.Split(",") |> Seq.map int |> Seq.toList

    map, groups

let solvePartOne lines =
    lines
    |> Seq.sumBy (fun (map, groups) -> arrangements groups map)

arrangements [3;2;1] "?###????????"

let sample =
    @"???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"

sample.Split("\n")
|> Seq.map parse
|> solvePartOne

#time

File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
|> Seq.map parse
|> solvePartOne

#time
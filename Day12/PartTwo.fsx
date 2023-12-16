open System.Collections.Generic
open System.IO

let arrangements groups map =
    let rec loopDot map groups index (cache: Dictionary<int * int list, int64>) =
        match cache.TryGetValue((index, groups)) with
        | true, count -> count
        | false, _ ->
            if index + List.sum groups + List.length groups - 1 > String.length map then
                0L
            else if index >= String.length map then
                1L
            else
                match groups with
                | [] -> if map.IndexOf('#', index) < 0 then 1 else 0
                | length :: left ->
                    let count =
                        match map[index] with
                        | '.' -> loopDot map groups (index + 1) cache
                        | '#' -> loopHash map left (length - 1) (index + 1) cache
                        | '?' ->
                            loopHash map left (length - 1) (index + 1) cache
                            + loopDot map groups (index + 1) cache
                        | chr -> failwithf $"Unexpected character '%c{chr}' at index %i{index} of map '%s{map}'"

                    cache.Add((index, groups), count)
                    count

    and loopHash map groups length index cache =
        let endIndex = index + length
        if endIndex > String.length map then
            0L
        else
            let nextDot =
                match map.IndexOf('.', index) with
                | -1 -> map.Length
                | n -> n

            if nextDot >= endIndex then
                if endIndex = String.length map then
                    if List.isEmpty groups then 1 else 0
                else if map[endIndex] = '#' then
                    0L
                else
                    loopDot map groups (endIndex + 1) cache
            else
                0L

    loopDot map groups 0 (Dictionary<int * int list, int64>())

let parse (line: string) =
    let map, groupsPart =
        match line.Split(" ") with
        | [| springs; damagedSpringsPart |] -> springs, damagedSpringsPart
        | _ -> failwith $"Unexpected parts with length of %i{line.Length}"

    let groups = groupsPart.Split(",") |> Seq.map int |> Seq.toList

    map, groups

let solvePartTwo lines =
    lines
    |> Seq.sumBy (fun (map, groups) -> arrangements (groups @ groups @ groups @ groups @ groups) (map + "?" + map + "?" + map + "?" + map + "?" + map))

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
|> solvePartTwo

#time

File.ReadLines(Path.Combine(__SOURCE_DIRECTORY__, "Input.txt"))
|> Seq.map parse
|> solvePartTwo

#time
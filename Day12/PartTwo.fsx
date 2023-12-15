open System
let skipWhile predicate take list =
    let rec imp predicate left list =
        if left = 0 then
            Some list
        else
            match list with
            | x :: xs when predicate x -> imp predicate (left - 1) xs
            | _ -> None

    imp predicate take list

skipWhile (function | '#' | '?' -> true | _ -> false) 3 (Seq.toList "?##..#")

let rec collectCandidates damagedSprings springs =
    // printfn "%i, %s" damagedSprings (springs |> List.toArray |> String)
    let rec imp acc damagedSprings springs =
        match springs with
        | [] -> acc
        | '.' :: left -> imp acc damagedSprings left
        | '#' :: _ ->
            match springs |> skipWhile (function | '#' | '?' -> true | _ -> false) damagedSprings with
            | Some [] -> [] :: acc
            | Some ('#' :: _) -> acc
            | Some (_ :: xs) -> xs :: acc
            | None -> []
        | '?' :: left ->
            match springs |> skipWhile (function | '#' | '?' -> true | _ -> false) damagedSprings with
            | Some [] -> [] :: acc
            | Some ('#' :: _) -> acc
            | Some (_ :: xs) -> imp (xs :: acc) damagedSprings left
            | None -> []

    imp [] damagedSprings springs

let step damagedSpringCounts springs =
    match damagedSpringCounts with
    | [] -> [], []
    | d :: ds -> ds, collectCandidates d springs

let arrangements damagedSpringCounts springs =
    let rec imp damagedSpringCounts springs =
        printfn "%A, %s" damagedSpringCounts (springs |> List.toArray |> String)
        match damagedSpringCounts, springs with
        | [], [] -> 1
        | [], _ when springs |> List.forall ((<>) '#') -> 1
        | [], _ -> 0
        | _, [] -> 0
        | d :: ds, _ ->
            let next = collectCandidates d springs
            
            next
            |> List.map (imp ds)
            |> List.length

    imp damagedSpringCounts springs

arrangements [3;1] (Seq.toList ".##?...??")

step [3;1] (Seq.toList ".##?...#")
|> fun (ds, springs) -> springs |> List.map (step ds)
let travel (distance: int64) (holdTime: int64) = holdTime * (distance - holdTime)

let ifZero fallback value = if value = 0L then fallback else value

let findMinHold distance threshold hold =
    let rec imp distance threshold hold incrementSoFar =
        let traveled = travel distance hold

        if traveled <= threshold then
            let traveledAfter = travel distance (hold + 1L)

            if traveledAfter > threshold then
                hold + 1L
            else
                let newIncrement = ifZero 1L (incrementSoFar / 2L)
                let newHold = hold + newIncrement
                imp distance threshold newHold newIncrement
        else
            let newIncrement = ifZero 1L (incrementSoFar / 2L)
            let newHold = hold - newIncrement
            imp distance threshold newHold newIncrement

    imp distance threshold hold hold

[ 0L .. 7L ] |> List.map (travel 7L)

findMinHold 7 9 3
findMinHold 15 40 8
findMinHold 30 200 15

let solve distance threshold =
    let initialHold =
        (distance + 1L + (if distance % 2L = 0L then 1L else 0L)) / 2L - 1L

    distance - (2L * findMinHold distance threshold initialHold) + 1L

solve 7 9
solve 15 40
solve 30 200

List.reduce (*) [ solve 46 214; solve 80 1177; solve 78 1402; solve 66 1024 ]

#time
solve 46807866L 214117714021024L
#time
(*
x12 = y/2 +- sqrt(p^2/4 + d)
*)

let solve2 (y: int64) (d: int64) =
    let p = double -y
    let q = double d
    let common = sqrt ((p / 2.0) * (p / 2.0) - q)

    ceil (-p / 2.0 + common - 1.0) + 1.0 - truncate (-p / 2.0 - common + 1.0)
    |> int64

solve2 7 9
solve2 15 40
solve2 30 200

#time
solve2 46807866L 214117714021024L
#time
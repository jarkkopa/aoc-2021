module Day1

open System.IO

let readInput () =
    File.ReadAllLines "Resources/day1-input.txt"
    |> Array.map int
    |> Array.toList

let depthTrend x =
    match x with
    | (a, b) when a < b -> "increased"
    | (a, b) when a > b -> "decreased"
    | (a, b) when a = b -> "no change"
    | _ -> "N/A"

let rec solve list r =
    match list with
    | [ _ ] -> r
    | head :: tail ->
        let next = (head, List.head tail) |> depthTrend
        let newR = r @ [ next ]
        solve tail newR
    | _ -> r

let howManyIncreases input =
    let filterIncreased = List.filter (fun x -> x = "increased")

    (input, [])
    ||> solve
    |> filterIncreased
    |> List.length

let rec sumRollingThree list r =
    match list with
    | [ _; _ ] -> r
    | head :: tail ->
        let newR =
            r @ [ head :: List.take 2 tail |> List.sum ]

        sumRollingThree tail newR
    | _ -> r

let day1part1 () = readInput () |> howManyIncreases

let day1part2 () =
    let input = readInput ()

    (input, [])
    ||> sumRollingThree
    |> howManyIncreases

module Day1

open System.IO

let readInput =
    File.ReadAllLines "Resources/day1-input.txt"
    |> Array.map int
    |> Array.toList

let depthTrend x =
    match x with
    | (a, b) when a < b -> "increased"
    | (a, b) when a > b -> "decreased"
    | _ -> "no change"

let rec solveDepthTrends r list =
    match list with
    | [ _ ] -> r
    | head :: tail ->
        let next = (head, List.head tail) |> depthTrend
        let newR = r @ [ next ]
        solveDepthTrends newR tail
    | _ -> r

let howManyIncreases input =
    input
    |> solveDepthTrends []
    |> List.filter (fun x -> x = "increased")
    |> List.length

let rec sumRollingThree r list =
    match list with
    | [ _; _ ] -> r
    | head :: tail ->
        let newR = r @ [ head :: List.take 2 tail |> List.sum ]
        sumRollingThree newR tail
    | _ -> r

let day1part1 = readInput |> howManyIncreases

let day1part2 = readInput |> sumRollingThree [] |> howManyIncreases
module Day2

open System.IO

let readInput =
    File.ReadAllLines "Resources/day2-input.txt"
    |> Array.map (fun x -> x.Split())
    |> Array.map (fun a -> a.[0], a.[1] |> int)
    |> Array.toList

let rec mov (h,v) dir =
    match dir with
    |   head::tail ->
        match head with
        | ("forward", b) -> mov (h + b,v) tail
        | ("up", b) -> mov (h, v - b) tail
        | ("down", b) -> mov (h, v + b) tail
        | a -> failwith "Invalid direction"
    | [] -> (h,v)

let day2part1 =
    readInput
    |> List.groupBy fst
    |> List.map (fun x -> fst x, x |> snd |> List.sumBy snd)
    |> mov (0,0)
    ||> (fun a b -> a*b)
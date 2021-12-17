module Day2

open System.IO

let readInput =
    File.ReadAllLines "Resources/day2-input.txt"
    |> Array.map (fun x -> x.Split())
    |> Array.map (fun a -> a.[0], a.[1] |> int)
    |> Array.toList

let rec mov (h, v) dir =
    match dir with
    |   head::tail ->
        match head with
        | ("forward", b) -> mov (h + b,v) tail
        | ("up", b) -> mov (h, v - b) tail
        | ("down", b) -> mov (h, v + b) tail
        | _ -> failwith "Invalid direction"
    | [] -> (h,v)

let day2part1 =
    readInput
    |> mov (0,0)
    ||> (fun a b -> a * b)

let rec movWithAim (h, v, a) dir =
    match dir with
    |   head::tail ->
        match head with
        | ("forward", b) ->
            movWithAim (h + b, v + a * b, a) tail
        | ("up", b) ->
            movWithAim (h, v, a - b) tail
        | ("down", b) ->
            movWithAim (h, v, a + b) tail
        | _ -> failwith "Invalid direction"
    | [] -> (h, v, a)

let day2part2 =
    readInput
    |> movWithAim (0, 0, 0)
    |||> (fun h v _ -> h * v)
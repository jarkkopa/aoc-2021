module Day2

open System.IO

let readInput =
    File.ReadAllLines "Resources/day2-input.txt"
    |> Array.map (fun x -> x.Split())
    |> Array.map (fun a -> a.[0], a.[1] |> int)
    |> Array.toList

let withPos (h, v, _) dir =
    match dir with
    | ("forward", b) -> (h + b, v, 0)
    | ("up", b) -> (h, v - b, 0)
    | ("down", b) -> (h, v + b, 0)
    | _ -> failwith "Invalid direction"

let withAim (h, v, a) dir =
    match dir with
    | ("forward", b) -> (h + b, v + a * b, a)
    | ("up", b) -> (h, v, a - b)
    | ("down", b) -> (h, v, a + b)
    | _ -> failwith "Invalid direction"

let rec movWith mode (h, v, a) dir =
    match dir with
    | head::tail -> movWith mode (mode(h,v,a) head) tail
    | [] -> (h, v, a)

let day2 mode = readInput |> movWith mode (0, 0, 0) |||> (fun h v _ -> h * v)
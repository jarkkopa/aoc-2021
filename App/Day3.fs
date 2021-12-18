module Day3

open System
open System.IO

let readInput =
    File.ReadAllLines "Resources/day3-input.txt"
    |> Array.toSeq

let toBit t =
    let higher x y =
        if snd x > snd y then fst x
        else fst y
        
    t |> List.pairwise |> List.map (fun x -> x ||> higher |> string)

let toInt x = Convert.ToInt32(x, 2)

let day3part1 =
    let bits =
        readInput
        |> Seq.transpose
        |> Seq.map Seq.toList
        |> Seq.map (List.countBy id)
        |> Seq.map toBit
        |> Seq.concat
        |> Seq.reduce(+)
    let gamma = bits |> toInt
    let mask = Seq.replicate bits.Length "1" |> Seq.reduce(+) |> toInt
    let alpha = gamma ^^^ mask
    gamma * alpha
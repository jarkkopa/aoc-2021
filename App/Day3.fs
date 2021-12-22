module Day3

open System
open System.IO

let readInput =
    File.ReadAllLines "Resources/day3-input.txt"
    |> Array.toList

let toBit t =
    let higher x y =
        if snd x > snd y then fst x
        else fst y
        
    t |> Seq.pairwise |> Seq.map (fun x -> x ||> higher |> string)

let toInt x = Convert.ToInt32(x, 2)

let day3part1 =
    let bits =
        readInput
        |> Seq.transpose
        |> Seq.map (Seq.countBy id)
        |> Seq.map toBit
        |> Seq.concat
        |> Seq.reduce(+)
    let gamma = bits |> toInt
    let mask = Seq.replicate bits.Length "1" |> Seq.reduce(+) |> toInt
    let alpha = gamma ^^^ mask
    gamma * alpha

let day3part2 =
    let significant def (items:(char*int)list) =
        if snd items.[0] = snd items.[1] then def
        else fst items.[0]

    let bitCounts sorter idx (bits: string list) =
        bits
        |> Seq.transpose
        |> Seq.item idx
        |> Seq.toList
        |> List.countBy id
        |> sorter snd

    let rec findValue sorter def i values =
        match values with
        | [_] -> values
        | x ->
            let wantedBit = 
                bitCounts sorter i values
                |> significant def

            values
            |> List.filter (fun x -> x.[i] = wantedBit)
            |> findValue sorter def (i + 1)
    
    let input = readInput
    let oxygen =
        input
        |> findValue List.sortByDescending '1' 0
        |> Seq.head
        |> toInt

    let co2 =
        input
        |> findValue List.sortBy '0' 0
        |> Seq.head
        |> toInt

    oxygen * co2
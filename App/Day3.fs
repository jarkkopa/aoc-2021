module Day3

open System
open System.IO

let readInput =
    File.ReadAllLines "Resources/day3-input.txt"
    |> Array.toSeq

let readInputTest =
    [|
        "00100";
        "11110";
        "10110";
        "10111";
        "10101";
        "01111";
        "00111";
        "11100";
        "10000";
        "11001";
        "00010";
        "01010";
    |]
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

let bitEquals idx expected (data:string) =
    data.[idx] = expected

let higher def x y =
    if snd x > snd y then fst x
    elif snd x < snd y then fst y
    else def

let lower def x y =
    if snd x < snd y then fst x
    elif snd x > snd y then fst y
    else def

let bitCounts idx bits =
    bits
    |> Seq.transpose
    |> Seq.map Seq.toList
    |> Seq.item idx
    |> List.countBy id

let day3part2 =
    let input = readInput

    let filterCommon compare def idx bits= 
        let common = bitCounts idx bits
        let c = (common.[0], common.[1])
            ||> compare def
        bits |> List.filter (fun (x:string) -> x.[idx] = c)

    let rec findValue compare def i values =
        match values with
        | [_] -> values
        | x -> (filterCommon compare def i x) |> findValue compare def (i+1)
    
    let oxygen =
        input
        |> Seq.toList
        |> findValue higher '1' 0
        |> Seq.head
        |> toInt

    let co2 =
        input
        |> Seq.toList
        |> findValue lower '0' 0
        |> Seq.head
        |> toInt
    
    oxygen*co2
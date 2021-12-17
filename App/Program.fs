open Day1
open Day2

[<EntryPoint>]
let main argv =
    // day1part1() |> printfn "Day 1-1 result: %i"
    // day1part2() |> printfn "Day 1-2 result: %i"
    day2 withPos |> printfn "Day 2-1 result: %A"
    day2 withAim |> printfn "Day 2-2 result: %A"
    0
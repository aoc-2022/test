open System.IO

let lines = File.ReadAllLines "/Users/xeno/projects/aoc/test/input1.txt"
let ints = lines |> Array.map int |> Array.toList 

// ints |> List.map (printfn "%d")

let rec check1 list =
    match list with
    | a::b::tail -> (if a < b then 1 else 0) + check1 (b::tail)
    | [a] -> 0
    | [] -> 0
    
let rec check2 list =
    match list with
    | a::b::c::d::tail -> (if (a+b+c) < (b+c+d) then 1 else 0) + check2 (b::c::d::tail)
    | _ -> 0
 
let increases = check1 ints
let increases2 = check2 ints

printfn "%d" increases
printfn "%d" increases2
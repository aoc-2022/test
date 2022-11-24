open System
open System.IO

let lines = File.ReadAllLines "/Users/xeno/projects/aoc/test/input2.txt"

let lineToDelta (line:string) =
    let words = line.Split [|' '|] |> Array.toList
    match words with
    | ["forward";x] -> (x |> int,0)
    | ["up";x] -> (0, - (x |> int))
    | ["down";x] -> (0, x |> int)
    | _ -> raise (Exception($"Invalid input {words}"))

let deltas = lines |> Array.map lineToDelta

let horizontal = deltas |> Array.map fst |> Array.sum
let verticals = deltas |> Array.map snd |> Array.sum

printfn $"(%d{horizontal},%d{verticals} => {horizontal * verticals})"

// part 2

let rec move (aim : int) ((x,y) : int*int) (lines : string list) : int*int = 
    match lines with
    | [] -> (x,y)
    | line :: rest ->
        match line.Split [|' '|] |> Array.toList with
        | ["up";d] -> move (aim - (d |> int)) (x,y) rest
        | ["down";d] -> move (aim + (d |> int)) (x,y) rest
        | ["forward";d] -> move (aim) (x+(d |> int),y+(aim*(d |> int))) rest
        | _ -> raise (Exception($"Unknown line: {line}"))

let pos = move 0 (0,0) (lines |> Array.toList)
printfn $"pos {pos} {fst pos * snd pos}"
        
        
        
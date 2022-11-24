open System
open System.IO

let lines = File.ReadAllLines "/Users/xeno/projects/aoc/test/input3.txt"
let bits = lines
           |> Array.toList
           |> List.map Seq.toList
           |> List.map (List.map (fun i -> if i = '0' then -1 else 1))
           // |> List.map (printfn "%A")
           
           
let rec addlist a b =
    match (a,b) with
    | (a::ar,b::br) -> a+b :: addlist ar br
    | ([],[]) -> []
    | _ -> raise (Exception($"List mismatch {(a,b)}"))

let bits2 = List.reduce addlist bits

bits2 |> List.map (printfn "%A")
printfn $"{bits2}"

let rec topos bits =
    match bits with
    | a::rest -> (if a > 0 then 1 else 0) :: topos rest
    | [] -> []
    
let rec toneg bits =
    match bits with
    | a::rest -> (if a < 0 then 1 else 0) :: toneg rest
    | [] -> []

let gamma_bits = topos bits2
let epsilon_bits = toneg bits2

printfn $"gamma: {gamma_bits} epsilon: {epsilon_bits}"

let toDeci bits =
    let bits = List.rev bits    
    let rec toDeci bits =
        match bits with
        | [] -> 0
        | [last] -> last
        | a::b::rest -> a + 2*(toDeci (b::rest))
    toDeci bits 
    
let gamma = toDeci gamma_bits
let epsilon = toDeci epsilon_bits

printfn $"gamma: {gamma} eps: {epsilon} mult = {gamma*epsilon}"

let x = toDeci 
           
// part 2

let rec findOxygen (bitss : int list list) =
    printfn $"findOxygen: {bitss |> List.length} {bitss}"
    if bitss |> List.length = 1
    then bitss |> List.head 
    else
        if bitss |> List.head |> List.length = 0 then []
        else 
            let first = bitss
                        |> List.map List.head
                        |> List.sum
            let value = if first >= 0 then 1 else -1
            let rems = bitss
                       |> List.filter (fun bl -> (List.head bl) = value)
                       |> List.map List.tail
            value :: findOxygen rems

let rec findScrubber (bitss : int list list) =
    printfn $"findOxygen: {bitss |> List.length} {bitss}"
    if bitss |> List.length = 1
    then bitss |> List.head 
    else
        if bitss |> List.head |> List.length = 0 then []
        else 
            let first = bitss
                        |> List.map List.head
                        |> List.sum
            let value = if first < 0 then 1 else -1
            let rems = bitss
                       |> List.filter (fun bl -> (List.head bl) = value)
                       |> List.map List.tail
            value :: findScrubber rems
            
let oxygen = findOxygen bits
             |> List.map (fun b -> if b = -1 then 0 else 1)
             |> toDeci 
printfn $"Oxygen: {findOxygen bits} {oxygen}"      
            
let scrubber = findScrubber bits
             |> List.map (fun b -> if b = -1 then 0 else 1)
             |> toDeci 
printfn $"Scrubber: {findScrubber bits} {scrubber}"

printfn $"res: {oxygen * scrubber}"
   
    
    
    
    
                
    
           
           


        
        
        
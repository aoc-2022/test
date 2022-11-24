open System
open System.IO

let lines = File.ReadAllLines "/Users/xeno/projects/aoc/test/input4.txt" |> Array.toList 

let splitOn list value =
    let rec split acc list =
        match list with
        | [] -> [acc]
        | v :: rest when v = value -> (List.rev acc) :: split [] rest
        | v :: rest -> split (v::acc) rest
    let res = split [] list
    res 

let v = splitOn lines ""
printfn $"{v}"    
    
let numbers = v |> List.head
let boards = v |> List.tail

type Board(numbers: int list, rows: Set<int> list, called: int) =
    member this.Numbers = numbers
    member this.Called = called
    
    member this.isBingo : Boolean = rows |> List.contains Set.empty
    
    member this.call (num:int) =
        let numbers = numbers |> List.filter (fun n -> n <> num)
        let rows = rows |> List.map (Set.remove num)
        // let called = called |> Set.add num
        Board(numbers, rows, num)
        
    member this.Score =
        let unused = numbers |> List.sum
        unused * called 
    
    override this.ToString () =
        $"Board({numbers} {rows}) called:{called}"
    
    new(board:int list list) =
        let numbers = board |> List.concat
        let rec toCols rows =
            if List.head rows = [] then []
            else
                let col = rows |> List.map List.head
                let rems = rows |> List.map List.tail
                col :: toCols rems
        let cols = toCols board
        let lines = [cols;board] |> List.concat
        let lines = lines |> List.map Set.ofList
        Board(numbers,lines,0)

let toInts (s:string) =
    s.Split [|' '|]
    |> Array.filter (fun s -> s = "" |> not) 
    |> Array.map int |> Array.toList
let boards1 = boards |> List.map (List.map toInts)
let boards2 = boards1 |> List.map Board

printfn $"{boards2}"
let numbers2 = numbers |> List.head |> (fun s -> s.Split [|','|]) |> Array.map int |> Array.toList  
printfn $"numbers: {numbers2}"

let rec callUntilBingo (numbers: int list) (boards: Board list) : Board =
    let number = numbers |> List.head 
    if boards |> List.exists (fun board -> board.isBingo)
    then boards |> List.filter (fun board -> board.isBingo) |> List.head
    else
        let boards = boards |> List.map (fun board -> board.call number)
        let numbers = numbers |> List.tail
        callUntilBingo numbers boards 

let bingo = callUntilBingo numbers2 boards2

printfn $"{bingo} {bingo.Numbers |> List.sum} {bingo.Called} {bingo.Score}"

let rec callUntilLoss (numbers: int list) (boards: Board list) : Board =
    printfn $"Call until loss: {numbers} {boards}"
    let number = numbers |> List.head
    let boards = boards |> List.map (fun board -> board.call number)
    let boards2 = boards |> List.filter (fun board -> board.isBingo |> not)
    if boards2 |> List.length = 0
    then boards |> List.head 
    else callUntilLoss (numbers |> List.tail) boards2 

let notBingo = callUntilLoss numbers2 boards2

printfn $"{notBingo} {notBingo.Numbers |> List.sum} {notBingo.Called} {notBingo.Score}"


    
           
           


        
        
        
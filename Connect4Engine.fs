module Connect4

open System
open System.IO

let width, height = 7, 6

type Player =
    | PlayerA
    | PlayerB

type Cell =
    | Empty
    | Occupied of Player

let isOccupied (cell: Cell): bool =
    match cell with
    | Empty -> false
    | _ -> true

type Board = Cell [] []

let createBoard (w: int) (h: int) (init) =
    [| for x in [ 1 .. h ] -> [| for y in [ 1 .. w ] -> init x y |] |]

let config c =
    match c with
    | Empty -> " "
    | Occupied p ->
        match p with
        | PlayerA -> "O"
        | PlayerB -> "X"

let drawSymbol config c = "| " + config c + " "


let drawRow row =
    row
    |> Array.map (drawSymbol config)
    |> fun x -> Array.append x [| "|" |]
    |> (Array.fold (+) "") // concats a list of strings

let drawHLine w =
    List.append [ for x in [ 1 .. w ] -> "+---" ] [ "+" ]
    |> List.fold (+) ""

let drawLastLine w =
    List.append [ for x in [ 1 .. w ] -> "+-" + string (x - 1) + "-" ] [ "+" ]
    |> List.fold (+) ""


let rec drawBoardToConsole (board: Cell [] []) =
    let n = Array.length board.[0]
    match board with
    | [||] -> printfn "%s" (drawLastLine n)
    | [| x |] ->
        printfn "%s" (drawRow x)
        printfn "%s" (drawLastLine n)
    | _ ->
        printfn "%s" (drawRow board.[0])
        printfn "%s" (drawHLine n)
        drawBoardToConsole board.[1..n - 1]



let rec drawBoardToString (board: Cell [] []) =
    let n = Array.length board.[0]
    match board with
    | [||] -> drawLastLine n
    | [| x |] -> (drawRow x) + "\n" + (drawLastLine n)
    | _ ->
        (drawRow board.[0])
        + "\n"
        + (drawHLine n)
        + "\n"
        + (drawBoardToString board.[1..n - 1])


let rec playInColFromRow (col: int) (row: int) (board: Board) (player: Player) =
    if row = -1 then
        (board, false)
    else if board.[row].[col] = Empty then
        Array.set board.[row] col (Occupied player)
        (board, true)
    else
        playInColFromRow col (row - 1) board player

let playInCol (n: int) (board: Board) (player: Player) =
    playInColFromRow n (Array.length board - 1) board player
(*
let rec getFours (l: 'a list):'a list list =
    if List.length l < 4 then
        [[]]
    elif List.length l = 4 then
        [l]
    else
        match l with
        | [] -> [[]]
        | x::xs -> [[x;l.[1];l.[2];l.[3]]] @ getFours xs

let quadToPlayer quad=
    if List.forall (fun x -> x = Occupied PlayerA) quad then
        Some PlayerA
    elif List.forall (fun x -> x = Occupied PlayerB) quad then
        Some PlayerB
    else
        None

let rowWinnerHelper (row : Cell []) : Player option=
    if Array.length row <4 then
        None
    else
        let possibleWinners = getFours (List.ofArray row) |> List.map quadToPlayer
        if List.exists (fun x -> x = Some PlayerA) possibleWinners then
           Some PlayerA
        elif List.exists (fun x -> x = Some PlayerB) possibleWinners then
           Some PlayerB
        else
           None

let rowWinner (board:Board) : Player option =
    let rowWinners = List.map rowWinnerHelper board
    if List.exists (fun x -> x = Some PlayerA) rowWinners then
        Some PlayerA
    elif List.exists (fun x -> x = Some PlayerB) rowWinners then
        Some PlayerB
    else
        None
*)


// Given a row and a piece, this function tells you
// if there are four consecutive pieces in the row
let arrayWinner (row: Cell []) (player: Player) =
    Array.fold (fun count p ->
        if count = 4 then 4
        elif p = Occupied player then count + 1
        else 0) 0 row
    |> (=) 4


// Takes a list of arrays and checks if p has one
let arrayOfArrayWinner (board: Cell [] []) (player: Player) =
    Array.exists (fun arr -> arrayWinner arr player) board

// This is basically just a transpose function, which I had to write again
// because of my terrible life choices, like defining boards as lists of arrays...
// I basically copied this function from
// https://stackoverflow.com/questions/3016139/help-me-to-explain-the-f-matrix-transpose-function
let rec getColumns (board: 'a [] []) =
    if Array.length board > 0
       && Array.length board.[0] > 0 then
        Array.append [| (Array.map Array.head board) |] (getColumns (Array.map Array.tail board))
    else
        [||]

(* match board with
    |  _::_ when Array.length board.[0]>0 ->
        (Array.map Array.head board) :: getColumns (Array.map Array.tail board)
    | _ -> [] *)

// Takes a board and returns Diagonals in this direction /
// This is admittedly super hacky.
// Todo: Make readable
let getDiags1 board =
    let nrows = Array.length board
    let ncols = Array.length board.[0]
    let n = nrows + ncols - 1 // number of diagonals
    [| for d in [ 0 .. n - 1 ] -> [| for i in [ max 0 (d - nrows + 1) .. (min d 6) ] -> board.[d - i].[i] |] |]

let getDiags2 board =
    let nrows = Array.length board
    let ncols = Array.length board.[0]
    let n = nrows + ncols - 1 // number of diagonals
    [| for d in [ -nrows + 1 .. ncols - 1 ] ->
        [| for i in [ max 0 d .. (min (d + nrows) ncols - 1) ] -> board.[i - d].[i] |] |]


let winner (board: Board) (player: Player) =
    if arrayOfArrayWinner board player
    then true
    elif arrayOfArrayWinner (getColumns board) player
    then true
    elif arrayOfArrayWinner (getDiags1 board) player
    then true
    else arrayOfArrayWinner (getDiags2 board) player
// equivalent to checking if listOfArrayWinner (getDiags2 board) player is true,
// then returning true, and returning false in the else case.

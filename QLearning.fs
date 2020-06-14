module AIPlay

open System
open FSharpx.Collections
open Synapses
open Connect4

let width, height = 7, 6

(*let middleLayer: int = (width * height + height) / 2 + 1

let net =
    NeuralNetwork.init [ width * height; middleLayer; width ] *)

// NeuralNetwork.prediction (net, [for i in [1..42] -> float i])

// Define actual play.

// Get move out of prediction
// Why is there no List.foldWithIndex in F#?
// Maybe better with fold2, but then I would have to
// compute length of nums first.
let indexOfMax nums =
    List.fold (fun ind x ->
        match ind with
        | ((indMax, i), m) -> if x > m then ((i, i + 1), x) else ((indMax, i + 1), m)

        ) ((0, 0), List.head nums) nums
    |> fst
    |> fst



type Turn = { BoardState: Board; PlayedMove: int }

type Robot =
    { Policy: NeuralNetwork
      Memory: Turn list
      Color: Player
      ExploreEpsilon: float }


// For every boardstate the Robot will have preferences in which
// columns to play
// Sometimes this will not be possible
// So we order the columns by preferences

let orderPrefernces nums =
    let n = List.length nums
    List.zip nums [ 0 .. n - 1 ] // now we have [(a,0); (b,1); ...]
    |> List.sortBy fst

//let updatePolicy  =

let boardToInput (board: Board) (player: Player) =
    Array.concat board
    |> List.ofArray
    |> List.map (fun x ->
        match x with
        | Empty -> 0.0
        | Occupied x when x = player -> 1.0
        | _ -> -1.0)

//Takes a list of preferences and tries to play them in sequence
//Returns the new board and the column that has been played into
//If the board is full or no preferences we given return None
let rec playPreferences (board: Board) (mightBeFull: bool) (player: Player) (pref: int list): (Board * int) option =
    // Array.forall (fun x -> x = Empty) (Array.concat board) checks if the borad is full
    if mightBeFull
       && (Array.forall (fun x -> x <> Empty) (Array.concat board)) then
        failwith "Board seems full"
        None
    else
        match pref with
        | x :: xs ->
            let (newBoard, legalMove) = tryPlayInCol x board player
            if legalMove then Some(newBoard, x) else playPreferences board false player xs
        | [] ->
            failwith "no preference got played"
            None


let playATurn (robot: Robot) (board: Board) =
    // Add exploration via epsilon
    let orderedPreferences =
        NeuralNetwork.prediction (robot.Policy, boardToInput board robot.Color)
        |> orderPrefernces
        |> List.map snd

    let afterPlay =
        playPreferences board true robot.Color orderedPreferences

    match afterPlay with
    | Some (newBoard, playedCol) ->
        (newBoard,
         { robot with
               Memory =
                   { BoardState = board
                     PlayedMove = playedCol }
                   :: robot.Memory })
    | None -> failwith "Something went wrong"

let sizeOfInput (net: NeuralNetwork): int =
    net
    |> LazyList.head
    |> LazyList.head
    |> fun x -> x.weights
    |> LazyList.length
    |> fun x -> x - 1

let lazyLast vals = (LazyList.rev >> LazyList.head) vals

let listLast vals = (List.rev >> List.head) vals


let sizeOfOutput (net: NeuralNetwork): int = net |> lazyLast |> LazyList.length

// Play a game between two AIs and return the winning player or None if it is a draw
let rec playAGame (p1: Robot) (p2: Robot) (board: Board): Player option =
    //drawBoardToConsole board
    // Some checks if the AIs can actually play against each other
    if p1.Color = p2.Color then
        failwith "Players must be different colors"
    elif sizeOfInput p1.Policy <> sizeOfInput p2.Policy then
        failwith "Players must play on same sized board"
    elif sizeOfOutput p2.Policy <> sizeOfOutput p2.Policy then
        failwith "Players must play on board with same number of columns"
    else
        let (newBoard, p1WithMemory) = playATurn p1 board
        drawBoardToConsole newBoard
        if winner newBoard p1.Color then Some p1.Color else playAGame p2 p1WithMemory newBoard







[<EntryPoint>]
let main argv =
    let width, height = 7, 6
    let middleLayer: int = (width * height + height) / 2 + 1
    let layers = [ width * height; middleLayer; width ]
    let testNet = NeuralNetwork.init layers

    let robo1 =
        { Policy = testNet
          Color = PlayerA
          Memory = []
          ExploreEpsilon = 0.2 }

    let robo2 = { robo1 with Color = PlayerB }

    let board =
        createBoard width height (fun x y -> Empty)

    drawBoardToConsole board
    let result = playAGame robo1 robo2 board
    printfn "%A has won" result
    0

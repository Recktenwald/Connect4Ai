module AIPlay

open System
open System.IO
open FSharpx.Collections
open Synapses
open Connect4
open FSharp.Json

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
    |> List.sortBy (fun x -> -(fst x))




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
            if x > numOfCols board then printfn "Tried to play %A" x else ()
            let (newBoard, legalMove) = tryPlayInCol x board player
            if legalMove then Some(newBoard, x) else playPreferences board false player xs
        | [] ->
            failwith "no preference got played"
            None


let rec playATurn (robot: Robot) (board: Board) (rnd: Random) =
    // Add exploration via epsilon
    if rnd.NextDouble() < robot.ExploreEpsilon then
        let playedInCol = rnd.Next(numOfCols board)
        //printfn "Playing Random: %A" playedInCol

        let (newBoard, legalMove) =
            tryPlayInCol playedInCol board robot.Color

        if legalMove then
            (newBoard,
             { robot with
                   Memory =
                       { BoardState = board
                         PlayedMove = playedInCol }
                       :: robot.Memory })
        else
            playATurn robot board rnd

    else
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
                       { BoardState = board // here we remember the old boardstate
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
// also return the winner and the loser
let rec playAGame (p1: Robot) (p2: Robot) (board: Board) (rnd: Random): Player option * Robot * Robot =
    //drawBoardToConsole board
    // Some checks if the AIs can actually play against each other
    if p1.Color = p2.Color then
        failwith "Players must be different colors"
    elif sizeOfInput p1.Policy <> sizeOfInput p2.Policy then
        failwith "Players must play on same sized board"
    elif sizeOfOutput p2.Policy <> sizeOfOutput p2.Policy then
        failwith "Players must play on board with same number of columns"
    else
        let (newBoard, p1WithMemory) = playATurn p1 board rnd
        //drawBoardToConsole newBoard
        if winner newBoard p1.Color then (Some p1.Color, p1, p2) else playAGame p2 p1WithMemory newBoard rnd


// Training
// Draws give 0 point
// Winning moves get 1 points
// Losers get -1 points

// From knowing the outcome of the game we go through the memory of the player
// And with another neural net (which might be the same as the one the player used)
// We compute an 'expected outcome' for each position.
// This comes from the Bellmann Equation
// The output layer's i-th node should approximate the Q function of action i,
// which is what we are trying to learn. So for each action we have to compute the Q-value
// For illegal moves we give them a value of -1

let qValueAction (trainingNet: NeuralNetwork) (board: Board) (player: Player) (discountRate: float) (action: int) =
    let (newBoard, legalMove) = tryPlayInCol action board player
    if not legalMove then
        -1.0
    elif winner newBoard player then
        1.0
        + discountRate
        * (List.max (NeuralNetwork.prediction (trainingNet, boardToInput newBoard player)))
    else
        discountRate
        * (List.max (NeuralNetwork.prediction (trainingNet, boardToInput newBoard player)))

let qValueVector trainingNet board player discountRate =
    let cols = numOfCols board
    [ 0 .. cols - 1 ]
    |> List.map (qValueAction trainingNet board player discountRate)

(* let generateExpectedOutput (robot: Robot) (trainingNet: NeuralNetwork) (outcome: Player option) (discountRate: float) =
    match robot.Memory with
    | [] -> []
    | x :: xs ->
        let reward =
            match outcome with
            | None -> 0.0
            | Some player -> if player = robot.Color then 1.0 else -1.0

        let turnToOutput (turn: Turn) =
            discountRate
            * (List.max (NeuralNetwork.prediction (trainingNet, boardToInput turn.BoardState robot.Color)))

        let lastMove = reward + turnToOutput x
        let restMoves = List.map turnToOutput xs
        lastMove :: restMoves *)

// We now need a function that lets to robots play against each other n times
// Then trains both of them with all the previous game
// Then we can continue this whole cycle as often as we want
// Also some logging should be done,
// but I would have to come up with some type of fixed benchmarking AI

(* let playOneGameAndTrain (net1:NeuralNetwork) (net2:NeuralNetwork) (epsilon:float) =
    let robo1:Robot =
        {
            Color = PlayerA
            Policy = net1
            Memory = []
            ExploreEpsilon = epsilon
        }
     let robo2:Robot =
        {
            Color = PlayerB
            Policy = net2
            Memory = []
            ExploreEpsilon = epsilon
        }
    let emptyBoard = createBoard width height (fun x y -> Empty)

    playAGame robo1 robo2 emptyBoard *)



let playGameAndTrain
    (net: NeuralNetwork)
    (trainingNet: NeuralNetwork)
    (epsilon: float)
    (discountRate: float)
    (learningRate: float)
    (rnd: Random)
    =
    let robo1: Robot =
        { Color = PlayerA
          ExploreEpsilon = epsilon
          Memory = []
          Policy = net }

    let robo2 =
        { Color = PlayerB
          Policy = net
          Memory = []
          ExploreEpsilon = epsilon }

    let emptyBoard =
        createBoard width height (fun x y -> Empty)


    let (_, winner, loser) = playAGame robo1 robo2 emptyBoard rnd

    let inputOutputPairW =
        winner.Memory
        |> List.map (fun (turn: Turn) ->
            (boardToInput turn.BoardState winner.Color,
             qValueVector trainingNet turn.BoardState winner.Color discountRate))

    let inputOutputPairL =
        loser.Memory
        |> List.map (fun (turn: Turn) ->
            (boardToInput turn.BoardState loser.Color, qValueVector trainingNet turn.BoardState loser.Color discountRate))

    inputOutputPairW
    @ inputOutputPairL
    |> List.fold (fun newNet pair -> NeuralNetwork.fit (newNet, learningRate, fst pair, snd pair)) net

// The difference is that we keep the same Training net for N Games and decrease epsilon as we go
let playNGamesAndTrain
    (net: NeuralNetwork)
    (epsilon: float)
    (numOfGames: int)
    (discountRate: float)
    (learningRate: float)
    (rnd: Random)
    =
    let epsdec = epsilon / float numOfGames

    let trainedNet =
        [ 0 .. numOfGames - 1 ]
        |> List.fold (fun newNet i ->
            playGameAndTrain newNet net (epsilon - (float i) * epsdec) discountRate learningRate rnd) net

    (*  let file =
        @"trainedNets\net"
        + DateTime.Now.ToString("yyyy-MM-dd-HH-mm-ss")
        + ".json"

    File.AppendAllText(file, NeuralNetwork.toJson trainedNet) *)
    trainedNet

let playMRoundsOfNGamesAndTrain
    (net: NeuralNetwork)
    (numOfRounds: int)
    (numOfGames: int)
    (discountRate: float)
    (learningRate: float)
    (rnd: Random)
    =
    let trainedNet =
        [ 0 .. numOfRounds - 1 ]
        |> List.fold (fun newNet _ -> playNGamesAndTrain newNet 0.0 numOfGames discountRate learningRate rnd) net

    let file =
        @"trainedNets\net"
        + DateTime.Now.ToString("yyyy-MM-dd-HH-mm-ss")
        + ".json"

    File.AppendAllText(file, NeuralNetwork.toJson trainedNet)
    trainedNet

let rec askForCol n board (rnd: Random): int =
    if n > 10 then
        printfn "%s" "You had enough chances."
        rnd.Next(numOfCols board)
    else
        try
            int (Console.ReadLine())
        with :? System.InvalidOperationException ->
            printfn "%s" "Could not parse"
            askForCol (n + 1) board rnd

let rec playAgainstHuman (rnd: Random) (robo: Robot) (board: Board) (humanTurn: bool) (humanColor: Player) =
    if humanTurn then
        printfn "%s" "Which column do you want to play"
        let action = askForCol 0 board rnd
        let newBoard, legalMove = tryPlayInCol action board humanColor
        if legalMove then
            drawBoardToConsole newBoard
            if winner newBoard humanColor then
                printfn "%s" "Congratulations! You won"
                Some humanColor
            else
                playAgainstHuman rnd robo newBoard false humanColor
        else
            printfn "%s" "Illegal Move. Try again"
            playAgainstHuman rnd robo board true humanColor
    else
        let newBoard, _ = playATurn robo board rnd
        drawBoardToConsole newBoard
        if winner newBoard robo.Color then
            printfn "%s" "Too bad! Better luck next time!"
            Some robo.Color
        else
            playAgainstHuman rnd robo board true humanColor


[<EntryPoint>]
let main argv =
    let width, height = 7, 6
    let middleLayer: int = (width * height + height) / 2 + 1
    let layers = [ width * height; middleLayer; width ]
    let startnet = NeuralNetwork.init layers

    let json =
        File.ReadAllText(@"trainedNets/net2020-06-23-16-27-46.json")

    let loadedNetwork = NeuralNetwork.ofJson (json)
    let rnd = Random()

    let net0 =
        playNGamesAndTrain loadedNetwork 0.1 100 0.2 0.02 rnd

    let net1 =
        playMRoundsOfNGamesAndTrain net0 100 100 0.2 0.01 rnd

    let net2 =
        playMRoundsOfNGamesAndTrain net1 100 100 0.2 0.001 rnd
    //This is for play against humans

    let emptyBoard =
        createBoard width height (fun _ _ -> Empty)

    drawBoardToConsole emptyBoard
    printfn "%s" "Who should go first? 0 for Human, 1 for Robot"
    //This is hacky
    match askForCol 0 (createBoard 2 1 (fun x y -> Empty)) rnd with
    | 0 ->
        playAgainstHuman rnd
            { Policy = net2
              Color = PlayerB
              ExploreEpsilon = 0.0
              Memory = [] } emptyBoard true PlayerA
    | _ ->
        playAgainstHuman rnd
            { Policy = net2
              Color = PlayerA
              ExploreEpsilon = 0.0
              Memory = [] } emptyBoard false PlayerB
    |> ignore

    (* let board =
        createBoard width height (fun x y -> Empty)

    drawBoardToConsole board
    let (result, _, _) = playAGame robo1 robo2 board
    printfn "%A has won" result *)
    0

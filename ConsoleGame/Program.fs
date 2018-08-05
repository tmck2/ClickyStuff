open System
open Types
open Domain

let width = 20
let height = 10

let instructions = [
    "Clicky Stuff"
    ""
    "Instructions"
    "Arrow Keys - Move"
    "         Q - Quit"
]

type Model = {
    Cursor: Position
    Game: Game
}

let initModel () = {
    Cursor = 0,0
    Game = {
        Board = mkRandomBoard height width 1000
        Score = 0
    }
}

module UI =
    let clearScreen () =
        [0..Console.BufferHeight]
        |> List.iter (fun _ -> Console.Write (String(' ', Console.BufferWidth)))
    
    let printc color ch =
        Console.ForegroundColor <- color
        printf "%c" ch

let view {Board=board as game} =
    Console.CursorVisible <- false

    UI.clearScreen |> ignore

    board
    |> List.iteri (fun y row ->
        List.iteri (fun x cell ->
            Console.SetCursorPosition (x,y)
            match cell with 
            | None -> 
                UI.printc ConsoleColor.Black '.'
            | Some gem ->
                match gem with
                | Club -> UI.printc ConsoleColor.Black '♣'
                | Heart -> UI.printc ConsoleColor.Red '♥'
                | Diamond -> UI.printc ConsoleColor.Blue '♦'
                | Spade -> UI.printc ConsoleColor.Green '♠'
        ) row
        Console.WriteLine ()
    )

    Console.ForegroundColor <- ConsoleColor.Black
    Console.CursorVisible <- true

let rec gameLoop ({Game=game;Cursor=cursor} as model:Model) =
    let board = game.Board

    view game

    Console.SetCursorPosition cursor |> ignore

    let keyInfo = Console.ReadKey (true)

    let (x,y) = cursor

    let h,w = List.length board, List.length board.[0]

    match keyInfo.Key with
    | ConsoleKey.RightArrow ->
        gameLoop { model with Cursor = (min (x + 1) (w-1),y) }
    | ConsoleKey.LeftArrow ->
        gameLoop { model with Cursor = (max (x - 1) 0,y) }
    | ConsoleKey.DownArrow ->
        gameLoop { model with Cursor = (x,min (y+1) (h-1)) }
    | ConsoleKey.UpArrow ->
        gameLoop { model with Cursor = (x,max (y-1) 0)}
    | ConsoleKey.Spacebar ->
        gameLoop { model with Game = handleEvent (CellClicked (y,x)) game }
    | ConsoleKey.Q -> ()
    | _ -> gameLoop model

[<EntryPoint>]
let main argv =
    UI.clearScreen ()

    instructions |>
    List.iteri (fun y str ->
        Console.SetCursorPosition(25, y) |> ignore
        printf "%s" str
    )

    gameLoop (initModel ())

    0 // return an integer exit code

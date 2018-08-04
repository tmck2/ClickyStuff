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
    Board: Board
}

let initModel () = {
    Cursor = 0,0
    Board = mkRandomBoard height width 1000
}

module UI =
    let clearScreen () =
        [0..Console.BufferHeight]
        |> List.iter (fun _ -> Console.Write (String(' ', Console.BufferWidth)))
    
    let printc color ch =
        Console.ForegroundColor <- color
        printf "%c" ch

let view (model:Model) =
    Console.CursorVisible <- false

    UI.clearScreen |> ignore

    model.Board
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

let rec gameLoop model =
    view model

    Console.SetCursorPosition model.Cursor |> ignore

    let keyInfo = Console.ReadKey (true)

    let (x,y) = model.Cursor

    let h,w = List.length model.Board, List.length model.Board.[0]

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
        gameLoop { model with Board = handleEvent (CellClicked (y,x)) model.Board }
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

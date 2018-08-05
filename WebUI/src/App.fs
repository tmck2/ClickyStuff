module WebUI

open Fable.Import
open Types
open Domain

let width = 15
let height = 15

let mutable game = {
    Board = mkRandomBoard height width 1000
    Score = 0
}

let coinSound = Sound.create "../sounds/coin.wav"

let gemImages = 
    ["heart.png";"hex.png";"round.png";"star.png"]
    |> List.map (sprintf "../images/%s")

let gridContainer = Browser.document.getElementById "container"

let scoreElement = Browser.document.getElementById "score"

let imageGrid =
    [for _ in [0..height-1] ->
        [for _ in [0..width-1] ->
            Browser.document.createElement ("img")
        ]
    ]

let view game =
    let blankImage = "data:image/gif;base64,R0lGODlhAQABAAD/ACwAAAAAAQABAAACADs="
    let {Board=board;Score=score} = game
    board
    |> List.iteri (fun row cells ->
        List.iteri (fun col cell ->
            let imgSrc =
                match cell with
                | Some gem ->
                    match gem with
                    | Spade -> gemImages.[0]
                    | Diamond -> gemImages.[1]
                    | Heart -> gemImages.[2]
                    | Club -> gemImages.[3]
                | None -> blankImage
            imageGrid.[row].[col].setAttribute ("src", imgSrc)
        ) cells)
    scoreElement.innerText <- (sprintf "%i" score)    

let init() =
    let render () = view game

    render ()

    imageGrid
    |> List.iteri (fun row cells ->
        List.iteri (fun col (cell:Browser.HTMLElement) ->
            cell.addEventListener_click (fun _ ->
                let curGame = game
                game <- handleEvent (CellClicked (row,col)) game
                if not (curGame = game) then
                    Sound.play coinSound
                    render ()
            )
            gridContainer.appendChild cell |> ignore
        ) cells
    )

init()

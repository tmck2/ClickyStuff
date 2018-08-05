module WebUI

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Types
open Domain

let width = 15
let height = 15

let mutable board = mkRandomBoard height width 1000

let gemImages = 
    ["heart.png";"hex.png";"round.png";"star.png"]
    |> List.map (sprintf "../images/%s")

let gridContainer = Browser.document.getElementById "container"

let imageGrid =
    [for _ in [0..height-1] ->
        [for _ in [0..width-1] ->
            Browser.document.createElement ("img")
        ]
    ]

let coinSound =
    let audio = Browser.document.createElement "audio"
    audio.setAttribute ("src", "../sounds/coin.wav")
    audio
[<Emit("$0.play()")>]
let play audio = jsNative
let playCoinSound () =
    play coinSound

let view board =
    let blankImage = "data:image/gif;base64,R0lGODlhAQABAAD/ACwAAAAAAQABAAACADs="
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

let init() =
    let render () = view board

    render ()

    imageGrid
    |> List.iteri (fun row cells ->
        List.iteri (fun col (cell:Browser.HTMLElement) ->
            cell.addEventListener_click (fun _ ->
                board <- handleEvent (CellClicked (row,col)) board
                playCoinSound ()
                render ()
            )
            gridContainer.appendChild cell |> ignore
        ) cells
    )

init()
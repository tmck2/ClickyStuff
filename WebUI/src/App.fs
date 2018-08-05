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
    [for row in [0..height-1] ->
        [for col in [0..width-1] ->
            let img = Browser.document.createElement ("img")
            img.setAttribute ("id", sprintf "%i-%i" row col)
            img.setAttribute ("width", "48px")
            img.setAttribute ("height", "48px")
            img
        ]
    ]

let view board =
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
                | None -> "data:image/gif;base64,R0lGODlhAQABAAD/ACwAAAAAAQABAAACADs="
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
                render ()
            )
            gridContainer.appendChild cell |> ignore
        ) cells
    )

init()

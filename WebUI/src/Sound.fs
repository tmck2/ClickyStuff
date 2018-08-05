module Sound

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

let create url =
    let audio = Browser.document.createElement "audio"
    audio.setAttribute ("src", url)
    audio

[<Emit("$0.play()")>]
let play audio = jsNative
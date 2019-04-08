#load "../.paket/load/netstandard2.0/main.group.fsx"

#if INTERACTIVE
#r "netstandard"
#endif

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React

module Editor =

    type Props =
        | OnChange of (string -> unit)
        | Value of string
        | Language of string
        | IsReadOnly of bool

    let inline editor (props: Props list) : ReactElement =
        ofImport "default" "./Editor.js" (keyValueList CaseRules.LowerFirst props) []


importSideEffects "./style.sass"

type Msg =
    | Increase
    | Decrease

type Model = int

let init _ = 0, Cmd.none

let update msg model =
   match msg with
   | Increase -> model + 1, Cmd.none
   | Decrease -> model - 1, Cmd.none

let view model dispatch =
    div [ ClassName "container" ] [
        h1 [ClassName "title is-1"] [ str "F# & Elmish" ]
        p [] [ strong [] [str "Count:"]
               ofInt model ]
        button [ OnClick (fun _ -> dispatch Increase) ] [ str "increase" ]
        button [ OnClick (fun _ -> dispatch Decrease) ] [ str "decrease" ]
    ]

// App
Program.mkProgram init update view
|> Program.withConsoleTrace
|> Program.withReactSynchronous "elmish-app"
|> Program.run

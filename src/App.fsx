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
open Elmish.HMR
open Thoth.Json
open System

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
    | SourceUpdated of string
    | GetTokens
    | TokenReceived of string
    | LineSelected of int
    | TokenSelected of int
    | PlayScroll of int

type Token =
    { ColorClass: string
      CharClass: string
      FSharpTokenTriggerClass: string
      TokenName: string
      LeftColumn: int
      RightColumn: int
      Tag: int
      FullMatchedLength: int }

    static member Decoder : Decoder<Token> =
          Decode.object
                (fun get ->
                  { ColorClass = get.Required.Field "ColorClass" Decode.string
                    CharClass = get.Required.Field "CharClass" Decode.string
                    FSharpTokenTriggerClass = get.Required.Field "FSharpTokenTriggerClass" Decode.string
                    TokenName = get.Required.Field "TokenName" Decode.string
                    LeftColumn = get.Required.Field "LeftColumn" Decode.int
                    RightColumn = get.Required.Field "RightColumn" Decode.int
                    Tag = get.Required.Field "Tag" Decode.int
                    FullMatchedLength = get.Required.Field "FullMatchedLength" Decode.int }
                )


type Model = 
    { Source: string
      Tokens: (Token * int) array
      ActiveLine: int option
      ActiveTokenIndex: int option }

let initSource =
    """let update msg model =
    match msg with
    | SourceUpdated source -> 
         { model with Source = source }, Cmd.none
    | GetTokens ->
         model, Cmd.OfPromise.perform getTokens model.Source TokenReceived
    | TokenReceived(tokensText) ->
         let decodingResult = Decode.fromString tokensDecoder tokensText 
         match decodingResult with
         | Ok tokens ->
             { model with Tokens = tokens}, Cmd.OfFunc.result (LineSelected 1)
         | Error error ->
             printfn "%A" error
             model, Cmd.none
     | LineSelected lineNumber ->
         { model with ActiveLine = Some lineNumber }, Cmd.none
 
     | TokenSelected tokenIndex ->
         { model with ActiveTokenIndex = Some tokenIndex}, Cmd.OfFunc.result (PlayScroll tokenIndex)
     | PlayScroll index ->
         scrollTo index // cheating
         model, Cmd.none
    """

let init _ = 
    { Source = initSource;
      Tokens = [||]
      ActiveLine = None
      ActiveTokenIndex = None }, Cmd.none

let getTokens (source: string) : JS.Promise<string> = import "getTokens" "./api"

let tokensDecoder =
    Decode.array (Decode.tuple2 Token.Decoder Decode.int)

let scrollTo (index: int) : unit = import "scrollTo" "./scrollTo.js"

let update msg model =
   match msg with
   | SourceUpdated source -> 
        { model with Source = source }, Cmd.none
   | GetTokens ->
        model, Cmd.OfPromise.perform getTokens model.Source TokenReceived
   | TokenReceived(tokensText) ->
        let decodingResult = Decode.fromString tokensDecoder tokensText 
        match decodingResult with
        | Ok tokens ->
            { model with Tokens = tokens}, Cmd.OfFunc.result (LineSelected 1)
        | Error error ->
            printfn "%A" error
            model, Cmd.none
    | LineSelected lineNumber ->
        { model with ActiveLine = Some lineNumber }, Cmd.none

    | TokenSelected tokenIndex ->
        { model with ActiveTokenIndex = Some tokenIndex}, Cmd.OfFunc.result (PlayScroll tokenIndex)
    | PlayScroll index ->
        scrollTo index // cheating
        model, Cmd.none



let navbar =
    nav [ Class "navbar" ]
        [ div [ Class "navbar-brand" ]
            [ a [ Href "/"
                  Class "navbar-item" ]
                [ strong [ ]
                    [ str "Marksmanship" ] ] ]
          div [ Class "navbar-end" ]
            [ div [ Class "navbar-item" ]
                [ a [ Href "https://github.com/nojaf/fsharp-tokens" ]
                    [ button [ Class "button is-black" ]
                        [ span [ Class "icon" ]
                            [ i [ Class " fab fa-github" ]
                                [ ] ]
                          span [ ]
                            [ str "Github" ] ] ] ] ] ]

let editor model dispatch =
    div [Id "editor"] [
        div [Id "monaco"] [
            Editor.editor [Editor.Props.OnChange (SourceUpdated >> dispatch); Editor.Props.Value model.Source]
        ]
        div [ Id "settings" ]
            [ button [ Class "button is-dark is-fullwidth"; OnClick (fun _ -> dispatch Msg.GetTokens) ]
                [ span [ Class "icon is-small" ]
                    [ i [ Class "fas fa-code" ]
                        [ ] ]
                  span [ ]
                    [ str "Get Tokens" ] ] ]
    ]

let tokenNameClass token =
    sprintf "is-%s" (token.TokenName.ToLower())

let lineToken dispatch index (token:Token) =
    div [ClassName "token"; Key (index.ToString()); OnClick (fun _ -> dispatch (TokenSelected index))] [
        span [ClassName (sprintf "tag %s"(tokenNameClass token))] [
            str token.TokenName
        ]
    ]

let line dispatch activeLine (lineNumber, tokens) =
    let tokens = 
        tokens
        |> Array.mapi (fun idx (token,_) -> lineToken dispatch idx token)

    let className =
        match activeLine with
        | Some al when (al = lineNumber) -> "line active"
        | _ -> "line"

    div [ClassName className; Key (sprintf "line-%d" lineNumber); OnClick (fun _ -> LineSelected lineNumber |> dispatch)] [
        div [ClassName "line-number"] [ofInt lineNumber]
        div [ClassName "tokens" ] [ofArray tokens]
    ]

let tokens model dispatch =
    let lines = 
        model.Tokens
        |> Array.groupBy snd
        |> Array.map (line dispatch model.ActiveLine)

    div [Id "tokens"] [
        h2 [ Class "title is-4" ] [ str "Tokens" ]
        div [Class "lines"] [ofArray lines]
    ]

let private tokenDetailRow label content =
    tr [] [
        td [] [
            strong [] [str label]
        ]
        td [] [
            content
        ]
    ]

let private splitOnNewLines (value: string) = value.Split([|System.Environment.NewLine;"\n";"\r";|], StringSplitOptions.RemoveEmptyEntries)

let contentOfToken model token =
    let lines = splitOnNewLines model.Source
    match model.ActiveLine with
    | Some line ->
        lines.[line - 1].Substring(token.LeftColumn, token.RightColumn - token.LeftColumn + 1)
    | None -> 
        String.Empty

let tokenDetail model index (token, _) =
    let className = 
        tokenNameClass token
        |> sprintf "tag is-large %s"

    div [ClassName "detail"; Key (index.ToString())] [
        h3 [ClassName className] [
            str token.TokenName
            small [ClassName "is-size-6"] [sprintf "(%d)" index |> str]
        ]
        table [ClassName "table is-striped"] [
            tbody [] [
                tokenDetailRow "TokenName" (str token.TokenName)
                tokenDetailRow "LeftColumn" (ofInt token.LeftColumn)
                tokenDetailRow "RightColumn" (ofInt token.RightColumn)
                tokenDetailRow "Content" (code [] [contentOfToken model token |> str])
                tokenDetailRow "ColorClass" (str token.ColorClass)
                tokenDetailRow "CharClass" (str token.CharClass)
                tokenDetailRow "Tag" (ofInt token.Tag)
                tokenDetailRow "FullMatchedLength" (ofInt token.FullMatchedLength)
            ]
        ]
    ]

let details model =
    model.ActiveLine
    |> Option.map (fun activeLine ->
        let details =
            model.Tokens
            |> Array.filter (snd >> ((=) activeLine))
            |> Array.mapi (tokenDetail model)

        div [Id "details"] [
            h2 [ Class "title is-4" ] [ str "Details of line "; span [ Class "has-text-grey" ] [ofInt activeLine] ]
            div [ Class "detail-container" ] [ofArray details]
        ]
    )
    |> ofOption

let view model dispatch =
    fragment [] [
        navbar
        main [ClassName "columns is-gapless"] [
            div [ClassName "column is-one-third"] [
                editor model dispatch
            ]
            div [ClassName "column is-two-thirds"] [
                div [Id "results"] [
                    tokens model dispatch
                    details model
                ]
            ]
        ]
    ]

// App
Program.mkProgram init update view
|> Program.withConsoleTrace
|> Program.withReactSynchronous "elmish-app"
|> Program.run
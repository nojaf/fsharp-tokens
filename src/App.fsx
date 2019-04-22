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

type TokenInfo =
    { ColorClass: string
      CharClass: string
      FSharpTokenTriggerClass: string
      TokenName: string
      LeftColumn: int
      RightColumn: int
      Tag: int
      FullMatchedLength: int }

    static member Decoder : Decoder<TokenInfo> =
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

type Token =
    { TokenInfo: TokenInfo
      LineNumber: int
      Content: string }

    static member Decoder : Decoder<Token> =
          Decode.object
                  (fun get ->
                    { TokenInfo = get.Required.Field "TokenInfo" TokenInfo.Decoder
                      LineNumber = get.Required.Field "LineNumber" Decode.int
                      Content = get.Required.Field "Content" Decode.string }
                  )

type Model = 
    { Source: string
      Tokens: Token array 
      ActiveLine: int option
      ActiveTokenIndex: int option }

let init _ = 
    { Source = "let a = 7";
      Tokens = [||]
      ActiveLine = None
      ActiveTokenIndex = None }, Cmd.none

let getTokens (source: string) : JS.Promise<string> = import "getTokens" "./api"

let scrollTo (index: int) : unit = import "scrollTo" "./scrollTo.js"

let update msg model =
   match msg with
   | SourceUpdated source -> 
        { model with Source = source }, Cmd.none
   | GetTokens ->
        model, Cmd.OfPromise.perform getTokens model.Source TokenReceived
   | TokenReceived(tokensText) ->
        let decodingResult = Decode.fromString (Decode.array Token.Decoder) tokensText 
        match decodingResult with
        | Ok tokens ->
            let cmd = 
                if (Array.length tokens) = 1 then
                    Cmd.OfFunc.result (LineSelected 1)
                else
                    Cmd.none

            { model with Tokens = tokens}, cmd
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
    sprintf "is-%s" (token.TokenInfo.TokenName.ToLower())

let lineToken dispatch index (token:Token) =
    div [ClassName "token"; Key (index.ToString()); OnClick (fun _ -> dispatch (TokenSelected index))] [
        span [ClassName (sprintf "tag %s"(tokenNameClass token))] [
            str token.TokenInfo.TokenName
        ]
    ]

let line dispatch activeLine (lineNumber, tokens) =
    let tokens = 
        tokens
        |> Array.mapi (fun idx token -> lineToken dispatch idx token)

    let className =
        match activeLine with
        | Some al when (al = lineNumber) -> "line active"
        | _ -> "line"

    div [ClassName className; Key (sprintf "line-%d" lineNumber); OnClick (fun _ -> LineSelected lineNumber |> dispatch)] [
        div [ClassName "tokens" ] [ofArray tokens]
    ]

let tokens model dispatch =
    let lines = 
        model.Tokens
        |> Array.groupBy (fun t -> t.LineNumber)
        |> Array.map (line dispatch model.ActiveLine)

    div [Id "tokens"] [
        h2 [ Class "title is-4" ] [ str "Tokens" ]
        ofArray lines
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

let tokenDetail model index token =
    let className = 
        tokenNameClass token
        |> sprintf "tag is-large %s"

    let { TokenName = tokenName; LeftColumn = leftColumn; RightColumn = rightColumn; 
          ColorClass = colorClass; CharClass = charClass; Tag = tag
          FullMatchedLength = fullMatchedLength } = token.TokenInfo

    div [ClassName "detail"; Key (index.ToString())] [
        h3 [ClassName className] [
            str token.TokenInfo.TokenName
            small [ClassName "is-size-6"] [sprintf "(%d)" index |> str]
        ]
        table [ClassName "table is-striped"] [
            tbody [] [
                tokenDetailRow "TokenName" (str tokenName)
                tokenDetailRow "LeftColumn" (ofInt leftColumn)
                tokenDetailRow "RightColumn" (ofInt rightColumn)
                tokenDetailRow "Content" (code [] [token.Content |> str])
                tokenDetailRow "ColorClass" (str colorClass)
                tokenDetailRow "CharClass" (str charClass)
                tokenDetailRow "Tag" (ofInt tag)
                tokenDetailRow "FullMatchedLength" (ofInt fullMatchedLength)
            ]
        ]
    ]

let details model =
    model.ActiveLine
    |> Option.map (fun activeLine ->
        let details =
            model.Tokens
            |> Array.filter (fun t -> t.LineNumber = activeLine)
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
            div [ClassName "column is-two-third"] [
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
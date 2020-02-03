#load "../../.paket/load/netstandard2.0/client/client.group.fsx"
#load "../Shared.fs"

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Elmish.HMR
open Thoth.Json
open Fetch
open Nojaf.FSharpTokens
open System

module Monaco =
    module Editor =
        /// A range in the editor. This interface is suitable for serialization.
        [<AllowNullLiteral>]
        type IRange =
            /// Line number on which the range starts (starts at 1).
            abstract startLineNumber: float with get, set
            /// Column on which the range starts in line `startLineNumber` (starts at 1).
            abstract startColumn: float with get, set
            /// Line number on which the range ends.
            abstract endLineNumber: float with get, set
            /// Column on which the range ends in line `endLineNumber`.
            abstract endColumn: float with get, set

        [<RequireQualifiedAccess>]
        type ScrollType =
            | Smooth = 0
            | Immediate = 1

        [<AllowNullLiteral>]
        type IStandaloneCodeEditor =
            abstract setSelection: selection:IRange -> unit
            abstract revealRangeInCenter: range:IRange * ?scrollType:ScrollType -> unit

module Editor =

    type Props =
        | OnChange of (string -> unit)
        | Value of string
        | Language of string
        | IsReadOnly of bool
        | GetEditor of (Monaco.Editor.IStandaloneCodeEditor -> unit)

    let inline editor (props: Props list): ReactElement =
        ofImport "default" "./js/Editor.js" (keyValueList CaseRules.LowerFirst props) []


importSideEffects "./style.sass"

let mutable monacoInstance: Monaco.Editor.IStandaloneCodeEditor option = None

type Msg =
    | SourceUpdated of string
    | GetTokens
    | TokenReceived of string
    | LineSelected of int
    | TokenSelected of int
    | PlayScroll of int
    | DefinesUpdated of string
    | VersionFound of string
    | NetworkException of exn

type TokenInfo =
    { ColorClass: string
      CharClass: string
      FSharpTokenTriggerClass: string
      TokenName: string
      LeftColumn: int
      RightColumn: int
      Tag: int
      FullMatchedLength: int }

    static member Decoder: Decoder<TokenInfo> =
        Decode.object (fun get ->
            { ColorClass = get.Required.Field "ColorClass" Decode.string
              CharClass = get.Required.Field "CharClass" Decode.string
              FSharpTokenTriggerClass = get.Required.Field "FSharpTokenTriggerClass" Decode.string
              TokenName = get.Required.Field "TokenName" Decode.string
              LeftColumn = get.Required.Field "LeftColumn" Decode.int
              RightColumn = get.Required.Field "RightColumn" Decode.int
              Tag = get.Required.Field "Tag" Decode.int
              FullMatchedLength = get.Required.Field "FullMatchedLength" Decode.int })

type Token =
    { TokenInfo: TokenInfo
      LineNumber: int
      Content: string }

    static member Decoder: Decoder<Token> =
        Decode.object (fun get ->
            { TokenInfo = get.Required.Field "TokenInfo" TokenInfo.Decoder
              LineNumber = get.Required.Field "LineNumber" Decode.int
              Content = get.Required.Field "Content" Decode.string })

type Model =
    { Source: string
      Defines: string
      Tokens: Token array
      ActiveLine: int option
      ActiveTokenIndex: int option
      IsLoading: bool
      Version: string }

let private backendRoot =
#if DEBUG
    "http://localhost:7071/api"
#else
    "https://azfun-fsharptokens.azurewebsites.net/api"
#endif

let private getVersion() =
    let url = sprintf "%s/%s" backendRoot "version"
    Fetch.fetch url []
    |> Promise.bind (fun res -> res.text())
    |> Promise.map (fun (json: string) ->
        match Decode.fromString Decode.string json with
        | Ok v -> v
        | Error e -> failwithf "%A" e)

let private setGetParam (key, value): unit = import "setGetParam" "./js/urlUtils.js"
let private encodeUrl (x: string): string = import "compressToEncodedURIComponent" "./js/urlUtils.js"
let private decodeUrl (x: string): string = import "decompressFromEncodedURIComponent" "./js/urlUtils.js"

let private splitDefines (value: string) =
    value.Split([| ' '; ';' |], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

let private modelToParseRequest (model: Model): Shared.GetTokensRequest =
    let defines = splitDefines model.Defines
    { Defines = defines
      SourceCode = model.Source }

let private encodeGetTokensRequest (value: Shared.GetTokensRequest): JsonValue =
    Encode.object
        [ "defines",
          (value.Defines
           |> List.map Encode.string
           |> List.toArray
           |> Encode.array)
          "sourceCode", Encode.string value.SourceCode ]

let private decodeGetTokensRequest: Decoder<Shared.GetTokensRequest> =
    Decode.object (fun get ->
        { Defines = get.Required.Field "defines" (Decode.list Decode.string)
          SourceCode = get.Required.Field "sourceCode" Decode.string })

let private updateUrl (model: Model) _ =
    let json = Encode.toString 2 ((modelToParseRequest >> encodeGetTokensRequest) model)
    setGetParam ("data", encodeUrl json)

let getTokens (request: Shared.GetTokensRequest): JS.Promise<string> =
    let url = sprintf "%s/%s" backendRoot "get-tokens"
    let json = Encode.toString 4 (encodeGetTokensRequest request)

    fetch url
        [ RequestProperties.Body(!^json)
          RequestProperties.Method HttpMethod.POST ]
    |> Promise.bind (fun res -> res.text())

let private initialModel =
    { Source = ""
      Defines = ""
      Tokens = [||]
      ActiveLine = None
      ActiveTokenIndex = None
      IsLoading = false
      Version = "??" }

let private restoreModelFromUrl() =
    match Browser.Dom.window.location.search with
    | x when System.String.IsNullOrWhiteSpace(x) -> initialModel, None
    | x ->
        let search = x.Substring(1) // remove ?

        let keyValues =
            search.Split('&')
            |> Array.map (fun kv -> kv.Split('=').[0], kv.Split('=').[1])
            |> Map.ofArray
        match Map.tryFind "data" keyValues with
        | Some data ->
            let json = JS.decodeURIComponent (data) |> decodeUrl
            let urlInfo = Decode.fromString decodeGetTokensRequest json
            match urlInfo with
            | Result.Ok u ->
                { initialModel with
                      Source = u.SourceCode
                      Defines = String.concat " " u.Defines }, Some u
            | Error err ->
                printfn "%A" err
                initialModel, None
        | None -> initialModel, None

let init _ =
    let model, parseRequest = restoreModelFromUrl()

    let cmd =
        let parseCmd =
            match parseRequest with
            | Some pr -> Cmd.OfPromise.either getTokens pr Msg.TokenReceived Msg.NetworkException
            | None -> Cmd.none

        let versionCmd = Cmd.OfPromise.either getVersion () VersionFound NetworkException
        Cmd.batch [ versionCmd; parseCmd ]

    model, cmd

let scrollTo (index: int): unit = import "scrollTo" "./js/scrollTo.js"

let highLightCode lineNumber tokenStart tokenEnd =
    monacoInstance
    |> Option.iter (fun e ->
        let range: Monaco.Editor.IRange =
            jsOptions (fun r ->
                r.startLineNumber <- (float) lineNumber
                r.endLineNumber <- (float) lineNumber
                r.startColumn <- (tokenStart + 1) |> (float)
                r.endColumn <- (tokenEnd + 2) |> (float))

        e.setSelection (range)
        e.revealRangeInCenter (range, Monaco.Editor.ScrollType.Smooth))

let update msg model =
    match msg with
    | SourceUpdated source ->
        { model with Source = source }, Cmd.none
    | GetTokens ->
        let cmd =
            let requestCmd = Cmd.OfPromise.perform getTokens (modelToParseRequest model) TokenReceived
            let updateUrlCmd = Cmd.ofSub (updateUrl model)
            Cmd.batch [ requestCmd; updateUrlCmd ]

        { model with IsLoading = true }, cmd
    | TokenReceived(tokensText) ->
        let decodingResult = Decode.fromString (Decode.array Token.Decoder) tokensText
        match decodingResult with
        | Ok tokens ->
            let cmd =
                if (Array.length tokens) = 1 then Cmd.OfFunc.result (LineSelected 1) else Cmd.none

            { model with
                  Tokens = tokens
                  IsLoading = false }, cmd
        | Error error ->
            printfn "%A" error
            { model with IsLoading = false }, Cmd.none
    | LineSelected lineNumber ->
        { model with ActiveLine = Some lineNumber }, Cmd.none

    | TokenSelected tokenIndex ->
        model.ActiveLine
        |> Option.iter (fun activeLine ->
            let token =
                model.Tokens
                |> Array.filter (fun t -> t.LineNumber = activeLine)
                |> Array.item tokenIndex
                |> fun t -> t.TokenInfo
            highLightCode activeLine token.LeftColumn token.RightColumn)

        { model with ActiveTokenIndex = Some tokenIndex }, Cmd.OfFunc.result (PlayScroll tokenIndex)
    | PlayScroll index ->
        scrollTo index // cheating
        model, Cmd.none
    | DefinesUpdated defines ->
        { model with Defines = defines }, Cmd.none
    | VersionFound v -> { model with Version = v }, Cmd.none
    | NetworkException e ->
        JS.console.error e
        model, Cmd.none

let navbar version =
    let brand = sprintf "F# tokens - FCS version %s" version
    nav [ Class "navbar" ]
        [ div [ Class "navbar-brand" ]
              [ a
                  [ Href "/"
                    Class "navbar-item" ] [ strong [] [ str brand ] ] ]
          div [ Class "navbar-end" ]
              [ div [ Class "navbar-item" ]
                    [ a [ Href "https://github.com/nojaf/fsharp-tokens" ]
                          [ button [ Class "button is-black" ]
                                [ span [ Class "icon" ] [ i [ Class " fab fa-github" ] [] ]
                                  span [] [ str "Github" ] ] ] ] ] ]

let editor model dispatch =
    let loaderOrButton =
        if model.IsLoading then
            progress
                [ ClassName "progress is-small is-primary"
                  Max "100" ] [ str "50%" ]
        else
            button
                [ Class "button is-dark is-fullwidth"
                  OnClick(fun _ -> dispatch Msg.GetTokens) ]
                [ span [ Class "icon is-small" ] [ i [ Class "fas fa-code" ] [] ]
                  span [] [ str "Get Tokens" ] ]

    div [ Id "editor" ]
        [ div [ Id "monaco" ]
              [ Editor.editor
                  [ Editor.Props.OnChange(SourceUpdated >> dispatch)
                    Editor.Props.Value model.Source
                    Editor.Props.GetEditor(fun e -> monacoInstance <- Some e) ] ]

          div
              [ Id "defines"
                ClassName "input" ]
              [ input
                  [ ClassName "input"
                    Placeholder "Enter your defines separated with a space"
                    Value model.Defines
                    OnChange(fun ev ->
                        ev.Value
                        |> DefinesUpdated
                        |> dispatch) ] ]
          div [ Id "settings" ] [ loaderOrButton ] ]

let tokenNameClass token =
    sprintf "is-%s" (token.TokenInfo.TokenName.ToLower())

let lineToken dispatch index (token: Token) =
    div
        [ ClassName "token"
          Key(index.ToString())
          OnClick(fun _ -> dispatch (TokenSelected index)) ]
        [ span [ ClassName(sprintf "tag %s" (tokenNameClass token)) ] [ str token.TokenInfo.TokenName ] ]

let line dispatch activeLine (lineNumber, tokens) =
    let tokens =
        tokens |> Array.mapi (fun idx token -> lineToken dispatch idx token)

    let className =
        match activeLine with
        | Some al when (al = lineNumber) -> "line active"
        | _ -> "line"

    div
        [ ClassName className
          Key(sprintf "line-%d" lineNumber)
          OnClick(fun _ -> LineSelected lineNumber |> dispatch) ]
        [ div [ ClassName "line-number" ] [ ofInt lineNumber ]
          div [ ClassName "tokens" ] [ ofArray tokens ] ]

let tokens model dispatch =
    let lines =
        model.Tokens
        |> Array.groupBy (fun t -> t.LineNumber)
        |> Array.map (line dispatch model.ActiveLine)

    div [ Id "tokens" ]
        [ h2 [ Class "title is-4" ] [ str "Tokens" ]
          div [ Class "lines" ] [ ofArray lines ] ]

let private tokenDetailRow label content =
    tr []
        [ td [] [ strong [] [ str label ] ]
          td [] [ content ] ]

let tokenDetail model dispatch index token =
    let className =
        tokenNameClass token |> sprintf "tag is-large %s"

    let { TokenName = tokenName; LeftColumn = leftColumn; RightColumn = rightColumn; ColorClass = colorClass;
          CharClass = charClass; Tag = tag; FullMatchedLength = fullMatchedLength } = token.TokenInfo

    div
        [ ClassName "detail"
          Key(index.ToString()) ]
        [ h3
            [ ClassName className
              OnClick(fun _ -> Msg.TokenSelected index |> dispatch) ]
              [ str token.TokenInfo.TokenName
                small [ ClassName "is-size-6" ] [ sprintf "(%d)" index |> str ] ]
          table [ ClassName "table is-striped" ]
              [ tbody []
                    [ tokenDetailRow "TokenName" (str tokenName)
                      tokenDetailRow "LeftColumn" (ofInt leftColumn)
                      tokenDetailRow "RightColumn" (ofInt rightColumn)
                      tokenDetailRow "Content" (pre [] [ code [] [ str token.Content ] ])
                      tokenDetailRow "ColorClass" (str colorClass)
                      tokenDetailRow "CharClass" (str charClass)
                      tokenDetailRow "Tag" (ofInt tag)
                      tokenDetailRow "FullMatchedLength"
                          (span [ ClassName "has-text-weight-semibold" ] [ ofInt fullMatchedLength ]) ] ] ]

let details model dispatch =
    model.ActiveLine
    |> Option.map (fun activeLine ->
        let details =
            model.Tokens
            |> Array.filter (fun t -> t.LineNumber = activeLine)
            |> Array.mapi (tokenDetail model dispatch)

        div [ Id "details" ]
            [ h2 [ Class "title is-4" ]
                  [ str "Details of line "
                    span [ Class "has-text-grey" ] [ ofInt activeLine ] ]
              div [ Class "detail-container" ] [ ofArray details ] ])
    |> ofOption

let view model dispatch =
    fragment []
        [ navbar model.Version
          main [ ClassName "columns is-gapless" ]
              [ div [ ClassName "column is-one-third" ] [ editor model dispatch ]
                div [ ClassName "column is-two-thirds" ]
                    [ div [ Id "results" ]
                          [ tokens model dispatch
                            details model dispatch ] ] ] ]

// App
Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"
|> Program.run

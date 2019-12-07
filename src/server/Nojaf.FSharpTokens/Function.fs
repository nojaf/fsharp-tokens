module Nojaf.FSharpTokens.Function

open FSharp.Compiler.SourceCodeServices
open Microsoft.AspNetCore.Http
open Microsoft.Azure.WebJobs
open Microsoft.Azure.WebJobs.Extensions.Http
open Microsoft.Extensions.Logging
open Newtonsoft.Json.Linq
open System.IO
open System.Net
open System.Net.Http
open Thoth.Json.Net
open Fantomas.TriviaTypes
open Fantomas
open Nojaf.FSharpTokens.Shared

module GetTokens =
    let private encodeEnum<'t> (value: 't) = value.ToString() |> Encode.string

    let private decodeEnum<'t> (path: string) (token: JsonValue) =
        let v = token.Value<string>()
        match System.Enum.Parse(typeof<'t>, v, true) with
        | :? 't as t -> Ok t
        | _ ->
            let typeName = typeof<'t>.Name
            Error(DecoderError(sprintf "Cannot decode to %s" typeName, ErrorReason.BadField(path, token)))

    let private toJson (tokens: Token list) =
        let extra =
            Extra.empty
            |> Extra.withCustom encodeEnum<FSharpTokenColorKind> decodeEnum<FSharpTokenColorKind>
            |> Extra.withCustom encodeEnum<FSharpTokenCharKind> decodeEnum<FSharpTokenCharKind>
            |> Extra.withCustom encodeEnum<FSharpTokenTriggerClass> decodeEnum<FSharpTokenTriggerClass>
        tokens
        |> List.map
            (Thoth.Json.Net.Encode.Auto.generateEncoder<Token> (false, extra))
        |> Encode.list
        |> Encode.toString 4

    let getTokens (req: HttpRequest) =
        let content = using (new StreamReader(req.Body)) (fun stream -> stream.ReadToEnd())
        let model = Decode.fromString GetTokensRequest.Decode content
        match model with
        | Ok model ->
            let json =
                TokenParser.tokenize model.Defines model.SourceCode
                |> fst
                |> toJson
            new HttpResponseMessage(HttpStatusCode.OK, Content = new StringContent(json, System.Text.Encoding.UTF8, "application/json"))
        | Error err ->
            printfn "Failed to decode: %A" err
            new HttpResponseMessage(HttpStatusCode.BadRequest, Content = new StringContent(err, System.Text.Encoding.UTF8, "text/plain"))

    let getVersion () =
        let version =
            let assembly = typeof<FSharp.Compiler.SourceCodeServices.FSharpChecker>.Assembly
            let version = assembly.GetName().Version
            sprintf "%i.%i.%i" version.Major version.Minor version.Revision
        let json =
            Encode.string version
            |> Encode.toString 4
        new HttpResponseMessage(HttpStatusCode.OK, Content = new StringContent(json, System.Text.Encoding.UTF8, "application/json"))

    let notFound () =
        let json =
            Encode.string "Not found"
            |> Encode.toString 4
        new HttpResponseMessage(HttpStatusCode.NotFound, Content = new StringContent(json, System.Text.Encoding.UTF8, "application/json"))

[<FunctionName("Tokens")>]
let Run([<HttpTrigger(AuthorizationLevel.Anonymous, "get", "post", Route = "{*any}")>] req: HttpRequest (*, log: ILogger*)) =
    //log.LogInformation("F# HTTP trigger function processed a request.")
    printfn ("F# HTTP trigger function processed a request..")
    let path = req.Path.Value.ToLower()
    let method = req.Method.ToUpper()

    match method, path with
    | "POST", "/api/get-tokens" -> GetTokens.getTokens req
    | "GET", "/api/version" -> GetTokens.getVersion ()
    | _ -> GetTokens.notFound ()



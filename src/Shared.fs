module Nojaf.FSharpTokens.Shared

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

type GetTokensRequest =
    { Defines: string list
      SourceCode: string }

#if FABLE_COMPILER
    static member Encode (value:GetTokensRequest) :JsonValue =
        Encode.object [
            "defines", (value.Defines |> List.map Encode.string |> List.toArray |> Encode.array)
            "sourceCode", Encode.string value.SourceCode
        ]

#else
    static member Decode : Decoder<GetTokensRequest> =
        Decode.object
            (fun get -> 
                { Defines = get.Required.Field "defines" (Decode.list Decode.string)
                  SourceCode = get.Required.Field "sourceCode" Decode.string }
            )
#endif

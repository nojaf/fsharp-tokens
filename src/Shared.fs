module Nojaf.FSharpTokens.Shared
type GetTokensRequest =
    { Defines: string list
      SourceCode: string }
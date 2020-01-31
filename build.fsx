#r "paket: groupref build //"
#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.JavaScript
open Fantomas.FakeHelpers
open Fantomas.FormatConfig
open System

let clientPath = Path.getFullName "./src/client"
let setYarnWorkingDirectory (args: Yarn.YarnParams) = { args with WorkingDirectory = clientPath }
let serverPath = Path.getFullName "./src/server/Nojaf.FSharpTokens"
let sharedFile = Path.getFullName "./src/Shared.fs"

module Paket =
    let private runCmd cmd args =
        CreateProcess.fromRawCommand cmd args
        |> Proc.run
        |> ignore

    let private paket args = runCmd "dotnet" ("paket" :: args)

    let ``generate load script``() = paket [ "generate-load-scripts"; "-f"; "netstandard2.0"; "-t"; "fsx" ]

Target.create "Clean" (fun _ ->
    Shell.rm_rf (clientPath </> ".fable")
    Shell.rm_rf (serverPath </> "bin")
    Shell.rm_rf (serverPath </> "obj"))

Target.create "Yarn" (fun _ -> Yarn.installPureLock setYarnWorkingDirectory)

Target.create "Paket" (fun _ ->
    Paket.restore (fun p -> { p with ToolType = ToolType.CreateLocalTool() })
    Shell.rm_rf (".paket" </> "load")
    Paket.``generate load script``())

Target.create "Build" (fun _ ->
    Yarn.exec "build" setYarnWorkingDirectory
    DotNet.build (fun config -> { config with Configuration = DotNet.BuildConfiguration.Release })
        (serverPath </> "Nojaf.FSharpTokens.fsproj"))

Target.create "Watch" (fun _ ->
    let compileFable =
        async {
            Yarn.exec "start" setYarnWorkingDirectory
        }

    let stopFunc() = System.Diagnostics.Process.GetProcessesByName("func") |> Seq.iter (fun p -> p.Kill())

    let rec startFunc() =
        match ProcessUtils.tryFindPath [] "func" with
        | None -> failwith "func command was not found"
        | Some funcPath ->
            let dirtyWatcher: IDisposable ref = ref null

            let watcher =
                !!(serverPath </> "*.fs") ++ (serverPath </> "*.fsproj")
                |> ChangeWatcher.run (fun changes ->
                    printfn "FILE CHANGE %A" changes
                    if !dirtyWatcher <> null then
                        (!dirtyWatcher).Dispose()
                        stopFunc()
                        startFunc())


            dirtyWatcher := watcher

            CreateProcess.fromRawCommand funcPath [ "host";"start";"--csharp" ]
            |> CreateProcess.withWorkingDirectory serverPath
            |> Proc.run
            |> ignore

    let runAzureFunction = async { startFunc() }

    Async.Parallel [ runAzureFunction ; compileFable ]
    |> Async.Ignore
    |> Async.RunSynchronously)

Target.create "Format" (fun _ ->
    let fantomasConfig =
        { FormatConfig.Default with
              KeepNewlineAfter = true }

    let fsharpFiles = !!(serverPath </> "*.fs") ++ (clientPath </> "fsharp" </> "*.fsx") ++ sharedFile

    fsharpFiles
    |> formatCode fantomasConfig
    |> Async.RunSynchronously
    |> printfn "Formatted F# files: %A"

    Yarn.exec "format" setYarnWorkingDirectory)

Target.create "Deploy" (fun _ -> Yarn.exec "deploy" setYarnWorkingDirectory)

Target.create "Default" ignore

"Clean" ==> "Paket" ==> "Yarn" ==> "Build"

"Paket" ==> "Yarn" ==> "Watch"

Target.runOrDefault "Build"

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

let pwd = Shell.pwd()
let clientPath = Path.getFullName "./src/client"
let setYarnWorkingDirectory (args: Yarn.YarnParams) = { args with WorkingDirectory = clientPath }
let serverPath = Path.getFullName "./src/server/Nojaf.FSharpTokens"
let sharedFile = Path.getFullName "./src/Shared.fs"
let publishPath = pwd </> "deploy"
let serverProject = (serverPath </> "Nojaf.FSharpTokens.fsproj")

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
    Shell.rm_rf (serverPath </> "obj")
    Shell.rm_rf publishPath)

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

    let fsharpFiles = !!(serverPath </> "*.fs") ++ (clientPath </> "*.fsx") ++ sharedFile

    fsharpFiles
    |> formatCode fantomasConfig
    |> Async.RunSynchronously
    |> printfn "Formatted F# files: %A"

    Yarn.exec "format" setYarnWorkingDirectory)

module Azure =
    let az parameters =
        let azPath = ProcessUtils.findPath [] "az"
        CreateProcess.fromRawCommand azPath parameters
        |> Proc.run
        |> ignore

    let func parameters =
        let funcPath = ProcessUtils.findPath [] "func"
        CreateProcess.fromRawCommand funcPath parameters
        |> CreateProcess.withWorkingDirectory serverPath
        |> Proc.run
        |> ignore

Target.create "DeployServer" (fun _ ->
//    let resourceGroup = Environment.environVar "AZ_RESOURCE_GROUP"
//    let armFile = pwd </> "infrastructure" </> "azuredeploy.json"
//    let functionappName = Environment.environVar "AZ_FUNCTIONAPP"
//    let serverFarmName = Environment.environVar "AZ_SERVERFARM"
//    let applicationInsightsName = Environment.environVar "AZ_APPINSIGHTS"
//    let storageName = Environment.environVar "AZ_STORAGE"
//    let corsUrl = Environment.environVar "AZ_CORS"
//
//    Azure.az ["group";"deployment"; "validate";"-g"
//              resourceGroup; "--template-file"; armFile
//              "--parameters"; (sprintf "functionappName=%s" functionappName)
//              "--parameters"; (sprintf "serverFarmName=%s" serverFarmName)
//              "--parameters"; (sprintf "applicationInsightsName=%s" applicationInsightsName)
//              "--parameters"; (sprintf "storageName=%s" storageName)
//              "--parameters"; (sprintf "appUrl=%s" corsUrl)]
//
//    Azure.az ["group";"deployment"; "create";"-g"
//              resourceGroup; "--template-file"; armFile
//              "--parameters"; (sprintf "functionappName=%s" functionappName)
//              "--parameters"; (sprintf "serverFarmName=%s" serverFarmName)
//              "--parameters"; (sprintf "applicationInsightsName=%s" applicationInsightsName)
//              "--parameters"; (sprintf "storageName=%s" storageName)
//              "--parameters"; (sprintf "appUrl=%s" corsUrl)]

    DotNet.publish (fun config -> { config with
                                        Configuration = DotNet.BuildConfiguration.Release
                                        OutputPath = Some publishPath }) serverProject

    Zip.createZip "./deploy" "func.zip" "" Zip.DefaultZipLevel false (!! "./deploy/*.*" ++ "./deploy/**/*.*")
    Shell.mv "func.zip" "./deploy/func.zip"

    // Azure.az ["functionapp";"deployment";"source";"config-zip";"-g";resourceGroup;"-n";functionappName;"--src";"./deploy/func.zip"]
)

Target.create "Deploy" (fun _ -> Yarn.exec "deploy" setYarnWorkingDirectory)

Target.create "Default" ignore

"Clean" ==> "Paket" ==> "Yarn" ==> "Build"

"Paket" ==> "Yarn" ==> "Watch"

Target.runOrDefault "Build"

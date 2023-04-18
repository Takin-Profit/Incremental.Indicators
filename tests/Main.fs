module Incremental.Indicators.Tests.Main

open Expecto
open Hopac
open Logary
open Logary.Configuration
open Logary.Adapters.Facade
open Logary.Targets

[<EntryPoint>]
let main argv =
    let logary =
        Config.create "TakinProfit.Incremental.Indicators.Tests" "localhost"
        |> Config.targets [ LiterateConsole.create LiterateConsole.empty "console" ]
        |> Config.processing (Events.events |> Events.sink [ "console" ])
        |> Config.build
        |> run

    LogaryFacadeAdapter.initialise<Expecto.Logging.Logger> logary

    // Invoke Expecto:
    Tests.runTestsInAssemblyWithCLIArgs [] argv

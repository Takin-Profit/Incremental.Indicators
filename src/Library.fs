namespace Incremental.Indicators

open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("Incremental.Indicators.Tests")>]
do ()

module Say =
    let hello name = printfn "Hello %s" name

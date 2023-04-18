module TakinProfit.Indicators.SMA

open System
open FSharp.Data.Adaptive

type SMAResult = { Date: DateTime; Sma: double }
let mutable private _results: aset<SMAResult> = cset []

let results = _results

let calculate =
    _results <- Quotes.all |> ASet.map (fun r -> { Date = DateTime.Now; Sma = 0.0 })
    AVal.force results.Content

let recalculate =
    Quotes.all |> ASet.map id
    AVal.force results.Content


let toTimeSpan periodSize =
    match periodSize with
    | "1" -> TimeSpan.Zero

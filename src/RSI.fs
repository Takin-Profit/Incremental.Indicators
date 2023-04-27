module Incremental.Indicators.RSI

open Quotes
open Results

open FSharp.Data.Adaptive
open System

type RsiResult = { Value: double; Date: DateTime }

let private rsi (n: int) =
    let mutable averageGain: double = 0.0
    let mutable averageLoss: double = 0.0
    let mutable prevClose: double = 0.0
    let mutable prevGain: double = 0.0
    let mutable prevLoss: double = 0.0
    let mutable rs: double = 0.0
    let mutable rsiValue: double = 0.0

    fun (d: DateTime) (price: double) ->
        if prevClose = 0.0 then
            prevClose <- price
            rsiValue <- 0.0
        else
            let change = price - prevClose
            prevClose <- price
            let gain = if change > 0.0 then change else 0.0
            let loss = if change < 0.0 then abs change else 0.0

            if prevGain = 0.0 then
                prevGain <- gain
                prevLoss <- loss
                averageGain <- gain
                averageLoss <- loss
            else
                averageGain <- (prevGain * (double n - 1.0) + gain) / double n
                averageLoss <- (prevLoss * (double n - 1.0) + loss) / double n
                prevGain <- averageGain
                prevLoss <- averageLoss
                rs <- averageGain / averageLoss
                rsiValue <- 100.0 - 100.0 / (1.0 + rs)

        { Date = d; Value = rsiValue }


type private RSI =
    private
        { results: RsiResult aset }

    member x.Results = x.results

    member x.reCalculate = AVal.force x.Results.Content

    static member Create a b = { results = b }


let create a (quotes: Quote cset) =
    if a < 1 then
        Error("LookBack periods must be greater than 0 for RSI.")
    else
        let rsi = rsi a
        let results = quotes |> ASet.map (fun q -> rsi q.Date (double q.Close))
        let t = RSI.Create a results
        Ok(fun () -> t.Results, (fun () -> t.reCalculate))

let inline from<'TResult when 'TResult: (member Date: DateTime) and 'TResult: (member Value: double)>
    a
    (results: 'TResult cset)
    (rsiResults: RsiResult aset)
    =
    if a < 1 then
        Error("LookBack periods must be greater than 0 for RSI.")
    else
        let rsi = rsi a
        let rsiOfResults = results |> ASet.map (fun q -> rsi q.Date q.Value)
        let synced = syncIndex rsiOfResults rsiResults SyncType.Prepend
        let t = RSI.Create a synced
        Ok(fun () -> t.Results, (fun () -> t.reCalculate))

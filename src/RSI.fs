module Incremental.Indicators.RSI

open Quotes

open FSharp.Data.Adaptive
open System

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

        {| Date = d; Value = rsiValue |}


type private RSI =
    private
        { results: {| Date: DateTime; Value: double |} aset }

    member private x.Results = x.results

    member x.calculate = AVal.force x.Results.Content

    static member Create a b =
        if a < 1 then
            Error("LookBack periods must be greater than 0 for RSI.")
        else
            Ok { results = b }


let create a (quotes: Quote cset) =
    let rsi = rsi a
    let results = quotes |> ASet.map (fun q -> rsi q.Date (double q.Close))
    let calc = RSI.Create a results

    match calc with
    | Error a -> Error a
    | Ok c -> Ok(fun () -> c.calculate)

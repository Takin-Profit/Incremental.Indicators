module Incremental.Indicators.RSI

open Incremental.Indicators.Quotes
open FSharp.Data.Adaptive


let private rsi (n: int) =
    let mutable averageGain: double = 0.0
    let mutable averageLoss: double = 0.0
    let mutable prevClose: double = 0.0
    let mutable prevGain: double = 0.0
    let mutable prevLoss: double = 0.0
    let mutable rs: double = 0.0
    let mutable rsiValue: double = 0.0

    fun (price: double) ->
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

        rsiValue

type private RSI =
    private
        { results: aset<double> }

    member private x.Results = x.results

    member x.calculate = AVal.force x.Results.Content

    static member Create a b =
        if a < 1 then
            Error("LookBack periods must be greater than 0 for RSI.")
        else
            Ok { results = b }


let create a (b: cset<Quote>) =
    if a < 1 then
        Error("LookBack periods must be greater than 0 for RSI.")
    else
        let rsi = rsi a
        let quotes = quotesToTuple
        Ok RSI.Create a

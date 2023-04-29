module Incremental.Indicators.RSI

open Quotes

open FSharp.Data.Adaptive
open System

type RsiResult = { Value: double; Date: DateTime }

let internal calculateRsi (quotes: QuoteD seq) (period: int) =
    let quoteList = List.ofSeq quotes
    let length = List.length quoteList
    let mutable avgGain = 0.0
    let mutable avgLoss = 0.0
    let mutable rs = 0.0
    let mutable prevQuote = quoteList[0]
    let mutable rsiResults = []

    // Calculate initial gains and losses for the first period
    let mutable gain = 0.0
    let mutable loss = 0.0

    for i in 1..period do
        let curQuote = quoteList[i]
        let diff = curQuote.Close - prevQuote.Close

        if diff > 0.0 then
            gain <- gain + diff
        else
            loss <- loss + (abs diff)

        prevQuote <- curQuote

    avgGain <- gain / float period
    avgLoss <- loss / float period

    if avgLoss = 0.0 then
        rs <- 100.0
    else
        rs <- 100.0 - (100.0 / (1.0 + (avgGain / avgLoss)))

    // Add initial RSI result
    let initialResult = { Value = rs; Date = prevQuote.Date }
    rsiResults <- [ initialResult ]

    // Calculate RSI for remaining quotes
    for i in period .. (length - 1) do
        let curQuote = quoteList[i]
        let diff = curQuote.Close - prevQuote.Close

        if diff > 0.0 then
            avgGain <- ((avgGain * float (period - 1)) + diff) / float period
            avgLoss <- (avgLoss * float (period - 1)) / float period
        else
            avgGain <- (avgGain * float (period - 1)) / float period
            avgLoss <- ((avgLoss * float (period - 1)) + (abs diff)) / float period

        if avgLoss = 0.0 then
            rs <- 100.0
        else
            rs <- 100.0 - (100.0 / (1.0 + (avgGain / avgLoss)))

        let result = { Value = rs; Date = curQuote.Date }
        rsiResults <- rsiResults @ [ result ]
        prevQuote <- curQuote

    rsiResults |> List.toArray

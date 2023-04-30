module Incremental.Indicators.RSI

open FSharp.Data.Adaptive
open System
open Util

type RsiResult = { Value: double; Date: DateTime }

let calculateRSI (lookBack: int) (quotes: Quotes) =
    let mutable gain: double = 0.0
    let mutable loss: double = 0.0
    let mutable rs: double = 0.0
    let mutable rsi: double = 0.0
    let mutable count = 0
    let mutable index = 0
    let mutable buffer = Array.create lookBack (double 0.0, double 0.0)

    alist {
        for quote in quotes.DoublePrecis do
            count <- count + 1
            let! previousQuote = getVal (count - 1) QuoteD.Empty quotes.DoublePrecis
            let diff = quote.Close - previousQuote.Close

            if diff > 0.0 then
                gain <- gain + diff
                let prevGain, prevLoss = buffer[index]
                loss <- prevLoss + max 0.0 (prevGain - (prevGain + diff))
                buffer[index] <- (diff, prevLoss)
            else
                loss <- loss + abs diff
                let prevGain, prevLoss = buffer[index]
                gain <- prevGain + max 0.0 (prevLoss - (prevLoss + abs diff) / double lookBack)
                buffer[index] <- (prevGain, abs diff)

            index <- (index + 1) % lookBack

            if count > lookBack then
                rs <- gain / loss
                rsi <- 100.0 - (100.0 / (1.0 + rs))
            else
                rsi <- Double.NaN

            yield { Value = rsi; Date = quote.Date }
    }

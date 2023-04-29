module Incremental.Indicators.SMA

open System
open FSharp.Data.Adaptive
open Incremental.Indicators.Types
open Quotes

type SMAResult = { Date: DateTime; Value: double }

let internal calcSMA (lookBack: int) (quotes: Quote alist) =
    let newQuotes = toQuoteDList quotes
    let mutable count = 0

    alist {
        for quote in newQuotes do
            count <- count + 1

            if count >= lookBack then
                let offset = count - lookBack

                let period =
                    AList.sub offset lookBack newQuotes
                    |> quoteDListToToTuples CandlePart.Close
                    |> AList.map snd

                let! res = AList.average period

                { Date = quote.Date; Value = res }
    }

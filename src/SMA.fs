module Incremental.Indicators.SMA

open System
open FSharp.Data.Adaptive

type SMAResult = { Date: DateTime; Value: double }

let internal calcSMA (lookBack: int) (quotes: Quotes) =

    let mutable count = 0

    alist {
        for quote in quotes.DoublePrecis do
            count <- count + 1

            if count >= lookBack then
                let offset = count - lookBack

                let period = AList.sub offset lookBack quotes.Close

                let! res = AList.average period

                { Date = quote.Date; Value = res }
    }

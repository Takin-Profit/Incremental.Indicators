module Incremental.Indicators.SMA

open System
open FSharp.Data.Adaptive
open Types

let internal calcSMA (lookBack: int) (series: Series) =

    let mutable count = 0

    alist {
        for t in series do
            count <- count + 1

            if count >= lookBack then
                let offset = count - lookBack

                let period = AList.sub offset lookBack series

                let! res = AList.average period

                {| Date = quote.Date; Value = res |}
            else
                {| Date = quote.Date
                   Value = double Double.NaN |}
    }

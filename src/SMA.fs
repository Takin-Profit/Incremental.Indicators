module Incremental.Indicators.SMA

open System
open FSharp.Data.Adaptive


let internal calcSMA (lookBack: int) (series: Series) : Series =

    let mutable count = 0

    alist {
        for t in series do
            count <- count + 1

            if count >= lookBack then
                let offset = count - lookBack

                let period = AList.sub offset lookBack (Series.values series)

                let! res = AList.average period

                {| Date = t.Date; Value = res |}
            else
                {| Date = t.Date
                   Value = double Double.NaN |}
    }

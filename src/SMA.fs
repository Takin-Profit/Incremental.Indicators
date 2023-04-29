module Incremental.Indicators.SMA

open System
open FSharp.Data.Adaptive

type SMAResult = { Date: DateTime; Sma: double }

let calcSMA (period: int) =
    let mutable sum: double = 0.0
    let mutable count = 0
    let mutable buffer = []

    fun (price: double) ->
        sum <- sum + price
        count <- count + 1
        buffer <- buffer @ [ price ]

        if count > period then
            let head = List.head buffer
            sum <- sum - head
            buffer <- List.tail buffer
            count <- count - 1

        sum / double (min count period)

module Incremental.Indicators.Tests.Candle

open Expecto
open System
open Incremental.Indicators
open FSharp.Data.Adaptive

[<Tests>]
let candleTests =
    let quotes = TestData.getMismatch |> Option.defaultValue AList.empty

    let candles = Candle.toCandleResults quotes |> AList.force

    testList
        "candle tests"
        [ testCase "candles list should have proper quantities"
          <| fun _ ->
              let result = candles.Count
              Expect.equal result 502 "should be 502" ]

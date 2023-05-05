module Incremental.Indicators.Tests.Candle

open Expecto
open System
open Incremental.Indicators
open FSharp.Data.Adaptive
open System.Globalization

[<Tests>]
let candleTests =
    let quotes = TestData.getMismatch |> Option.defaultValue AList.empty

    let candles = Candle.toCandleResults quotes |> AList.force

    testList
        "candle tests"
        [ testCase "candles list should have proper quantities"
          <| fun _ ->
              let result = candles.Count
              Expect.equal result 502 "should be 502"

          testCase "first candle date should match"
          <| fun _ ->
              let firstDate =
                  DateTime.ParseExact("01/18/2016", "MM/dd/yyyy", CultureInfo("en-US"))

              Expect.equal candles[0].Date firstDate "dates should match"

          testCase "last candle date should match"
          <| fun _ ->
              let lastDate = DateTime.ParseExact("12/31/2018", "MM/dd/yyyy", CultureInfo("en-US"))

              Expect.equal candles[candles.Count - 1].Date lastDate "dates should match" ]

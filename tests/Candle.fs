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

              Expect.equal candles[candles.Count - 1].Date lastDate "dates should match"

          testCase "spot candle date should match"
          <| fun _ ->
              let spotDate = DateTime.ParseExact("03/16/2017", "MM/dd/yyyy", CultureInfo("en-US"))

              Expect.equal candles[50].Date spotDate "dates should match"

          ]

[<Tests>]
let moreCandleTests =
    let quotes = TestData.getDefault 502 |> Option.defaultValue AList.empty

    let candles = Candle.toCandleResults quotes |> AList.force

    testList
        "candle tests extended"
        [ testCase "candles list should have proper quantities"
          <| fun _ ->
              let result = candles.Count
              Expect.equal result 502 "should be 502"

          testCase "candles should have correct values"
          <| fun _ ->
              let res0 = candles[0]

              Expect.equal res0.Candle.Close 212.8m "close price should match"
              Expect.equal res0.Candle.Size 1.83m "candle size be correct"
              Expect.equal res0.Candle.Body 0.19m "candle body should match"
              Expect.equal res0.Candle.UpperWick 0.55m "upperWick should be correct"
              Expect.equal res0.Candle.LowerWick 1.09m "lowerWick should be correct"
              Expect.equal (Math.Round(res0.Candle.BodyPct, 5)) 0.10383 "body percent should be correct"
              Expect.equal (Math.Round(res0.Candle.UpperWickPct, 5)) 0.30055 "upperWick percent should be correct"
              Expect.equal (Math.Round(res0.Candle.LowerWickPct, 5)) 0.59563 "lowerWick percent should be correct"
              Expect.isTrue res0.Candle.IsBullish "should calculate bullishness correctly"
              Expect.isFalse res0.Candle.IsBearish "should calculate bearishness correctly"


          ]

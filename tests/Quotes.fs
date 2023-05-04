module Incremental.Indicators.Tests.Quotes

open Expecto
open Incremental.Indicators.Quotes
open Incremental.Indicators.Types
open Incremental.Indicators.Util
open Incremental.Indicators
open System
open FSharp.Data.Adaptive

[<Tests>]
let aggregateByTimeFrameTests =
    let intraDay = TestData.getIntraDay 1564
    let testQuotes = Option.defaultValue AList.empty intraDay

    let fifteenMin =
        aggregateByTimeFrame TimeFrame.FifteenMin testQuotes
        |> Result.defaultValue AList.empty

    let sampleQuotes =
        [ { Quote.Date = DateTime(2023, 5, 1, 12, 0, 0)
            Open = 10m
            High = 15m
            Low = 8m
            Close = 12m
            Volume = 100m }
          { Quote.Date = DateTime(2023, 5, 1, 12, 5, 0)
            Open = 12m
            High = 16m
            Low = 9m
            Close = 13m
            Volume = 120m }
          { Quote.Date = DateTime(2023, 5, 1, 12, 10, 0)
            Open = 13m
            High = 17m
            Low = 10m
            Close = 14m
            Volume = 140m } ]

    testList
        "aggregateByTimeFrame function tests"
        [ testCase "aggregates quotes by 5 minutes"
          <| fun _ ->
              let quotes = AList.ofList sampleQuotes
              let timeFrame = TimeFrame.FiveMin

              let result =
                  aggregateByTimeFrame timeFrame quotes |> Result.defaultValue AList.empty

              let expectedResult =
                  [ { Quote.Date = DateTime(2023, 5, 1, 12, 0, 0)
                      Open = 10m
                      High = 15m
                      Low = 8m
                      Close = 12m
                      Volume = 100m }
                    { Quote.Date = DateTime(2023, 5, 1, 12, 5, 0)
                      Open = 12m
                      High = 16m
                      Low = 9m
                      Close = 13m
                      Volume = 120m }
                    { Quote.Date = DateTime(2023, 5, 1, 12, 10, 0)
                      Open = 13m
                      High = 17m
                      Low = 10m
                      Close = 14m
                      Volume = 140m } ]
                  |> IndexList.ofList

              Expect.equal (AList.force result) expectedResult "The function should aggregate quotes by 5 minutes"

          testCase "aggregates quotes by month"
          <| fun _ ->
              let quotes = AList.ofList sampleQuotes
              let timeFrame = TimeFrame.Month

              let result =
                  aggregateByTimeFrame timeFrame quotes |> Result.defaultValue AList.empty

              let expectedResult =
                  [ { Quote.Date = DateTime(2023, 5, 1)
                      Open = 10m
                      High = 17m
                      Low = 8m
                      Close = 14m
                      Volume = 360m } ]
                  |> IndexList.ofList

              Expect.equal (AList.force result) expectedResult "The function should aggregate quotes by month"

          testCase "aggregates quotes returns proper quantities"
          <| fun _ ->

              let count = AList.count fifteenMin |> AVal.force

              Expect.equal count 108 "aggregate should return proper count"

          testCase "quotes should have correct values"
          <| fun _ ->
              let result1 = getVal 0 Quote.Empty fifteenMin |> AVal.force
              Expect.equal result1.Date (DateTime.Parse("2020-12-15 09:30")) "should have correct date"
              Expect.equal result1.Open 367.40m "should have correct open"
              Expect.equal result1.High 367.775m "should have correct high"
              Expect.equal result1.Low 367.02m "should have correct low"
              Expect.equal result1.Close 367.24m "should have correct close"
              Expect.equal result1.Volume 2401786m "should have correct volume"

              let result2 = getVal 1 Quote.Empty fifteenMin |> AVal.force
              Expect.equal result2.Date (DateTime.Parse("2020-12-15 09:45")) "should have correct date"
              Expect.equal result2.Open 367.25m "should have correct open"
              Expect.equal result2.High 367.44m "should have correct high"
              Expect.equal result2.Low 366.69m "should have correct low"
              Expect.equal result2.Close 366.86m "should have correct close"
              Expect.equal result2.Volume 1669983m "should have correct volume" ]


[<Tests>]
let aggregateByTimeSpanTests =
    testList
        "aggregateByTimeSpan tests"
        [ testCase "Aggregate quotes by valid TimeSpan"
          <| fun _ ->
              let quotes =
                  AList.ofSeq
                      [ { Quote.Empty with
                            Date = DateTime(2023, 5, 1, 9, 0, 0)
                            Open = 100m
                            High = 200m
                            Low = 50m
                            Close = 150m
                            Volume = 500m }
                        { Quote.Empty with
                            Date = DateTime(2023, 5, 1, 9, 30, 0)
                            Open = 110m
                            High = 210m
                            Low = 60m
                            Close = 160m
                            Volume = 600m }
                        { Quote.Empty with
                            Date = DateTime(2023, 5, 1, 10, 0, 0)
                            Open = 120m
                            High = 220m
                            Low = 70m
                            Close = 170m
                            Volume = 700m } ]

              let timeSpan = TimeSpan.FromHours(1.0)

              let expected =
                  AList.ofSeq
                      [ { Quote.Empty with
                            Date = DateTime(2023, 5, 1, 9, 0, 0)
                            Open = 100m
                            High = 210m
                            Low = 50m
                            Close = 160m
                            Volume = 1100m }
                        { Quote.Empty with
                            Date = DateTime(2023, 5, 1, 10, 0, 0)
                            Open = 120m
                            High = 220m
                            Low = 70m
                            Close = 170m
                            Volume = 700m } ]
                  |> AList.force

              let actualResult = aggregateByTimeSpan timeSpan quotes

              match actualResult with
              | Ok actual -> Expect.equal (AList.force actual) expected "Expected aggregated quotes for valid TimeSpan"
              | Error msg -> failwithf $"Expected Ok, got Error: %s{msg}"

          testCase "Aggregate quotes by zero TimeSpan"
          <| fun _ ->
              let quotes =
                  AList.ofSeq
                      [ { Quote.Empty with
                            Date = DateTime(2023, 5, 1, 9, 0, 0)
                            Open = 100m
                            High = 200m
                            Low = 50m
                            Close = 150m
                            Volume = 500m }
                        { Quote.Empty with
                            Date = DateTime(2023, 5, 1, 9, 30, 0)
                            Open = 110m
                            High = 210m
                            Low = 60m
                            Close = 160m
                            Volume = 600m } ]

              let timeSpan = TimeSpan.Zero
              let actualResult = aggregateByTimeSpan timeSpan quotes

              match actualResult with
              | Ok _ -> failwith "Expected Error, got Ok"
              | Error msg ->
                  Expect.stringContains msg "usable new size value" "Expected error message for zero TimeSpan" ]

[<Tests>]
let quoteDListToToTuplesTests =
    testList
        "quoteDListToToTuples tests"
        [ testCase "Convert QuoteD list to tuples for CandlePart.OHL3"
          <| fun _ ->
              let quoteDList =
                  AList.ofSeq
                      [ { QuoteD.Empty with
                            Date = DateTime(2023, 5, 1)
                            Open = 100.0
                            High = 200.0
                            Low = 50.0
                            Close = 150.0
                            Volume = 500.0 }
                        { QuoteD.Empty with
                            Date = DateTime(2023, 5, 2)
                            Open = 110.0
                            High = 210.0
                            Low = 60.0
                            Close = 160.0
                            Volume = 600.0 } ]

              let expected =
                  AList.ofSeq
                      [ (DateTime(2023, 5, 1), (100.0 + 200.0 + 50.0) / 3.0)
                        (DateTime(2023, 5, 2), (110.0 + 210.0 + 60.0) / 3.0) ]
                  |> AList.force

              let actual = quoteDListToToTuples CandlePart.OHL3 quoteDList |> AList.force
              Expect.equal actual expected "Expected tuples from QuoteD list for CandlePart.OHL3"

          testCase "Convert QuoteD list to tuples for CandlePart.OHLC4"
          <| fun _ ->
              let quoteDList =
                  AList.ofSeq
                      [ { QuoteD.Empty with
                            Date = DateTime(2023, 5, 1)
                            Open = 100.0
                            High = 200.0
                            Low = 50.0
                            Close = 150.0
                            Volume = 500.0 }
                        { QuoteD.Empty with
                            Date = DateTime(2023, 5, 2)
                            Open = 110.0
                            High = 210.0
                            Low = 60.0
                            Close = 160.0
                            Volume = 600.0 } ]

              let expected =
                  AList.ofSeq
                      [ (DateTime(2023, 5, 1), (100.0 + 200.0 + 50.0 + 150.0) / 4.0)
                        (DateTime(2023, 5, 2), (110.0 + 210.0 + 60.0 + 160.0) / 4.0) ]
                  |> AList.force

              let actual = quoteDListToToTuples CandlePart.OHLC4 quoteDList |> AList.force
              Expect.equal actual expected "Expected tuples from QuoteD list for CandlePart.OHLC4" ]

[<Tests>]
let toQuoteDListTests =
    testList
        "toQuoteDList tests"
        [ testCase "Convert Quote list to QuoteD list"
          <| fun _ ->
              let quotes =
                  AList.ofSeq
                      [ { Quote.Empty with
                            Date = DateTime(2023, 5, 1)
                            Open = 100m
                            High = 200m
                            Low = 50m
                            Close = 150m
                            Volume = 500m }
                        { Quote.Empty with
                            Date = DateTime(2023, 5, 2)
                            Open = 110m
                            High = 210m
                            Low = 60m
                            Close = 160m
                            Volume = 600m } ]

              let expected =
                  AList.ofSeq
                      [ { QuoteD.Empty with
                            Date = DateTime(2023, 5, 1)
                            Open = 100.0
                            High = 200.0
                            Low = 50.0
                            Close = 150.0
                            Volume = 500.0 }
                        { QuoteD.Empty with
                            Date = DateTime(2023, 5, 2)
                            Open = 110.0
                            High = 210.0
                            Low = 60.0
                            Close = 160.0
                            Volume = 600.0 } ]
                  |> AList.force

              let actual = toQuoteDList quotes |> AList.force
              Expect.equal actual expected "Expected QuoteD list from Quote list" ]

[<Tests>]
let listToTuplesTests =
    testList
        "listToTuples tests"
        [ testCase "OHL3 conversion"
          <| fun _ ->
              let quotes =
                  AList.ofSeq
                      [ { Quote.Empty with
                            Date = DateTime(2023, 5, 1)
                            Open = 100m
                            High = 200m
                            Low = 50m }
                        { Quote.Empty with
                            Date = DateTime(2023, 5, 2)
                            Open = 110m
                            High = 210m
                            Low = 60m } ]

              let expected =
                  AList.ofSeq
                      [ (DateTime(2023, 5, 1), 116.66666666666667)
                        (DateTime(2023, 5, 2), 126.66666666666667) ]
                  |> AList.force

              let actual = listToTuples CandlePart.OHL3 quotes |> AList.force
              Expect.equal actual expected "Expected tuples for OHL3 conversion"

          testCase "OHLC4 conversion"
          <| fun _ ->
              let quotes =
                  AList.ofSeq
                      [ { Quote.Empty with
                            Date = DateTime(2023, 5, 1)
                            Open = 100m
                            High = 200m
                            Low = 50m
                            Close = 150m }
                        { Quote.Empty with
                            Date = DateTime(2023, 5, 2)
                            Open = 110m
                            High = 210m
                            Low = 60m
                            Close = 160m } ]

              let expected =
                  AList.ofSeq [ (DateTime(2023, 5, 1), 125.0); (DateTime(2023, 5, 2), 135.0) ]
                  |> AList.force

              let actual = listToTuples CandlePart.OHLC4 quotes |> AList.force
              Expect.equal actual expected "Expected tuples for OHLC4 conversion" ]

[<Tests>]
let toTupleTests =
    testList
        "toTuple tests"
        [ testCase "CandlePart.Open"
          <| fun _ ->
              let quote =
                  { Quote.Empty with
                      Date = DateTime(2023, 5, 1)
                      Open = 100m }

              let expected = (DateTime(2023, 5, 1), 100.0)
              let actual = toTuple CandlePart.Open quote
              Expect.equal actual expected "Expected tuple for Open"

          testCase "CandlePart.High"
          <| fun _ ->
              let quote =
                  { Quote.Empty with
                      Date = DateTime(2023, 5, 1)
                      High = 200m }

              let expected = (DateTime(2023, 5, 1), 200.0)
              let actual = toTuple CandlePart.High quote
              Expect.equal actual expected "Expected tuple for High"

          testCase "CandlePart.Low"
          <| fun _ ->
              let quote =
                  { Quote.Empty with
                      Date = DateTime(2023, 5, 1)
                      Low = 50m }

              let expected = (DateTime(2023, 5, 1), 50.0)
              let actual = toTuple CandlePart.Low quote
              Expect.equal actual expected "Expected tuple for Low"

          testCase "CandlePart.Close"
          <| fun _ ->
              let quote =
                  { Quote.Empty with
                      Date = DateTime(2023, 5, 1)
                      Close = 150m }

              let expected = (DateTime(2023, 5, 1), 150.0)
              let actual = toTuple CandlePart.Close quote
              Expect.equal actual expected "Expected tuple for Close"

          testCase "CandlePart.Volume"
          <| fun _ ->
              let quote =
                  { Quote.Empty with
                      Date = DateTime(2023, 5, 1)
                      Volume = 300m }

              let expected = (DateTime(2023, 5, 1), 300.0)
              let actual = toTuple CandlePart.Volume quote
              Expect.equal actual expected "Expected tuple for Volume"

          testCase "CandlePart.HL2"
          <| fun _ ->
              let quote =
                  { Quote.Empty with
                      Date = DateTime(2023, 5, 1)
                      High = 200m
                      Low = 50m }

              let expected = (DateTime(2023, 5, 1), 125.0)
              let actual = toTuple CandlePart.HL2 quote
              Expect.equal actual expected "Expected tuple for HL2"

          testCase "CandlePart.HLC3"
          <| fun _ ->
              let quote =
                  { Quote.Empty with
                      Date = DateTime(2023, 5, 1)
                      High = 200m
                      Low = 50m
                      Close = 150m }

              let expected = (DateTime(2023, 5, 1), 133.33333333333334)
              let actual = toTuple CandlePart.HLC3 quote
              Expect.equal actual expected "Expected tuple for HLC3"

          testCase "CandlePart.OC2"
          <| fun _ ->
              let quote =
                  { Quote.Empty with
                      Date = DateTime(2023, 5, 1)
                      Open = 100m
                      Close = 150m }

              let expected = (DateTime(2023, 5, 1), 125.0)
              let actual = toTuple CandlePart.OC2 quote
              Expect.equal actual expected "Expected"

          testCase "CandlePart.OHL3"
          <| fun _ ->
              let quote =
                  { Quote.Empty with
                      Date = DateTime(2023, 5, 1)
                      Open = 100m
                      High = 200m
                      Low = 50m }

              let expected = (DateTime(2023, 5, 1), 116.66666666666667)
              let actual = toTuple CandlePart.OHL3 quote
              Expect.equal actual expected "Expected tuple for OHL3"

          testCase "CandlePart.OHLC4"
          <| fun _ ->
              let quote =
                  { Quote.Empty with
                      Date = DateTime(2023, 5, 1)
                      Open = 100m
                      High = 200m
                      Low = 50m
                      Close = 150m }

              let expected = (DateTime(2023, 5, 1), 125.0)
              let actual = toTuple CandlePart.OHLC4 quote
              Expect.equal actual expected "Expected tuple for OHLC4" ]

[<Tests>]
let validateTests =
    testList
        "validate tests"
        [ testCase "No duplicates"
          <| fun _ ->
              let quotes =
                  [ { Quote.Empty with
                        Date = DateTime(2023, 5, 1) }
                    { Quote.Empty with
                        Date = DateTime(2023, 5, 2) } ]

              match validate quotes with
              | Ok _ -> ()
              | Error errMsg -> failwithf $"Unexpected error: %s{errMsg}"

          testCase "Duplicates found"
          <| fun _ ->
              let quotes =
                  [ { Quote.Empty with
                        Date = DateTime(2023, 5, 1) }
                    { Quote.Empty with
                        Date = DateTime(2023, 5, 1) } ]

              match validate quotes with
              | Ok _ -> failwith "Expected error due to duplicates, but got success"
              | Error errMsg ->
                  Expect.stringContains errMsg "Duplicate date found" "Expected error message about duplicate dates"

          testCase "Empty quotes list"
          <| fun _ ->
              let quotes = []

              match validate quotes with
              | Ok _ -> ()
              | Error errMsg -> failwithf $"Unexpected error: %s{errMsg}" ]


[<Tests>]
let isValidTests =
    testList
        "isValid tests"
        [ testCase "Valid quote"
          <| fun _ ->
              let quote1 =
                  { Quote.Empty with
                      Date = DateTime(2023, 5, 1) }

              let quote2 =
                  { Quote.Empty with
                      Date = DateTime(2023, 5, 2) }

              let quotes = alist { quote1 }
              let actual = isValid quote2 quotes
              Expect.isTrue actual "Quote should be valid"

          testCase "Invalid quote"
          <| fun _ ->
              let quote =
                  { Quote.Empty with
                      Date = DateTime(2023, 5, 1) }

              let quotes = alist { quote }
              let actual = isValid quote quotes
              Expect.isFalse actual "Quote should be invalid"

          testCase "Empty quotes list"
          <| fun _ ->
              let quote =
                  { Quote.Empty with
                      Date = DateTime(2023, 5, 1) }

              let quotes = AList.empty
              let actual = isValid quote quotes
              Expect.isTrue actual "Quote should be valid when the quotes list is empty" ]

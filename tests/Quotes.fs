module Incremental.Indicators.Tests.Quotes

open Expecto
open Incremental.Indicators.Quotes
open Incremental.Indicators.Types
open Incremental.Indicators
open System
open FSharp.Data.Adaptive

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

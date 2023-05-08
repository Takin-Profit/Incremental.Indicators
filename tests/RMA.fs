module Incremental.Indicators.Tests.RMA

open FSharp.Data.Adaptive
open Incremental.Indicators
open Expecto
open System
open Incremental.Indicators.RMA
open Incremental.Indicators.Types

let internal quotes =
    TestData.getEthRMA
    |> Option.defaultValue AList.empty
    |> AList.force
    |> Quotes.create
    |> Result.defaultValue Quotes.empty

[<Tests>]
let RMATests =

    testList
        "RMA Tests"
        [ testCase "RMA should return correct number of results"
          <| fun _ ->
              let results = calcRMA 14 quotes.Series |> AList.force
              Expect.equal results.Count 1000 "should be 1027"


          testCase "quotes and results should have matching dates"
          <| fun _ ->
              let results = calcRMA 14 quotes.Series |> AList.force

              let result210 = IndexList.tryAt 210 results |> Option.defaultValue Series.emptyVal

              let quote210 = quotes.toArray[210]

              Expect.equal result210.Date quote210.Date "quotes and result should have matching dates"

          testCase "should return correct Values for eth_rma.csv"
          <| fun _ ->
              let results = calcRMA 14 quotes.Series |> AList.force

              let result210 = IndexList.tryAt 853 results |> Option.defaultValue Series.emptyVal

              // use low accuracy as pinescript indicators have different semantics
              Expect.floatClose Accuracy.low result210.Value (Helpers.roundBy 5 229.809949990314) "should be 229.809949"



          ]

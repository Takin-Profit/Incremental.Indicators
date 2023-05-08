module Incremental.Indicators.Tests.RMA

open FSharp.Data.Adaptive
open Incremental.Indicators
open Expecto
open System
open Incremental.Indicators.RMA
open Incremental.Indicators.Types

let internal quotes =
    TestData.getSpyWithRMA
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
              let results = calcRMA 5 quotes.Series |> AList.force
              Expect.equal results.Count 1026 "should be 1027"


          testCase "should return correct Values for default.csv"
          <| fun _ ->
              let results = calcRMA 977 quotes.Series |> AList.force

              let result19 = IndexList.tryAt 80 results |> Option.defaultValue Series.emptyVal

              Expect.equal (Math.Round(result19.Value, 4)) 439.305 "should be 439.305"



          ]

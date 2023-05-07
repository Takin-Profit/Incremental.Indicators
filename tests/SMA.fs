module Incremental.Indicators.Tests.SMA

open FSharp.Data.Adaptive
open Incremental.Indicators
open Expecto
open System
open Incremental.Indicators.SMA

let internal quotes =
    TestData.getDefault 502
    |> Option.defaultValue AList.empty
    |> AList.force
    |> Quotes.create
    |> Result.defaultValue Quotes.empty

[<Tests>]
let smaTests =

    testList
        "SMA Tests"
        [ testCase "sma should return correct number of results"
          <| fun _ ->
              let results = calcSMA 20 quotes.Series |> AList.force
              Expect.equal results.Count 502 "should be 502"

          testCase "should return correct number of results without NaN values"
          <| fun _ ->
              let results =
                  calcSMA 20 quotes.Series
                  |> AList.force
                  |> IndexList.filter (fun t -> not (Double.IsNaN(t.Value)))

              let actual = IndexList.count results
              Expect.equal actual 483 "should be 483"


          testCase "should return correct Values for default.csv"
          <| fun _ ->
              let results = calcSMA 20 quotes.Series |> AList.force

              let result = IndexList.tryAt 18 results |> Option.defaultValue Util.emptyResult
              let result19 = IndexList.tryAt 19 results |> Option.defaultValue Util.emptyResult
              let result24 = IndexList.tryAt 24 results |> Option.defaultValue Util.emptyResult
              let result149 = IndexList.tryAt 149 results |> Option.defaultValue Util.emptyResult
              let result249 = IndexList.tryAt 249 results |> Option.defaultValue Util.emptyResult
              let result501 = IndexList.tryAt 501 results |> Option.defaultValue Util.emptyResult

              Expect.isTrue (Double.IsNaN(result.Value)) "should be NaN"
              Expect.equal (Math.Round(result19.Value, 4)) 214.5250 "should be 214.5250"
              Expect.equal (Math.Round(result24.Value, 4)) 215.0310 "should be 215.0310"
              Expect.equal (Math.Round(result149.Value, 4)) 234.9350 "should be 234.9350"
              Expect.equal (Math.Round(result249.Value, 4)) 255.5500 "should be 255.5500"
              Expect.equal (Math.Round(result501.Value, 4)) 251.8600 "should be 251.8600"

          ]

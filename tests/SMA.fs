module Incremental.Indicators.Tests.SMA

open FSharp.Data.Adaptive
open Incremental.Indicators
open Expecto
open System
open Incremental.Indicators.SMA
open Incremental.Indicators.Types

let internal quotes =
    TestData.getDefault 502
    |> Option.defaultValue AList.empty
    |> AList.force
    |> Quotes.create
    |> Result.defaultValue Quotes.empty

let badData =
    TestData.getBad 502
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

              let result = IndexList.tryAt 18 results |> Option.defaultValue Series.emptyVal
              let result19 = IndexList.tryAt 19 results |> Option.defaultValue Series.emptyVal
              let result24 = IndexList.tryAt 24 results |> Option.defaultValue Series.emptyVal
              let result149 = IndexList.tryAt 149 results |> Option.defaultValue Series.emptyVal
              let result249 = IndexList.tryAt 249 results |> Option.defaultValue Series.emptyVal
              let result501 = IndexList.tryAt 501 results |> Option.defaultValue Series.emptyVal

              Expect.isTrue (Double.IsNaN(result.Value)) "should be NaN"
              Expect.equal (Math.Round(result19.Value, 4)) 214.5250 "should be 214.5250"
              Expect.equal (Math.Round(result24.Value, 4)) 215.0310 "should be 215.0310"
              Expect.equal (Math.Round(result149.Value, 4)) 234.9350 "should be 234.9350"
              Expect.equal (Math.Round(result249.Value, 4)) 255.5500 "should be 255.5500"
              Expect.equal (Math.Round(result501.Value, 4)) 251.8600 "should be 251.8600"

          testCase "CandlePart.Open tests"
          <| fun _ ->
              let results = calcSMA 20 (quotes.seriesFrom CandlePart.Open) |> AList.force

              Expect.equal results.Count 502 "should be 502 total results"

              let nonNaN = results |> IndexList.filter (fun t -> not (Double.IsNaN(t.Value)))

              Expect.equal (IndexList.count nonNaN) 483 "should be 483 nonNaN results"

              let result = IndexList.tryAt 18 results |> Option.defaultValue Series.emptyVal
              let result19 = IndexList.tryAt 19 results |> Option.defaultValue Series.emptyVal
              let result24 = IndexList.tryAt 24 results |> Option.defaultValue Series.emptyVal
              let result149 = IndexList.tryAt 149 results |> Option.defaultValue Series.emptyVal
              let result249 = IndexList.tryAt 249 results |> Option.defaultValue Series.emptyVal
              let result501 = IndexList.tryAt 501 results |> Option.defaultValue Series.emptyVal

              Expect.isTrue (Double.IsNaN(result.Value)) "should be NaN"
              Expect.equal (Math.Round(result19.Value, 4)) 214.3795 "should be 214.3795"
              Expect.equal (Math.Round(result24.Value, 4)) 214.9535 "should be 214.9535"
              Expect.equal (Math.Round(result149.Value, 4)) 234.8280 "should be 234.8280"
              Expect.equal (Math.Round(result249.Value, 4)) 255.6915 "should be 255.6915"
              Expect.equal (Math.Round(result501.Value, 4)) 253.1725 "should be 253.1725"

          testCase "CandlePart.Volume tests"
          <| fun _ ->
              let results = calcSMA 20 (quotes.seriesFrom CandlePart.Volume) |> AList.force

              Expect.equal results.Count 502 "should be 502 total results"

              let nonNaN = results |> IndexList.filter (fun t -> not (Double.IsNaN(t.Value)))

              Expect.equal (IndexList.count nonNaN) 483 "should be 483 nonNaN results"

              let result24 = IndexList.tryAt 24 results |> Option.defaultValue Series.emptyVal
              let result290 = IndexList.tryAt 290 results |> Option.defaultValue Series.emptyVal
              let result501 = IndexList.tryAt 501 results |> Option.defaultValue Series.emptyVal

              Expect.equal result24.Value 77293768.2 "should be 77293768.2"
              Expect.equal result290.Value 157958070.8 "should be 157958070.8"
              Expect.equal result501.Value 163695200 "should be 163695200"


          ]

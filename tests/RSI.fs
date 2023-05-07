module Incremental.Indicators.Tests.RSI
(*
open FSharp.Data.Adaptive
open Incremental.Indicators
open Expecto
open System
open Incremental.Indicators.RSI
open Incremental.Indicators.Util

let internal quotes =
    TestData.getDefault 502
    |> Option.defaultValue AList.empty
    |> AList.force
    |> Quotes.create
    |> Result.defaultValue Quotes.empty

[<Tests>]
let rsiTests =

    testList
        "RSI Tests"
        [ testCase "rsi should return correct number of results"
          <| fun _ ->
              let results = calcRSI 14 quotes |> AList.force
              Expect.equal results.Count 502 "should be 502"

          testCase "should have matching dates"
          <| fun _ ->
              let results = calcRSI 14 quotes
              let actual = getVal 20 RsiResult.empty results |> AVal.force
              Expect.equal actual.Value 62.0541 "should return correct PDI"

          (*
          testCase "should return correct results"
          <| fun _ ->
              let results = adx quotes
              let actual = Seq.item 18 results
              Expect.equal (Math.Round(actual.Pdi, 4)) 21.0361 "should return correct PDI"
            *)
          ]
*)

module Incremental.Indicators.Tests.ADX

open System
open Expecto
open FSharp.Data.Adaptive
open Incremental.Indicators
open Incremental.Indicators.ADX

let internal quotes =
    TestData.getDefault 502
    |> Option.defaultValue AList.empty
    |> Quotes.toQuoteDList
    |> AList.force

let adxTests =

    testList
        "ADX Tests"
        [ testCase "should have correct number of quotes"
          <| fun _ -> Expect.equal (Seq.length quotes) 502 "should return correct number of quotes"

          testCase "should have correct number of results"
          <| fun _ ->
              let results = adx quotes
              Expect.equal (Seq.length results) 502 "should return correct number of quotes"

          testCase "should have matching dates"
          <| fun _ ->
              let expected = Seq.item 19 quotes
              let results = adx quotes
              let actual = Seq.item 19 results
              Expect.equal actual.Date expected.Date "should return correct PDI"
          (*
          testCase "should return correct results"
          <| fun _ ->
              let results = adx quotes
              let actual = Seq.item 18 results
              Expect.equal (Math.Round(actual.Pdi, 4)) 21.0361 "should return correct PDI"
            *)
          ]

module Incremental.Indicators.Tests.TA

open FSharp.Data.Adaptive
open Incremental.Indicators
open Expecto
open Helpers

let internal quotes =
    TestData.getDefault 502
    |> Option.defaultValue AList.empty
    |> AList.force
    |> Quotes.create
    |> Result.defaultValue Quotes.empty

[<Tests>]
let taTests =
    testList
        "TA.change Tests"
        [ testCase "TA.change should return correct result for given period"
          <| fun _ ->
              let actual = TA.change (quotes.Series, 10) |> AVal.force
              Expect.equal (roundBy 5 actual) -8.87000 "should be -8.87000"

          testCase "TA.change should return correct result for 50 bars ago"
          <| fun _ ->
              let actual = TA.change (quotes.Series, 50) |> AVal.force
              Expect.equal (roundBy 5 actual) -28.36000 "should be -28.36000"

          testCase "TA.change should return correct result for 316 bars ago"
          <| fun _ ->
              let actual = TA.change (quotes.Series, 316) |> AVal.force
              Expect.equal (roundBy 5 actual) 5.68000 "should be 5.68000"

          testCase "TA.change should throw exception for invalid length"
          <| fun _ ->

              Expect.throws (fun () -> TA.change (quotes.Series, 0) |> ignore) "should throw an exception with 0"
              Expect.throws (fun () -> TA.change (quotes.Series, -1) |> ignore) "should throw an exception with -1"
              Expect.throws (fun () -> TA.change (quotes.Series, -2) |> ignore) "should throw an exception with -2"
              Expect.throws (fun () -> TA.change (quotes.Series, -3) |> ignore) "should throw an exception with -3"
              Expect.throws (fun () -> TA.change (quotes.Series, -4) |> ignore) "should throw an exception with -4" ]

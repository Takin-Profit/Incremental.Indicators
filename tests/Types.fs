module Incremental.Indicators.Tests.Types

open System
open Expecto
open Incremental.Indicators.Types

[<Tests>]
let toTimeSpanTests =
    testList
        "toTimeSpan tests"
        [ testCase "Month"
          <| fun _ ->
              let expected = TimeSpan.FromDays 30
              let actual = toTimeSpan TimeFrame.Month
              Expect.equal actual expected "Month should be 30 days"

          testCase "ThreeWeeks"
          <| fun _ ->
              let expected = TimeSpan.FromDays 21
              let actual = toTimeSpan TimeFrame.ThreeWeeks
              Expect.equal actual expected "Three weeks should be 21 days"

          testCase "TwoWeeks"
          <| fun _ ->
              let expected = TimeSpan.FromDays 14
              let actual = toTimeSpan TimeFrame.TwoWeeks
              Expect.equal actual expected "Two weeks should be 14 days"

          // Add more test cases for other TimeFrame cases

          testCase "OneMin"
          <| fun _ ->
              let expected = TimeSpan.FromMinutes 1
              let actual = toTimeSpan TimeFrame.OneMin
              Expect.equal actual expected "One minute should be 1 minute" ]

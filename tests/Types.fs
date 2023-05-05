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
              let result = toTimeSpan TimeFrame.Month
              let expected = TimeSpan.FromDays 30
              Expect.equal result expected "Month should be converted to 30 days"

          testCase "ThreeWeeks"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.ThreeWeeks
              let expected = TimeSpan.FromDays 21
              Expect.equal result expected "ThreeWeeks should be converted to 21 days"

          testCase "TwoWeeks"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.TwoWeeks
              let expected = TimeSpan.FromDays 14
              Expect.equal result expected "TwoWeeks should be converted to 14 days"

          testCase "Week"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.Week
              let expected = TimeSpan.FromDays 7
              Expect.equal result expected "Week should be converted to 7 days"

          testCase "ThirtyDays"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.ThirtyDays
              let expected = TimeSpan.FromDays 30
              Expect.equal result expected "ThirtyDays should be converted to 30 days"

          testCase "TwentyDays"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.TwentyDays
              let expected = TimeSpan.FromDays 20
              Expect.equal result expected "TwentyDays should be converted to 20 days"

          testCase "FifteenDays"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.FifteenDays
              let expected = TimeSpan.FromDays 15
              Expect.equal result expected "FifteenDays should be converted to 15 days"

          testCase "TenDays"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.TenDays
              let expected = TimeSpan.FromDays 10
              Expect.equal result expected "TenDays should be converted to 10 days"

          testCase "FiveDays"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.FiveDays
              let expected = TimeSpan.FromDays 5
              Expect.equal result expected "FiveDays should be converted to 5 days"

          testCase "ThreeDays"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.ThreeDays
              let expected = TimeSpan.FromDays 3
              Expect.equal result expected "ThreeDays should be converted to 3 days"

          testCase "TwoDays"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.TwoDays
              let expected = TimeSpan.FromDays 2
              Expect.equal result expected "TwoDays should be converted to 2 days"

          testCase "OneDay"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.OneDay
              let expected = TimeSpan.FromDays 1
              Expect.equal result expected "OneDay should be converted to 1 day"

          testCase "TwentyHours"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.TwentyHours
              let expected = TimeSpan.FromHours 20
              Expect.equal result expected "TwentyHours should be converted to 20 hours"

          testCase "EightTeenHours"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.EightTeenHours
              let expected = TimeSpan.FromHours 18
              Expect.equal result expected "EightTeenHours should be converted to 18 hours"

          testCase "SixteenHours"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.SixteenHours
              let expected = TimeSpan.FromHours 16
              Expect.equal result expected "SixteenHours should be converted to 16 hours"

          testCase "FourteenHours"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.FourteenHours
              let expected = TimeSpan.FromHours 14
              Expect.equal result expected "FourteenHours should be converted to 14 hours"

          testCase "TwelveHours"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.TwelveHours
              let expected = TimeSpan.FromHours 12
              Expect.equal result expected "TwelveHours should be converted to 12 hours"

          testCase "TenHours"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.TenHours
              let expected = TimeSpan.FromHours 10
              Expect.equal result expected "TenHours should be converted to 10 hours"

          testCase "EightHours"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.EightHours
              let expected = TimeSpan.FromHours 8
              Expect.equal result expected "EightHours should be converted to 8 hours"

          testCase "SixHours"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.SixHours
              let expected = TimeSpan.FromHours 6
              Expect.equal result expected "SixHours should be converted to 6 hours"

          testCase "FourHours"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.FourHours
              let expected = TimeSpan.FromHours 4
              Expect.equal result expected "FourHours should be converted to 4 hours"

          testCase "ThreeHours"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.ThreeHours
              let expected = TimeSpan.FromHours 3
              Expect.equal result expected "ThreeHours should be converted to 3 hours"

          testCase "TwoHours"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.TwoHours
              let expected = TimeSpan.FromHours 2
              Expect.equal result expected "TwoHours should be converted to 2 hours"

          testCase "OneHour"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.OneHour
              let expected = TimeSpan.FromHours 1
              Expect.equal result expected "OneHour should be converted to 1 hour"

          testCase "ThreeHundredNinetyMin"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.ThreeHundredNinetyMin
              let expected = TimeSpan.FromMinutes 390
              Expect.equal result expected "ThreeHundredNinetyMin should be converted to 390 minutes"

          testCase "TwoHundredSixtyMin"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.TwoHundredSixtyMin
              let expected = TimeSpan.FromMinutes 260
              Expect.equal result expected "TwoHundredSixtyMin should be converted to 260 minutes"

          testCase "OneHundredThirtyMin"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.OneHundredThirtyMin
              let expected = TimeSpan.FromMinutes 130
              Expect.equal result expected "OneHundredThirtyMin should be converted to 130 minutes"

          testCase "SixtyFiveMin"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.SixtyFiveMin
              let expected = TimeSpan.FromMinutes 65
              Expect.equal result expected "SixtyFiveMin should be converted to 65 minutes"

          testCase "FortyFiveMin"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.FortyFiveMin
              let expected = TimeSpan.FromMinutes 45
              Expect.equal result expected "FortyFiveMin should be converted to 45 minutes"

          testCase "ThirtyMin"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.ThirtyMin
              let expected = TimeSpan.FromMinutes 30
              Expect.equal result expected "ThirtyMin should be converted to 30 minutes"

          testCase "TwentyFourMin"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.TwentyFourMin
              let expected = TimeSpan.FromMinutes 24
              Expect.equal result expected "TwentyFourMin should be converted to 24 minutes"

          testCase "FifteenMin"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.FifteenMin
              let expected = TimeSpan.FromMinutes 15
              Expect.equal result expected "FifteenMin should be converted to 15 minutes"

          testCase "TwelveMin"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.TwelveMin
              let expected = TimeSpan.FromMinutes 12
              Expect.equal result expected "TwelveMin should be converted to 12 minutes"

          testCase "FiveMin"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.FiveMin
              let expected = TimeSpan.FromMinutes 5
              Expect.equal result expected "FiveMin should be converted to 5 minutes"

          testCase "ThreeMin"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.ThreeMin
              let expected = TimeSpan.FromMinutes 3
              Expect.equal result expected "ThreeMin should be converted to 3 minutes"

          testCase "OneMin"
          <| fun _ ->
              let result = toTimeSpan TimeFrame.OneMin
              let expected = TimeSpan.FromMinutes 1
              Expect.equal result expected "OneMin should be converted to 1 minute" ]

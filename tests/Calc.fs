module Incremental.Indicators.Tests.Calc

open Expecto
open FSharp.Data.Adaptive
open Incremental.Indicators.Calc
open Incremental.Indicators.Types
open System

let longishQuotes = TestData.getLongish 5285 |> Option.defaultValue AList.empty
let closePrices = longishQuotes |> AList.map (fun t -> double t.Close)

let x: double list = [ 1.0; 2.0; 3.0; 4.0; 5.0 ]

let y: double list = [ 0.0; 0.0; 0.0; 0.0 ]

[<Tests>]
let roundDownTests =
    testList
        "roundDown function tests"
        [ testCase "rounds down to the nearest minute"
          <| fun _ ->
              let date = DateTime(2023, 5, 1, 12, 34, 59)
              let interval = TimeSpan.FromMinutes(1.0)
              let expectedResult = DateTime(2023, 5, 1, 12, 34, 0)

              let result = roundDown date interval

              Expect.equal result expectedResult "The function should round down to the nearest minute"

          testCase "rounds down to the nearest hour"
          <| fun _ ->
              let date = DateTime(2023, 5, 1, 12, 59, 59)
              let interval = TimeSpan.FromHours(1.0)
              let expectedResult = DateTime(2023, 5, 1, 12, 0, 0)

              let result = roundDown date interval

              Expect.equal result expectedResult "The function should round down to the nearest hour"

          testCase "rounds down to the nearest day"
          <| fun _ ->
              let date = DateTime(2023, 5, 2, 23, 59, 59)
              let interval = TimeSpan.FromDays(1.0)
              let expectedResult = DateTime(2023, 5, 2, 0, 0, 0)

              let result = roundDown date interval

              Expect.equal result expectedResult "The function should round down to the nearest day"

          testCase "no rounding when interval is TimeSpan.Zero"
          <| fun _ ->
              let date = DateTime(2023, 5, 1, 12, 34, 59)
              let interval = TimeSpan.Zero
              let expectedResult = DateTime(2023, 5, 1, 12, 34, 59)

              let result = roundDown date interval

              Expect.equal result expectedResult "The function should not round down when interval is TimeSpan.Zero"

          testCase "should roundDown an interval correctly"
          <| fun _ ->
              let interval = toTimeSpan TimeFrame.OneHour
              let evDate = DateTime.Parse("2020-12-15 09:35:45")
              let rnDate = roundDown evDate interval
              let exDate = DateTime.Parse("2020-12-15 09:00:00")
              Expect.equal exDate rnDate "should return correct DateTime" ]


[<Tests>]
// A test function that tests the slope function
let slopeTests =
    testList
        "slope function tests"
        [ testCase "calculates the slope for two series of equal length"
          <| fun _ ->
              let xValues = [ 1.0; 2.0; 3.0; 4.0; 5.0 ] |> AList.ofList
              let yValues = [ 2.0; 4.0; 6.0; 8.0; 10.0 ] |> AList.ofList
              let result = slope xValues yValues
              let res = AVal.force result
              Expect.equal res (Ok 2.0) "The function should correctly calculate the slope"


          testCase "returns error if series have different lengths"
          <| fun _ ->
              let xValues = [ 1.0; 2.0; 3.0; 4.0 ] |> AList.ofList
              let yValues = [ 2.0; 4.0; 6.0; 8.0; 10.0 ] |> AList.ofList
              let result = slope xValues yValues
              let res = AVal.force result
              Expect.isError res "Slope x and y must be the same size" ]


[<Tests>]
// A test function that tests the stdDev function
let stdDevTests =
    testList
        "stdDev function tests"
        [ testCase "calculates the standard deviation for a series of values"
          <| fun _ ->
              let values = [ 1.0; 2.0; 3.0; 4.0; 5.0 ] |> AList.ofList
              let result = stdDev values |> AVal.force

              Expect.equal
                  (round (result * 1000.0))
                  (round (1.4142 * 1000.0))
                  "The function should correctly calculate the standard deviation"

          testCase "calculates the standard deviation for a series with negative values"
          <| fun _ ->
              let values = [ -1.0; 2.0; -3.0; 4.0; -5.0 ] |> AList.ofList
              let result = stdDev values |> AVal.force

              Expect.equal
                  (round result)
                  (round 3.261901)
                  "The function should correctly calculate the standard deviation for series with negative values"

          testCase "calculates the standard deviation for an empty series"
          <| fun _ ->
              let values = [] |> AList.ofList
              let result = stdDev values |> AVal.force

              Expect.equal result 0.0 "The function should return 0 for an empty series"

          testCase "calculates the standard deviation for large list"
          <| fun _ ->
              let result = stdDev closePrices |> AVal.force
              Expect.equal (Math.Round(result, 9)) 633.932098287 "The function should return" ]


[<Tests>]
// A test function that tests the meanDev function
let meanDevTests =
    testList
        "meanDev function tests"
        [ testCase "calculates the mean deviation for a series of values"
          <| fun _ ->
              let values = [ 1.0; 2.0; 3.0; 4.0; 5.0 ] |> List.map double |> AList.ofList
              let result = meanDev values |> AVal.force

              Expect.equal result 1.2 "The function should correctly calculate the mean deviation"

          testCase "calculates the mean deviation for a series with negative values"
          <| fun _ ->
              let values = [ -1.0; 2.0; -3.0; 4.0; -5.0 ] |> List.map double |> AList.ofList
              let result = meanDev values |> AVal.force

              Expect.equal
                  result
                  2.88
                  "The function should correctly calculate the mean deviation for series with negative values"

          testCase "calculates the mean deviation for an empty series"
          <| fun _ ->
              let values = [] |> AList.ofList
              let result = meanDev values |> AVal.force

              Expect.equal result 0.0 "The function should return 0 for an empty series" ]


[<Tests>]
let getDecimalPlacesTests =
    testList
        "getDecimalPlaces"
        [ test "GetDecimalPlaces should return 0 for whole numbers" {
              let actual = getDecimalPlaces 123.0m
              let expected = 0
              Expect.equal actual expected "should return 0 for whole numbers"
          }

          test "GetDecimalPlaces should return the correct number of decimal places" {
              let actual = getDecimalPlaces 123.456m
              let expected = 3
              Expect.equal actual expected "should be 3"
          }

          test "GetDecimalPlaces should handle negative numbers" {
              let actual = getDecimalPlaces -123.456m
              let expected = 3
              Expect.equal actual expected "should be 3"
          } ]

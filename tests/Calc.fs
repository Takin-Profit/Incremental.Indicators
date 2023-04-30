module Incremental.Indicators.Tests.Calc

open Expecto
open FSharp.Data.Adaptive
open Incremental.Indicators.Calc



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

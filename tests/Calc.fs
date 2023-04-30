module Incremental.Indicators.Tests.Calc

open Incremental.Indicators.Calc
open Expecto
open System



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

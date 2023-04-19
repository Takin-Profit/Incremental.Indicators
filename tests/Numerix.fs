module Incremental.Indicators.Tests.Numerix

open Incremental.Indicators.Numerix
open Expecto
open System

let isNaN (num: double) = Double.IsNaN num

let isResultNaN (res: Result<double, string>) =
    match res with
    | Ok(num) when isNaN num -> true
    | _ -> false

let getRes res =
    match res with
    | Ok(num) -> num
    | Error _ -> 0.0

[<Tests>]
let meanTests =
    testList
        "Numerix.mean Tests"
        [ test "Empty list returns NaN" {
              let result = mean [||]
              let expected = isNaN result
              Expect.isTrue expected "should be NaN"
          }
          test "Null list returns NaN" {
              let result = mean null
              let expected = isNaN result
              Expect.isTrue expected "should be NaN"
          }
          test "Calculates mean of values" {
              let values = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
              let expected = 3.0
              let result = mean values
              Expect.equal result expected ""
          } ] //

[<Tests>]
let stdDevTests = // tolerance ]
    testList
        "Numerix.stdDev Tests"
        [ testCase "Empty Array"
          <| fun _ ->
              let values = [||]
              let actual = stdDev values
              Expect.isError actual "should be an error"

          testCase "Null Values"
          <| fun _ ->
              let values = null
              let actual = stdDev values
              Expect.isError actual "should be an error"

          testCase "Contains NaN"
          <| fun _ ->
              let values = [| 1.0; Double.NaN; 3.0; 4.0 |]
              let actual = stdDev values
              let expected = isResultNaN actual
              Expect.isTrue expected "should be NaN"

          testCase "Contains Infinity"
          <| fun _ ->
              let values = [| 1.0; Double.PositiveInfinity; 3.0; 4.0 |]
              let actual = stdDev values
              let expected = isResultNaN actual
              Expect.isTrue expected "should be NaN"

          testCase "Calculate stdDev for valid input"
          <| fun _ ->
              let values = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
              let expected = 1.41421356237309
              let actual = stdDev values
              Expect.isOk actual "should be Ok"
              let actual = stdDev values |> getRes
              Expect.floatClose Accuracy.high actual expected "should be within accuracy" ]

[<Tests>]
let slopeTests =
    testList
        "Slope function"
        [
          // Test with valid inputs
          testCase "Valid inputs should return proper result"
          <| fun _ ->
              let x = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
              let y = [| 2.0; 3.0; 4.0; 5.0; 6.0 |]
              let expected = Ok(1.0)
              let actual = slope x y
              Expect.isOk actual "should be Ok"
              Expect.equal (getRes actual) (getRes expected) "should be equal"

          // Test with empty arrays
          testCase "Empty arrays"
          <| fun _ ->
              let x = [||]
              let y = [||]
              let actual = slope x y
              Expect.isError actual "should return an error"

          // Test with null inputs
          testCase "Null inputs"
          <| fun _ ->
              let x = null
              let y = null
              let actual = slope x y
              Expect.isError actual "should return an error"

          // Test with different length arrays
          testCase "Different length arrays"
          <| fun _ ->
              let x = [| 1.0; 2.0; 3.0 |]
              let y = [| 2.0; 3.0; 4.0; 5.0 |]
              let actual = slope x y
              Expect.isError actual "should return an error" ]

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

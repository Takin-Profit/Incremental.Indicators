module Incremental.Indicators.Tests.Util

open System
open Expecto
open FSharp.Data.Adaptive
open Incremental.Indicators.Util


[<Tests>]
let testGetVal =

    let ls = AList.ofArray [| 1; 2; 3; 4; 5 |]

    testList
        "Util.getVal tests"
        [ test "getVal with valid offset should return correct value" {
              let result = getVal 2 0 ls
              Expect.equal result (aval { return 3 }) "should get the 2nd element"
          }

          test "getVal with negative offset should return default value" {
              let result = getVal -2 0 ls
              Expect.equal result (aval { return 0 }) "should return default value"
          }

          test "getVal with too large offset should return default value" {
              let result = getVal 10 0 ls
              Expect.equal result (aval { return 0 }) "should return default value"
          } ]
// A record type for testing purposes
type TestSeries = { Date: DateTime; Value: int }

// A test function that tests the find function using the TestSeries record
[<Tests>]
let findTests =
    testList
        "find function tests"
        [ testCase "finds an element in the series"
          <| fun _ ->
              let series =
                  AList.ofList
                      [ { Date = DateTime(2023, 1, 1)
                          Value = 1 }
                        { Date = DateTime(2023, 1, 2)
                          Value = 2 }
                        { Date = DateTime(2023, 1, 3)
                          Value = 3 } ]

              let lookupDate = DateTime(2023, 1, 2)
              let defaultVal = { Date = DateTime.MinValue; Value = 0 }

              let result = find series lookupDate defaultVal

              Expect.equal
                  result
                  (aval {
                      return
                          { Date = DateTime(2023, 1, 2)
                            Value = 2 }
                  })
                  "The function should find the element in the series"

          testCase "returns default value if element is not found"
          <| fun _ ->
              let series =
                  AList.ofList
                      [ { Date = DateTime(2023, 1, 1)
                          Value = 1 }
                        { Date = DateTime(2023, 1, 2)
                          Value = 2 }
                        { Date = DateTime(2023, 1, 3)
                          Value = 3 } ]

              let lookupDate = DateTime(2023, 1, 5)
              let defaultVal = { Date = DateTime.MinValue; Value = 0 }

              let result = find series lookupDate defaultVal

              Expect.equal
                  result
                  (aval { return { Date = DateTime.MinValue; Value = 0 } })
                  "The function should return the default value" ]

open Expecto

// A record type for testing purposes
type TestRecord = { Id: int; Value: string }

[<Tests>]
// A test function that tests the removeWarmupPeriods function using the TestRecord record
let removeWarmupPeriodsTests =
    testList
        "removeWarmupPeriods function tests"
        [ testCase "removes specified number of periods"
          <| fun _ ->
              let series =
                  AList.ofList [ { Id = 1; Value = "A" }; { Id = 2; Value = "B" }; { Id = 3; Value = "C" } ]

              let removePeriods = 2

              let result = removeWarmupPeriods series removePeriods

              match result with
              | Ok r ->
                  Expect.equal (AList.count r) (aval { return 1 }) "The function should remove the specified periods"
              | Error e -> failwithf $"Unexpected error: %s{e}"

          testCase "returns error if removePeriods is negative"
          <| fun _ ->
              let series =
                  AList.ofList [ { Id = 1; Value = "A" }; { Id = 2; Value = "B" }; { Id = 3; Value = "C" } ]

              let removePeriods = -1

              let result = removeWarmupPeriods series removePeriods

              match result with
              | Ok _ -> failwith "Expected error, but the function returned a valid result"
              | Error e ->
                  Expect.equal
                      e
                      "the Remove Periods value must be greater than or equal to 0."
                      "The function should return the expected error" ]

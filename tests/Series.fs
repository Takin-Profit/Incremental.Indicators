module Incremental.Indicators.Tests.Series

open System
open Expecto
open Incremental.Indicators.Series

let getList res =
    match res with
    | Ok l -> l
    | _ -> []

let tests =
    testList
        "Series tests"
        [

          test "Find returns correct element" {
              let series =
                  [| { new ISeries with
                         member this.Date = DateTime(2022, 01, 01) }
                     { new ISeries with
                         member this.Date = DateTime(2022, 01, 02) }
                     { new ISeries with
                         member this.Date = DateTime(2022, 01, 03) } |]

              let result = find series (DateTime(2022, 01, 02))

              Expect.equal
                  (Some
                      { new ISeries with
                          member this.Date = DateTime(2022, 01, 02) })
                  result
                  "Should return correct element"
          }

          test "Find returns None if element not found" {
              let series =
                  [| { new ISeries with
                         member this.Date = DateTime(2022, 01, 01) }
                     { new ISeries with
                         member this.Date = DateTime(2022, 01, 02) }
                     { new ISeries with
                         member this.Date = DateTime(2022, 01, 03) } |]

              let result = find series (DateTime(2022, 01, 04))

              Expect.equal None result "should return none"
          }

          test "Remove removes correct number of elements" {
              let series = [ 1; 2; 3; 4; 5 ]
              let result = remove series 2
              Expect.equal [ 3; 4; 5 ] result "should remove correct element"
          }

          test "Remove returns empty list if removing all elements" {
              let series = [ 1; 2; 3; 4; 5 ]
              let result = remove series 5
              Expect.equal [] result "should be empty list"
          }

          test "removeWarmupPeriods removes correct number of elements" {
              let series = [ 1; 2; 3; 4; 5 ]
              let result = removeWarmupPeriods series 2
              let list = getList result
              Expect.equal [ 3; 4; 5 ] list "lists should match"
          }

          test "removeWarmupPeriods returns error if removePeriods < 0" {
              let series = [ 1; 2; 3; 4; 5 ]
              let result = removeWarmupPeriods series -1
              Expect.isError result "should return an Error"
          } ]

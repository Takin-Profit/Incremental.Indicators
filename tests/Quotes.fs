module Incremental.Indicators.Tests.Quotes

open Expecto
open Incremental.Indicators.Quotes
open Incremental.Indicators
open System
open FSharp.Data.Adaptive


[<Tests>]
let validateTests =
    testList
        "validate tests"
        [ testCase "No duplicates"
          <| fun _ ->
              let quotes =
                  [ { Quote.Empty with
                        Date = DateTime(2023, 5, 1) }
                    { Quote.Empty with
                        Date = DateTime(2023, 5, 2) } ]

              match validate quotes with
              | Ok _ -> ()
              | Error errMsg -> failwithf $"Unexpected error: %s{errMsg}"

          testCase "Duplicates found"
          <| fun _ ->
              let quotes =
                  [ { Quote.Empty with
                        Date = DateTime(2023, 5, 1) }
                    { Quote.Empty with
                        Date = DateTime(2023, 5, 1) } ]

              match validate quotes with
              | Ok _ -> failwith "Expected error due to duplicates, but got success"
              | Error errMsg ->
                  Expect.stringContains errMsg "Duplicate date found" "Expected error message about duplicate dates"

          testCase "Empty quotes list"
          <| fun _ ->
              let quotes = []

              match validate quotes with
              | Ok _ -> ()
              | Error errMsg -> failwithf $"Unexpected error: %s{errMsg}" ]


[<Tests>]
let isValidTests =
    testList
        "isValid tests"
        [ testCase "Valid quote"
          <| fun _ ->
              let quote1 =
                  { Quote.Empty with
                      Date = DateTime(2023, 5, 1) }

              let quote2 =
                  { Quote.Empty with
                      Date = DateTime(2023, 5, 2) }

              let quotes = alist { quote1 }
              let actual = isValid quote2 quotes
              Expect.isTrue actual "Quote should be valid"

          testCase "Invalid quote"
          <| fun _ ->
              let quote =
                  { Quote.Empty with
                      Date = DateTime(2023, 5, 1) }

              let quotes = alist { quote }
              let actual = isValid quote quotes
              Expect.isFalse actual "Quote should be invalid"

          testCase "Empty quotes list"
          <| fun _ ->
              let quote =
                  { Quote.Empty with
                      Date = DateTime(2023, 5, 1) }

              let quotes = AList.empty
              let actual = isValid quote quotes
              Expect.isTrue actual "Quote should be valid when the quotes list is empty" ]

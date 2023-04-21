module Incremental.Indicators.Tests.Quotes

open Expecto
open Incremental.Indicators.Quotes
open Incremental.Indicators.Types
open System

[<Tests>]
let quotesTests =

    let testQuotes =
        [ { Quote.Date = DateTime.Parse("2023-04-15")
            Open = 2.3m
            High = 2.5m
            Low = 2.2m
            Close = 2.4m
            Volume = 1000m }
          { Date = DateTime.Parse("2023-04-16")
            Open = 2.5m
            High = 2.6m
            Low = 2.3m
            Close = 2.4m
            Volume = 1500m }
          { Date = DateTime.Parse("2023-04-17")
            Open = 2.4m
            High = 2.6m
            Low = 2.3m
            Close = 2.5m
            Volume = 2000m }
          { Date = DateTime.Parse("2023-04-18")
            Open = 2.5m
            High = 2.7m
            Low = 2.4m
            Close = 2.6m
            Volume = 3000m } ]

    let expectedTuples =
        [ (DateTime.Parse("2023-04-15"), 2.3)
          (DateTime.Parse("2023-04-16"), 2.5)
          (DateTime.Parse("2023-04-17"), 2.4)
          (DateTime.Parse("2023-04-18"), 2.5) ]

    let expectedSortedList =
        [ (DateTime.Parse("2023-04-15"), 2.3)
          (DateTime.Parse("2023-04-16"), 2.5)
          (DateTime.Parse("2023-04-17"), 2.4)
          (DateTime.Parse("2023-04-18"), 2.5) ]

    let actualTuples = toTupleSeq CandlePart.Open testQuotes
    let actualSortedList = quotesToSortedList CandlePart.Open testQuotes

    testList
        "Quote Functions"
        [ test "toTupleSeq test" { Expect.sequenceEqual actualTuples expectedTuples "Tuples should match expected." }
          test "quotesToSortedList test" {
              Expect.sequenceEqual actualSortedList expectedSortedList "Sorted list should match expected."
          }

          ]

[<Tests>]
let testList2 =
    testList
        "Conversion Functions"
        [

          test "toSortedList should return a list sorted by date" {
              let unsortedList =
                  seq {
                      yield DateTime(2022, 1, 1), 10.0
                      yield DateTime(2022, 1, 3), 20.0
                      yield DateTime(2022, 1, 2), 15.0
                  }

              let expectedList =
                  [ (DateTime(2022, 1, 1), 10.0)
                    (DateTime(2022, 1, 2), 15.0)
                    (DateTime(2022, 1, 3), 20.0) ]

              let actualList = toSortedList CandlePart.Open unsortedList
              Expect.sequenceEqual actualList expectedList "lists should be equal and sorted"
          }


          test "tuplesToSortedList should return a list sorted by date" {
              let unsortedList =
                  seq {
                      yield DateTime(2022, 1, 1), 10.0
                      yield DateTime(2022, 1, 3), 20.0
                      yield DateTime(2022, 1, 2), 15.0
                  }

              let expectedList =
                  [ (DateTime(2022, 1, 1), 10.0)
                    (DateTime(2022, 1, 2), 15.0)
                    (DateTime(2022, 1, 3), 20.0) ]

              let actualList = tuplesToSortedList unsortedList
              Expect.sequenceEqual actualList expectedList "lists should be sorted"
          }

          ]

[<Tests>]
let testsList3 =
    testList
        "toTupleArray"
        [ testCase "empty input"
          <| fun _ ->
              let expected = [||]
              let actual = toTupleArray CandlePart.Open Seq.empty
              Expect.sequenceEqual expected actual "should be empty"

          testCase "single item input"
          <| fun _ ->
              let expected = [| DateTime(2023, 1, 1), 1.0 |]

              let quotes =
                  seq {
                      yield
                          { Quote.Date = DateTime(2023, 1, 1)
                            Open = 1.0m
                            High = 2.0m
                            Low = 0.5m
                            Close = 1.5m
                            Volume = 100.0m }
                  }

              let actual = toTupleArray CandlePart.Open quotes
              Expect.sequenceEqual expected actual "should match"

          testCase "multiple item input"
          <| fun _ ->
              let expected =
                  [| DateTime(2023, 1, 1), 1.0
                     DateTime(2023, 1, 2), 2.0
                     DateTime(2023, 1, 3), 3.0 |]

              let quotes =
                  seq {
                      yield
                          { Quote.Date = DateTime(2023, 1, 1)
                            Open = 1.0m
                            High = 2.0m
                            Low = 0.5m
                            Close = 1.5m
                            Volume = 100.0m }

                      yield
                          { Date = DateTime(2023, 1, 2)
                            Open = 2.0m
                            High = 3.0m
                            Low = 1.5m
                            Close = 2.5m
                            Volume = 200.0m }

                      yield
                          { Date = DateTime(2023, 1, 3)
                            Open = 3.0m
                            High = 4.0m
                            Low = 2.5m
                            Close = 3.5m
                            Volume = 300.0m }
                  }

              let actual = toTupleArray CandlePart.Open quotes
              Expect.sequenceEqual expected actual "sequences should match." ]

[<Tests>]
let testsList4 =
    testList
        "tuplesToSortedList tests"
        [ testCase "empty input"
          <| fun () ->
              let actual = tuplesToSortedList Seq.empty
              let expected = []
              Expect.sequenceEqual actual expected "empty input failed"

          testCase "single tuple input"
          <| fun () ->
              let actual = tuplesToSortedList (seq { yield DateTime(2023, 4, 21), 3.14 })
              let expected = [ (DateTime(2023, 4, 21), 3.14) ]
              Expect.sequenceEqual actual expected "single tuple input failed"

          testCase "multiple tuples input"
          <| fun () ->
              let actual =
                  tuplesToSortedList (
                      seq {
                          yield DateTime(2023, 4, 21), 2.72
                          yield DateTime(2023, 4, 20), 3.14
                          yield DateTime(2023, 4, 22), 1.41
                      }
                  )

              let expected =
                  [ (DateTime(2023, 4, 20), 3.14)
                    (DateTime(2023, 4, 21), 2.72)
                    (DateTime(2023, 4, 22), 1.41) ]

              Expect.sequenceEqual actual expected "multiple tuples input failed" ]

[<Tests>]
let tests5 =
    testList
        "toSortedTupleArray tests"
        [ testCase "empty input"
          <| fun () ->
              let actual = toSortedTupleArray Seq.empty
              let expected = [||]
              Expect.sequenceEqual actual expected "empty input failed"

          testCase "single tuple input"
          <| fun () ->
              let actual = toSortedTupleArray (seq { yield DateTime(2023, 4, 21), 3.14 })
              let expected = [| (DateTime(2023, 4, 21), 3.14) |]
              Expect.sequenceEqual actual expected "single tuple input failed"

          testCase "multiple tuples input"
          <| fun () ->
              let actual =
                  toSortedTupleArray (
                      seq {
                          yield DateTime(2023, 4, 21), 2.72
                          yield DateTime(2023, 4, 20), 3.14
                          yield DateTime(2023, 4, 22), 1.41
                      }
                  )

              let expected =
                  [| (DateTime(2023, 4, 20), 3.14)
                     (DateTime(2023, 4, 21), 2.72)
                     (DateTime(2023, 4, 22), 1.41) |]

              Expect.sequenceEqual actual expected "multiple tuples input failed" ]

let toQuoteDListTests =
    testList
        "toQuoteDList tests"
        [ testCase "Empty list"
          <| fun _ ->
              let input = []
              let expectedOutput = []
              let actualOutput = toQuoteDList input
              Expect.sequenceEqual actualOutput expectedOutput "Should be empty list"
          testCase "List with one element"
          <| fun _ ->
              let input =
                  [ { Quote.Date = DateTime(2022, 01, 01)
                      Open = 100M
                      High = 110M
                      Low = 90M
                      Close = 105M
                      Volume = 1000M } ]

              let expectedOutput =
                  [ { Date = DateTime(2022, 01, 01)
                      Open = 100.0
                      High = 110.0
                      Low = 90.0
                      Close = 105.0
                      Volume = 1000.0 } ]

              let actualOutput = toQuoteDList input
              Expect.sequenceEqual actualOutput expectedOutput "should be correct doubles"
          testCase "List with multiple elements"
          <| fun _ ->
              let input =
                  [ { Quote.Date = DateTime(2022, 01, 01)
                      Open = 100M
                      High = 110M
                      Low = 90M
                      Close = 105M
                      Volume = 1000M }
                    { Date = DateTime(2022, 01, 02)
                      Open = 105M
                      High = 120M
                      Low = 95M
                      Close = 115M
                      Volume = 2000M } ]

              let expectedOutput =
                  [ { Date = DateTime(2022, 01, 01)
                      Open = 100.0
                      High = 110.0
                      Low = 90.0
                      Close = 105.0
                      Volume = 1000.0 }
                    { Date = DateTime(2022, 01, 02)
                      Open = 105.0
                      High = 120.0
                      Low = 95.0
                      Close = 115.0
                      Volume = 2000.0 } ]

              let actualOutput = toQuoteDList input
              Expect.sequenceEqual actualOutput expectedOutput "should have correct values" ]

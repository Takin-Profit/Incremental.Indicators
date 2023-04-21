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
              Expect.sequenceEqual actual expected "should be empty"

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
              Expect.sequenceEqual actual expected "should match"

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
              Expect.sequenceEqual actual expected "sequences should match." ]

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

[<Tests>]
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

[<Tests>]
let quoteDtoTupleTests =
    testList
        "quoteDtoTuple tests"
        [

          test "Open quoteDtoTuple test" {
              let expected = (DateTime(2022, 1, 1), 10.0)

              let actual =
                  quoteDtoTuple
                      { Date = DateTime(2022, 1, 1)
                        Open = 10.0
                        High = 12.0
                        Low = 9.0
                        Close = 11.0
                        Volume = 100.0 }
                      CandlePart.Open

              Expect.equal actual expected "Open quoteDtoTuple returns expected value"
          }

          test "High quoteDtoTuple test" {
              let expected = (DateTime(2022, 1, 1), 12.0)

              let actual =
                  quoteDtoTuple
                      { Date = DateTime(2022, 1, 1)
                        Open = 10.0
                        High = 12.0
                        Low = 9.0
                        Close = 11.0
                        Volume = 100.0 }
                      CandlePart.High

              Expect.equal actual expected "High quoteDtoTuple returns expected value"
          }

          test "Low quoteDtoTuple test" {
              let expected = (DateTime(2022, 1, 1), 9.0)

              let actual =
                  quoteDtoTuple
                      { Date = DateTime(2022, 1, 1)
                        Open = 10.0
                        High = 12.0
                        Low = 9.0
                        Close = 11.0
                        Volume = 100.0 }
                      CandlePart.Low

              Expect.equal actual expected "Low quoteDtoTuple returns expected value"
          }

          test "Close quoteDtoTuple test" {
              let expected = (DateTime(2022, 1, 1), 11.0)

              let actual =
                  quoteDtoTuple
                      { Date = DateTime(2022, 1, 1)
                        Open = 10.0
                        High = 12.0
                        Low = 9.0
                        Close = 11.0
                        Volume = 100.0 }
                      CandlePart.Close

              Expect.equal actual expected "Close quoteDtoTuple returns expected value"
          }

          test "Volume quoteDtoTuple test" {
              let expected = (DateTime(2022, 1, 1), 100.0)

              let actual =
                  quoteDtoTuple
                      { Date = DateTime(2022, 1, 1)
                        Open = 10.0
                        High = 12.0
                        Low = 9.0
                        Close = 11.0
                        Volume = 100.0 }
                      CandlePart.Volume

              Expect.equal actual expected "Volume quoteDtoTuple returns expected value"
          }

          test "HL2 quoteDtoTuple test" {
              let expected = (DateTime(2022, 1, 1), 10.5)

              let actual =
                  quoteDtoTuple
                      { Date = DateTime(2022, 1, 1)
                        Open = 10.0
                        High = 12.0
                        Low = 9.0
                        Close = 11.0
                        Volume = 100.0 }
                      CandlePart.HL2

              Expect.equal actual expected "HL2 quoteDtoTuple returns expected value"
          }

          test "HLC3 quoteDtoTuple test" {
              let expected = (DateTime(2022, 1, 1), 10.666666666666666)

              let actual =
                  quoteDtoTuple
                      { Date = DateTime(2022, 1, 1)
                        Open = 10.0
                        High = 12.0
                        Low = 9.0
                        Close = 11.0
                        Volume = 100.0 }
                      CandlePart.HLC3

              Expect.equal actual expected "HLC3 quoteDtoTuple"
          }

          test "OC2" {
              let q =
                  { Date = DateTime.Now
                    Open = 10.0
                    High = 20.0
                    Low = 5.0
                    Close = 15.0
                    Volume = 100.0 }

              let result =
                  quoteDtoTuple
                      { Date = q.Date
                        Open = float q.Open
                        High = float q.High
                        Low = float q.Low
                        Close = float q.Close
                        Volume = float q.Volume }
                      CandlePart.OC2

              Expect.equal result (q.Date, 12.5) "OC2 quoteDtoTuple"
          }
          test "OHL3" {
              let q =
                  { Date = DateTime.Now
                    Open = 10.0
                    High = 20.0
                    Low = 5.0
                    Close = 15.0
                    Volume = 100.0 }

              let result =
                  quoteDtoTuple
                      { Date = q.Date
                        Open = float q.Open
                        High = float q.High
                        Low = float q.Low
                        Close = float q.Close
                        Volume = float q.Volume }
                      CandlePart.OHL3

              Expect.equal result (q.Date, 11.666666666666666) "OHL3 results"
          }
          test "OHLC4" {
              let q =
                  { Date = DateTime.Now
                    Open = 10.0
                    High = 20.0
                    Low = 5.0
                    Close = 15.0
                    Volume = 100.0 }

              let result =
                  quoteDtoTuple
                      { Date = q.Date
                        Open = float q.Open
                        High = float q.High
                        Low = float q.Low
                        Close = float q.Close
                        Volume = float q.Volume }
                      CandlePart.OHLC4

              Expect.equal result (q.Date, 12.5) "OLC4 results"
          } ]

[<Tests>]
let quoteDListToToTuplesTests =
    testList
        "quoteDListToToTuples"
        [ testCase "Empty List"
          <| fun _ ->
              let emptyList = []
              let result = quoteDListToToTuples CandlePart.Close emptyList
              Expect.sequenceEqual result [] "list is empty"

          testCase "Single QuoteD"
          <| fun _ ->
              let singleQuoteD =
                  { Date = DateTime.Now
                    Open = 10.0
                    High = 20.0
                    Low = 5.0
                    Close = 15.0
                    Volume = 100.0 }

              let qdList = [ singleQuoteD ]
              let expected = [ (singleQuoteD.Date, singleQuoteD.Close) ]
              let result = quoteDListToToTuples CandlePart.Close qdList
              Expect.sequenceEqual result expected "list matches"

          testCase "Multiple QuoteD"
          <| fun _ ->
              let qdList =
                  [ { Date = DateTime.Now.AddDays(-3.0)
                      Open = 10.0
                      High = 20.0
                      Low = 5.0
                      Close = 15.0
                      Volume = 100.0 }
                    { Date = DateTime.Now.AddDays(-2.0)
                      Open = 15.0
                      High = 25.0
                      Low = 10.0
                      Close = 20.0
                      Volume = 200.0 }
                    { Date = DateTime.Now.AddDays(-1.0)
                      Open = 20.0
                      High = 30.0
                      Low = 15.0
                      Close = 25.0
                      Volume = 300.0 } ]

              let expected =
                  [ (qdList[0].Date, qdList[0].Close)
                    (qdList[1].Date, qdList[1].Close)
                    (qdList[2].Date, qdList[2].Close) ]

              let result = quoteDListToToTuples CandlePart.Close qdList
              Expect.sequenceEqual result expected "results are as expected"

          testCase "OC2 returns expected results"
          <| fun _ ->
              let qdList =
                  [ { Date = DateTime(2022, 1, 1)
                      Open = 10.0
                      High = 15.0
                      Low = 5.0
                      Close = 12.0
                      Volume = 1000.0 }
                    { Date = DateTime(2022, 1, 2)
                      Open = 12.0
                      High = 20.0
                      Low = 8.0
                      Close = 18.0
                      Volume = 1500.0 }
                    { Date = DateTime(2022, 1, 3)
                      Open = 18.0
                      High = 25.0
                      Low = 15.0
                      Close = 20.0
                      Volume = 2000.0 } ]

              let expected =
                  [ (DateTime(2022, 1, 1), 11.0)
                    (DateTime(2022, 1, 2), 15.0)
                    (DateTime(2022, 1, 3), 19.0) ]

              Expect.sequenceEqual (quoteDListToToTuples CandlePart.OC2 qdList) expected "returns correct results" ]

[<Tests>]
let toBasicDataTests =

    // define some test data
    let testQuote1 =
        { Quote.Date = DateTime(2021, 4, 21)
          Open = 100.0M
          High = 150.0M
          Low = 75.0M
          Close = 125.0M
          Volume = 10000.0M }

    let testQuote2 =
        { Quote.Date = DateTime(2021, 4, 22)
          Open = 200.0M
          High = 250.0M
          Low = 175.0M
          Close = 225.0M
          Volume = 20000.0M }

    let testQuote3 =
        { Quote.Date = DateTime(2021, 4, 23)
          Open = 300.0M
          High = 350.0M
          Low = 275.0M
          Close = 325.0M
          Volume = 30000.0M }

    testList
        "toBasicData tests"
        [
          // test converting Open
          testCase "Open"
          <| fun () ->
              let expected =
                  { Date = testQuote1.Date
                    Value = 100.0 }

              let actual = toBasicData CandlePart.Open testQuote1
              Expect.equal actual expected "should convert properly"

          // test converting High
          testCase "High"
          <| fun () ->
              let expected =
                  { Date = testQuote2.Date
                    Value = 250.0 }

              let actual = toBasicData CandlePart.High testQuote2
              Expect.equal actual expected "should convert properly"

          // test converting Low
          testCase "Low"
          <| fun () ->
              let expected =
                  { Date = testQuote3.Date
                    Value = 275.0 }

              let actual = toBasicData CandlePart.Low testQuote3
              Expect.equal actual expected "should convert properly"

          // test converting Close
          testCase "Close"
          <| fun () ->
              let expected =
                  { Date = testQuote1.Date
                    Value = 125.0 }

              let actual = toBasicData CandlePart.Close testQuote1
              Expect.equal actual expected "should convert properly"

          // test converting Volume
          testCase "Volume"
          <| fun () ->
              let expected =
                  { Date = testQuote2.Date
                    Value = 20000.0 }

              let actual = toBasicData CandlePart.Volume testQuote2
              Expect.equal actual expected "should convert properly"

          // test converting HL2
          testCase "HL2"
          <| fun () ->
              let expected =
                  { Date = testQuote3.Date
                    Value = 312.5 }

              let actual = toBasicData CandlePart.HL2 testQuote3
              Expect.equal actual expected "should convert properly"

          // test converting HLC3
          testCase "HLC3"
          <| fun () ->
              let expected =
                  { Date = testQuote1.Date
                    Value = 116.66666666666667 }

              let actual = toBasicData CandlePart.HLC3 testQuote1
              Expect.equal actual expected "should convert properly"

          // test converting OHL3
          testCase "OHL3"
          <| fun () ->
              let expected =
                  { Date = testQuote3.Date
                    Value = 308.3333333 }

              let actual = toBasicData CandlePart.OHL3 testQuote3
              Expect.floatClose Accuracy.medium actual.Value expected.Value "should convert properly"

          // test converting OC2
          testCase "OC2"
          <| fun () ->
              let expected =
                  { Date = testQuote2.Date
                    Value = 212.5 }

              let actual = toBasicData CandlePart.OC2 testQuote2
              Expect.equal actual expected "should convert properly" ]

[<Tests>]
let aggregateTests =
    testList
        "aggregateByTimeSpan tests"
        [

          testCase "When quotes is empty, return empty"
          <| fun () ->
              let input = Seq.empty
              let timeSpan = TimeSpan.FromMinutes 5.0
              let actual = aggregateByTimeSpan timeSpan input
              let expected = Ok(Seq.empty)
              Expect.equal actual expected "array should be empty"

          testCase "When timespan is negative, return error"
          <| fun () ->
              let input =
                  Seq.singleton
                      { Quote.Date = DateTime.Now
                        Open = 1M
                        High = 2M
                        Low = 0.5M
                        Close = 1.5M
                        Volume = 1000M }

              let timeSpan = TimeSpan.FromMinutes -5.0
              let actual = aggregateByTimeSpan timeSpan input
              Expect.isError actual "should return an error"

          testCase "When timespan is positive, return aggregated quotes"
          <| fun () ->
              let input =
                  Seq.init 4 (fun i ->
                      { Quote.Date = DateTime(2023, 4, 1, 12, 15, 0).AddDays(float i)
                        Open = decimal (i + 1)
                        High = decimal (i + 2)
                        Low = decimal i
                        Close = decimal (i + 1)
                        Volume = 1000M })

              let timeSpan = TimeSpan.FromDays 1.0
              let actual = aggregateByTimeSpan timeSpan input
              Expect.isOk actual "should return aggregated quotes" ]

[<Tests>]
let aggregateByTimeFrameTests =
    let testData =
        [ { Quote.Date = DateTime(2022, 1, 1)
            Open = 1m
            High = 2m
            Low = 0.5m
            Close = 1.5m
            Volume = 10m }
          { Date = DateTime(2022, 1, 2)
            Open = 1.5m
            High = 3m
            Low = 1m
            Close = 2m
            Volume = 15m }
          { Date = DateTime(2022, 1, 3)
            Open = 2m
            High = 3.5m
            Low = 1.5m
            Close = 3m
            Volume = 20m }
          { Date = DateTime(2022, 1, 4)
            Open = 3m
            High = 4.5m
            Low = 2.5m
            Close = 4m
            Volume = 25m } ]

    testList
        "aggregateByTimeFrame"
        [

          test "Aggregating by 12 hours should result in four items" {
              let result = aggregateByTimeSpan (TimeSpan.FromHours 12) testData
              Expect.equal (result |> Result.map Seq.length) (Ok 4) "should result in four items"
          }

          test "Aggregating by zero time should result in an error" {
              let result = aggregateByTimeSpan TimeSpan.Zero testData

              Expect.equal
                  result
                  (Error
                      "Quotes Aggregation must use a usable new size value (see documentation for options). Value: 00:00:00")
                  "should be an error"
          } ]

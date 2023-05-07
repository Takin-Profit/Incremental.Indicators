module Incremental.Indicators.Tests.Data


open Expecto
open FSharp.Data.Adaptive
open Incremental.Indicators


[<Tests>]
let dataTests =
    let quotes =
        TestData.getDefault 502
        |> Option.defaultValue AList.empty
        |> Quotes.toQuoteDList
        |> AList.force

    testList
        "TestData Tests"
        [ testCase "should load quotes correctly"
          <| fun _ ->
              let actual = quotes.Count
              Expect.equal actual 502 "should load the correct number of quotes"

          ]

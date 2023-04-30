module Incremental.Indicators.Tests.Util

open Expecto
open FSharp.Data.Adaptive
open Incremental.Indicators.Util


[<Tests>]
let testGetVal =

    let ls = AList.ofArray [| 1; 2; 3; 4; 5 |]

    testList
        "AList Tests"
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

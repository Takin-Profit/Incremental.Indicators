module Incremental.Indicators.Tests.Numerix

open Incremental.Indicators.Numerix
open Expecto
open System

let config =
    { FsCheckConfig.defaultConfig with
        maxTest = 10000 }

[<Tests>]
let tests =
    testList
        "Numerix Tests"
        [ testProperty "The mean of an array equals the sum divided by the length"
          <| fun (values: double[]) ->
              if
                  Array.isEmpty values
                  || Array.exists Double.IsNaN values
                  || containsInfinity values
              then
                  true
              else
                  let expectedMean = Array.sum values / double values.Length
                  let actualMean = mean values
                  expectedMean = actualMean ]

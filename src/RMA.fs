module Incremental.Indicators.RMA

open FSharp.Data.Adaptive
open System
open SMA
open Calc



let calcRMA (length: int) (series: Series) : Series =
    let alpha = 1.0 / (double length)

    let calculateRma (source: Series) =
        let mutable previousSum = Double.NaN


        alist {
            for t in source do
                let price = t.Value

                previousSum <-
                    if na previousSum then
                        ((calcSMA length source) |> Series.findByDate t.Date).Value
                    else
                        alpha * price + (1.0 - alpha) * nz previousSum

                yield
                    {| Date = t.Date
                       Value = double previousSum |}
        }

    calculateRma series

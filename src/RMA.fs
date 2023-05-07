module Incremental.Indicators.RMA

open FSharp.Data.Adaptive
open System
open SMA
open Calc


(*
let rma (length: int) (quotes: Quotes) =
    let alpha = 1.0 / (double length)

    let calculateRma (source: Quotes) =
        let mutable previousSum =
            { Date = DateTime.MinValue
              Value = Double.NaN }

        let rmaSeq =
            alist {
                for price in source.CloseTuples do
                    let currentPrice = snd price

                    previousSum <-
                        if na previousSum.Value then
                            calcSMA length source
                        else
                            alpha * price + (1.0 - alpha) * nz (previousSum)

                    yield previousSum
            }

        List.ofSeq rmaSeq

    calculateRma quotes
*)

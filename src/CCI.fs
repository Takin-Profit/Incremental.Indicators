module internal Incremental.Indicators.CCI

open System
open FSharp.Data.Adaptive
open Calc
open Util

type CciResult = { Value: float; Date: DateTime }

// cci calculation function
let calcCCI (lookBack: int) (quotes: Quotes) =
    // create typical Prices list
    let typicalPrices =
        quotes.DoublePrecis |> AList.map (fun q -> (q.High + q.Low + q.Close) / 3.0)

    let mutable count = 0

    alist {

        for quote in quotes.DoublePrecis do
            count <- count + 1
            // check for enough data to calculate based on lookBack length
            if count >= lookBack then
                // get the current typical price
                let! currentTP = getVal count 0.0 typicalPrices
                // position to start grabbing items from in the typicalPrices list
                let offset = count - lookBack
                // grab a chunk of data from the typicalPrices list to calculate the SMA with
                let period = AList.sub offset lookBack typicalPrices
                // sma of typical prices over given lookBack period
                let! typicalPriceSMA = AList.average period
                // mean deviation
                let! deviationSMA = meanDev period

                // cci value calculation
                let cciValue =
                    if deviationSMA <> 0.0 then
                        let numerator = (currentTP - typicalPriceSMA)
                        let denominator = (double 0.015 * deviationSMA)
                        (numerator / denominator)
                    else
                        Double.NaN

                yield { Value = cciValue; Date = quote.Date }

    }

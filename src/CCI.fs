module Incremental.Indicators.CCI

open System
open FSharp.Data.Adaptive
open Quotes
open Calc

type CciResult = { Value: float; Date: DateTime }


let private convert quotes =
    toQuoteDSet quotes |> ASet.toAList |> AList.sortBy (fun q -> q.Date)

// cci calculation function
let calcCCI quotes (lookBack: int) =
    // convert Quotes to QuoteD
    let newQuotes = convert quotes
    // create typical Prices list
    let typicalPrices =
        newQuotes |> AList.map (fun q -> (q.High + q.Low + q.Close) / 3.0)

    alist {
        let! len = AList.count newQuotes

        for i in 0..len do
            // check for enough data to calculate based on lookBack length
            if i + 1 >= lookBack then
                // offset is the number of items to skip in the list
                let offset = i + 1 - lookBack
                // grab a chunk of data from the typicalPrices list to calculate the SMA with
                let period = AList.sub offset lookBack typicalPrices
                // sma of typical prices over given lookBack period
                let! typicalPriceSMA = AList.average period
                // mean deviation of the typical prices from the SMA
                let! deviationSMA = meanDev period

                // cci value calculation
                let cciValue =
                    if deviationSMA <> 0.0 then
                        let factor = double 0.015
                        let numerator = (typicalPrices[i] - typicalPriceSMA)
                        let denominator = (factor * deviationSMA)
                        (numerator / denominator)
                    else
                        Double.NaN

                // create CciResult record
                let! result =
                    match cciValue with
                    | Some value ->
                        { Value = value
                          Date = newQuotes[i].Date }
                        |> Some
                    | None -> None

                // return CciResult record as incremental value
                yield result

    }



// let create a (quotes: Quote cset) =
//     if a < 1 then
//         Error("LookBack periods must be greater than 0 for Commodity Channel Index.")
//     else
//         let newQuotes = convert quotes
//         let typicalPrices = getTypicalPrices newQuotes
//         let sma = typicalPrices

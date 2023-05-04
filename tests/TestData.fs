module Incremental.Indicators.Tests.TestData

open Incremental.Indicators
open FSharp.Data.Adaptive
open FSharp.Data
open System


let private csvFile name =
    CsvFile.Load(__SOURCE_DIRECTORY__ + $"Data/{name}").Cache()

let private quoteFromCsv (row: CsvRow) =
    if row.Columns |> Array.contains "" then
        None
    else

        let parseDate str =
            try
                let dt = DateTime.Parse str
                Some dt
            with _ ->
                None

        let date = row.GetColumn "date" |> parseDate

        date
        |> Option.map (fun dt ->
            { Quote.Date = dt
              Open = row.GetColumn "open" |> decimal
              High = row.GetColumn "high" |> decimal
              Low = row.GetColumn "low" |> decimal
              Close = row.GetColumn "close" |> decimal
              Volume = row.GetColumn "volume" |> decimal })


let private getQuotes file days =
    let file = csvFile file

    let quotes =
        seq {
            for row in file.Rows do
                yield quoteFromCsv row
        }

    if quotes |> Seq.exists Option.isNone then
        None
    else
        quotes
        |> Seq.map (fun t -> Option.defaultValue Quote.Empty t)
        |> Seq.take days
        |> Seq.sortByDescending (fun t -> t.Date)
        |> AList.ofSeq
        |> Some

// DEFAULT: S&P 500 ~2 years of daily data
let getDefault days = getQuotes "default.csv" days

let getZeroes days = getQuotes "zeroes.csv" days

let getBad days = getQuotes "bad.csv" days

let getTooBig days = getQuotes "toobig.csv" days

let getMax days = getQuotes "toobig.csv" days
// bitcoin data
let getBitcoin days = getQuotes "bitcoin.csv" days
// COMPARE DATA ~2 years of TSLA data (matches default time)
let getCompare days = getQuotes "compare.csv" days
// INTRADAY DATA
let getIntraDay days = getQuotes "intraday.csv" days
// LONGISH DATA ~20 years of S&P 500 daily data
let getLongish days = getQuotes "longish.csv" days
// LONGEST DATA ~62 years of S&P 500 daily data
let getLongest days = getQuotes "longest.csv" days
// Penny Data
let getPenny days = getQuotes "penny.csv" days
// MISMATCH DATA is in incorrect sequence
let getMismatch days = getQuotes "mismatch.csv" days
// SPX, 30 years, daily
let getSpx days = getQuotes "spx.csv" days
// MSFT, 30 years, daily
let getMsft days = getQuotes "msft.csv" days
// BTCUSD, 69288 records, 15-minute bars
let getBtcUsdNan days = getQuotes "btcusd15x69k.csv" days

let getTupleNan =
    let timeFactor = double 10000000
    let mutable date = DateTime.UtcNow

    alist {

        for _ in 0 .. 25 - 1 do
            date <- date.AddDays 1
            let value = double (date.ToFileTime()) / timeFactor
            yield (date, value)

        for _ in 0 .. 25 - 1 do
            date <- date.AddDays 1
            let value = double (- date.ToFileTime()) / timeFactor
            yield (date, value)

        for _ in 0 .. 25 - 1 do
            date <- date.AddDays 1
            let value = double 0
            yield (date, value)

        for _ in 0 .. 25 - 1 do
            date <- date.AddDays 1
            let value = double -10
            yield (date, value)

        for _ in 0 .. 25 - 1 do
            date <- date.AddDays 1
            let value = double 10
            yield (date, value)

        for _ in 0 .. 25 - 1 do
            date <- date.AddDays 1
            let value = Double.NaN
            yield (date, value)

        for _ in 0 .. 25 - 1 do
            date <- date.AddDays 1
            let value = double (date.ToFileTime()) / timeFactor
            yield (date, value)

    }

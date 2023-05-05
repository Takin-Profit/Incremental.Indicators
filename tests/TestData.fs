module Incremental.Indicators.Tests.TestData

open Incremental.Indicators
open FSharp.Data.Adaptive
open FSharp.Data
open System

let private generateRandomGbm (bars: int) (volatility: double) (drift: double) (seed: double) =
    let randomPrice (seed: double) (volatility: double) (drift: double) =
        let rnd = Random(int DateTime.UtcNow.Ticks)
        let u1 = 1.0 - rnd.NextDouble()
        let u2 = 1.0 - rnd.NextDouble()
        let z = Math.Sqrt(-2.0 * Math.Log(u1)) * Math.Sin(2.0 * Math.PI * u2)
        seed * Math.Exp(drift - (volatility * volatility * 0.5) + (volatility * z))

    let mutable seed = seed
    let volatility = volatility * 0.01
    let drift = drift * 0.01

    let quotes = ResizeArray<Quote>()

    for i in 0 .. (bars - 1) do
        let date = DateTime.Today.AddDays(double (bars - i))
        let openPrice = randomPrice seed (volatility * volatility) drift
        let close = randomPrice openPrice volatility drift

        let ocMax = Math.Max(openPrice, close)
        let high = randomPrice seed (volatility * 0.5) 0.0
        let high = if high < ocMax then (2.0 * ocMax) - high else high

        let ocMin = Math.Min(openPrice, close)
        let low = randomPrice seed (volatility * 0.5) 0.0
        let low = if low > ocMin then (2.0 * ocMin) - low else low

        let volume = randomPrice (seed * 10.0) (volatility * 2.0) 0.0

        let quote: Quote =
            { Date = date
              Open = decimal openPrice
              High = decimal high
              Low = decimal low
              Close = decimal close
              Volume = decimal volume }

        seed <- close
        quotes.Add(quote)

    quotes.ToArray() |> AList.ofArray

let private csvFile name =
    CsvFile.Load(__SOURCE_DIRECTORY__ + $"/Data/{name}").Cache()

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

        let date = row.Columns[0] |> parseDate

        date
        |> Option.map (fun dt ->
            { Quote.Date = dt
              Open = row.Columns[1] |> decimal
              High = row.Columns[2] |> decimal
              Low = row.Columns[3] |> decimal
              Close = row.Columns[4] |> decimal
              Volume = row.Columns[5] |> decimal })


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
        let f =
            quotes
            |> Seq.map (fun t -> Option.defaultValue Quote.Empty t)
            |> Seq.sortByDescending (fun t -> t.Date)

        if days <> 0 then
            f |> Seq.take days |> AList.ofSeq |> Some
        else
            f |> AList.ofSeq |> Some

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
let getMismatch = getQuotes "mismatch.csv" 0
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

let getRandom = generateRandomGbm 502 1.0 0.05 10000000.0

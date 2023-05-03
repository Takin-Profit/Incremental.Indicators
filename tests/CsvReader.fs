module Incremental.Indicators.Tests.CsvReader

open Incremental.Indicators
open FSharp.Data
open System


let getCsvRow = CsvFile.Load(__SOURCE_DIRECTORY__ + "/../data/MSFT.csv").Cache()


let rec quoteFromCsv (row: CsvRow) =
    let parseDate str =
        try
            let dt = DateTime.Parse str
            Some dt
        with _ ->
            None

    let date = row.GetColumn "date" |> parseDate
    let opn = row.GetColumn "open" |> decimal
    let close = row.GetColumn "close" |> decimal
    let high = row.GetColumn "high" |> decimal
    let low = row.GetColumn "low" |> decimal
    let vol = row.GetColumn "volume" |> decimal

    match date with
    | None -> None
    | Some dt ->
        Some(
            { Quote.Date = dt
              Open = opn
              High = high
              Low = low
              Close = close
              Volume = vol }
        )

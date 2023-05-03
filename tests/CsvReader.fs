module Incremental.Indicators.Tests.CsvReader

open Incremental.Indicators
open FSharp.Data
open System


let getCsvRow = CsvFile.Load(__SOURCE_DIRECTORY__ + "/../data/MSFT.csv").Cache()


let quoteFromCsv (row: CsvRow) =
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

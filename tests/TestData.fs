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
        |> AList.ofSeq
        |> Some
        |> Option.map (AList.take days)
        |> Option.map (AList.sortByDescending (fun t -> t.Date))

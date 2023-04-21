module Incremental.Indicators.SMA

open System
open FSharp.Data.Adaptive

type Quote =
    { Date: DateTime
      Open: decimal
      High: decimal
      Low: decimal
      Close: decimal
      Volume: decimal }

type SMAResult = { Date: DateTime; Sma: double }

type SMA(quotes: cset<Quote>) as x =
    // Example of where the SMA the actual calculation would happen
    let mutable _results: aset<SMAResult> =
        x.Quotes
        |> ASet.map (fun (x: Quote) ->
            { Date = x.Date
              Sma = x.Close * x.Volume |> double }

        )

    member x.results = AVal.force _results.Content

    member private x.Quotes = quotes

module TakinProfit.Incremental.Indicators.Quotes

open System
open FSharp.Data.Adaptive

let fromCsv path = failwith "todo"
let fromJSON json = failwith "todo"

type Quote =
    { Date: DateTime
      Open: decimal
      High: decimal
      Low: decimal
      Close: decimal
      Volume: decimal }


type QuoteDouble =
    internal
        { Date: DateTime
          Open: double
          High: double
          Low: double
          Close: double
          Volume: double }

let private _quotesList: cset<Quote> = cset []

let all = _quotesList

let add quotes =
    for quote in quotes do
        transact (fun () -> _quotesList.Add(quote)) |> ignore

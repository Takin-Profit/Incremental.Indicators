module Incremental.Indicators.Quotes

open Incremental.Indicators.Series
open Incremental.Indicators.Types
open System
open FSharp.Data.Adaptive
open System.Globalization
open System.Collections.ObjectModel

let fromCsv path = failwith "todo"
let fromJSON json = failwith "todo"

type IQuote =
    inherit ISeries
    abstract Open: decimal with get
    abstract High: decimal with get
    abstract Low: decimal with get
    abstract Close: decimal with get
    abstract Volume: decimal with get


type Quote =
    { Date: DateTime
      Open: decimal
      High: decimal
      Low: decimal
      Close: decimal
      Volume: decimal }

    interface IQuote with
        member this.Date = this.Date
        member this.Close = this.Close
        member this.High = this.High
        member this.Low = this.Low
        member this.Open = this.Open
        member this.Volume = this.Volume

type internal QuoteD =
    { Date: DateTime
      Open: double
      High: double
      Low: double
      Close: double
      Volume: double }

//validate there are no quotes with duplicate dates
let validate<'TQuote when 'TQuote :> IQuote> (quotes: seq<'TQuote>) : Result<seq<'TQuote>, string> =
    // we cannot rely on date consistency when looking back, so we force sort
    let sortedQuotes = toSortedList quotes

    // Check for duplicates
    let mutable lastDate = DateTime.MinValue

    let duplicates =
        sortedQuotes
        |> Seq.tryFind (fun q ->
            let foundDuplicate = lastDate = q.Date
            lastDate <- q.Date
            foundDuplicate)

    match duplicates with
    | Some duplicateQuote -> Error $"Duplicate date found on %A{duplicateQuote.Date}."
    | None -> Ok(sortedQuotes)

let nativeCulture = System.Threading.Thread.CurrentThread.CurrentUICulture

// STANDARD DECIMAL QUOTES
// convert TQuote element to basic tuple
let quoteToTuple<'TQuote when 'TQuote :> IQuote> (q: 'TQuote) (candlePart: CandlePart) =
    match candlePart with
    | CandlePart.Open -> (q.Date, double q.Open)
    | CandlePart.High -> (q.Date, double q.High)
    | CandlePart.Low -> (q.Date, double q.Low)
    | CandlePart.Close -> (q.Date, double q.Close)
    | CandlePart.Volume -> (q.Date, double q.Volume)
    | CandlePart.HL2 -> (q.Date, double (q.High + q.Low) / 2.0)
    | CandlePart.HLC3 -> (q.Date, double (q.High + q.Open + q.Close) / 3.0)
    | CandlePart.OC2 -> (q.Date, double (q.Open + q.Close) / 2.0)
    | CandlePart.OHL3 -> (q.Date, double (q.Open + q.High + q.Low) / 3.0)
    | CandlePart.OHLC4 -> (q.Date, double (q.Open + q.High + q.Low + q.Close) / 4.0)

// convert seq<Quote> to seq<DateTime * double>
// same as https://github.com/DaveSkender/Stock.Indicators/blob/1ffd1333e00593e94ae6c7a9c6ff04acb3f48d1e/src/_common/Quotes/Quote.Converters.cs#L17
let toTupleSeq<'TQuote when 'TQuote :> IQuote> (candlePart: CandlePart) (quotes: seq<'TQuote>) =
    quotes |> Seq.map (fun x -> quoteToTuple x candlePart)

// convert seq<Quote> to (DateTime * double) list sorted by date

let quotesToSortedList<'TQuote when 'TQuote :> IQuote> (candlePart: CandlePart) (quotes: seq<'TQuote>) =
    quotes
    |> Seq.sortBy (fun x -> x.Date)
    |> Seq.map (fun x -> quoteToTuple x candlePart)
    |> List.ofSeq

// convert seq<DateTime * double> to (DateTime * double) list sorted by date
let toSortedList<'TQuote when 'TQuote :> IQuote> (candlePart: CandlePart) (quotes: seq<DateTime * double>) =
    quotes |> Seq.sortBy fst |> Seq.toList

// convert seq<Quote> to (Quote * double) array
let toTupleArray<'TQuote when 'TQuote :> IQuote> (candlePart: CandlePart) (quotes: seq<'TQuote>) =
    toTupleSeq candlePart quotes |> Array.ofSeq

// convert seq<DateTime * double> to (DateTime * double) list sorted by date
let tuplesToSortedList (tuples: seq<DateTime * double>) = tuples |> Seq.sortBy fst |> Seq.toList

// convert seq<DateTime * double> to (DateTime * double) array sorted by date
let toSortedTupleArray (tuples: seq<DateTime * double>) =
    tuples |> tuplesToSortedList |> Array.ofList

// DOUBLE QUOTES

// convert to quotes in double precision
let toQuoteDList<'TQuote when 'TQuote :> IQuote> (quotes: seq<'TQuote>) : QuoteD list =
    quotes
    |> Seq.map (fun x ->
        { Date = x.Date
          Open = float x.Open
          High = float x.High
          Low = float x.Low
          Close = float x.Close
          Volume = float x.Volume })
    |> Seq.sortBy (fun x -> x.Date)
    |> Seq.toList

// convert quoteD list to tuples
let toTupleListFromQuoteDList (qdList: QuoteD list) (candlePart: CandlePart) : (DateTime * float) list =
    qdList
    |> Seq.sortBy (fun x -> x.Date)
    |> Seq.map (fun x -> x.ToTuple(candlePart))
    |> Seq.toList



let private _quotesList: cset<Quote> = cset []

let all = _quotesList

let add quotes =
    for quote in quotes do
        transact (fun () -> _quotesList.Add(quote)) |> ignore

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

// convert Quotes to seq<DateTime * double>
let toTupleSeq<'TQuote when 'TQuote :> IQuote> (quotes: seq<'TQuote>) (candlePart: CandlePart) =
    quotes |> Seq.map (fun x -> quoteToTuple x candlePart)

// TUPLE QUOTES

// convert quotes to tuple list
let toTupleCollection<'TQuote when 'TQuote :> IQuote>
    (quotes: seq<'TQuote>)
    (candlePart: CandlePart)
    : Collection<DateTime * float> =
    quotes |> QuoteUtility.ToTupleList(candlePart) |> Seq.toCollection

let toTupleList<'TQuote when 'TQuote :> IQuote>
    (quotes: seq<'TQuote>)
    (candlePart: CandlePart)
    : (DateTime * float) list =
    quotes
    |> Seq.sortBy (fun x -> x.Date)
    |> Seq.map (fun x -> x.ToTuple(candlePart))
    |> Seq.toList

// convert tuples to list, with sorting
let toSortedCollection (tuples: seq<DateTime * float>) : Collection<DateTime * float> =
    tuples |> toSortedList |> Seq.toCollection

let toSortedList (tuples: seq<DateTime * float>) : (DateTime * float) list =
    tuples |> Seq.sortBy (fun x -> x |> fst) |> Seq.toList

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

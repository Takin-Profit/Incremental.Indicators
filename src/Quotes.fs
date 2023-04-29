module Incremental.Indicators.Quotes

open System
open FSharp.Data.Adaptive
open Series
open Types
open Calc

type Quote =
    { Date: DateTime
      Open: decimal
      High: decimal
      Low: decimal
      Close: decimal
      Volume: decimal }

    static member Empty =
        { Date = DateTime.MaxValue
          Open = 0m
          Close = 0m
          High = 0m
          Low = 0m
          Volume = 0m }

    static member IsEmpty q = q.Date = DateTime.MaxValue


type internal QuoteD =
    { Date: DateTime
      Open: double
      High: double
      Low: double
      Close: double
      Volume: double }

    static member Empty =
        { Date = DateTime.MaxValue
          Open = 0.0
          Close = 0.0
          High = 0.0
          Low = 0.0
          Volume = 0.0 }

    static member IsEmpty q = q.Date = DateTime.MaxValue




//validate there are no quotes with duplicate dates
let validate (quotes: Quote alist) : Result<Quote alist, string> =
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

let createQuotes (quotes: seq<Quote>) = validate quotes |> Result.map cset



let nativeCulture = System.Threading.Thread.CurrentThread.CurrentUICulture

// STANDARD DECIMAL QUOTES
// convert TQuote element to basic tuple
let quoteToTuple (q: Quote) (candlePart: CandlePart) =
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

// convert cset<Quote> to cset<DateTime * double>
let toTuples (candlePart: CandlePart) (quotes: Quote cset) =
    quotes |> ASet.map (fun x -> quoteToTuple x candlePart)

// convert Quote clist to (DateTime * double) clist sorted by date
// candlePart determines the part of price to be used
let toSortedSet (candlePart: CandlePart) (quotes: Quote clist) =
    quotes
    |> AList.sortBy (fun x -> x.Date)
    |> AList.map (fun x -> quoteToTuple x candlePart)

// DOUBLE QUOTES
// convert to quotes in double precision
// QuoteD alist sorted by date
let internal toQuoteDList (quotes: Quote clist) =
    quotes
    |> AList.map (fun x ->
        { Date = x.Date
          Open = double x.Open
          High = double x.High
          Low = double x.Low
          Close = double x.Close
          Volume = double x.Volume })
    |> AList.sortBy (fun x -> x.Date)


let internal quoteDtoTuple (q: QuoteD) (candlePart: CandlePart) =
    match candlePart with
    | CandlePart.Open -> (q.Date, q.Open)
    | CandlePart.High -> (q.Date, q.High)
    | CandlePart.Low -> (q.Date, q.Low)
    | CandlePart.Close -> (q.Date, q.Close)
    | CandlePart.Volume -> (q.Date, q.Volume)
    | CandlePart.HL2 -> (q.Date, (q.High + q.Low) / 2.0)
    | CandlePart.HLC3 -> (q.Date, (q.High + q.Low + q.Close) / 3.0)
    | CandlePart.OC2 -> (q.Date, (q.Open + q.Close) / 2.0)
    | CandlePart.OHL3 -> (q.Date, (q.Open + q.High + q.Low) / 3.0)
    | CandlePart.OHLC4 -> (q.Date, (q.Open + q.High + q.Low + q.Close) / 4.0)

// convert quoteD list to tuples
let internal quoteDSetToToTuples (candlePart: CandlePart) (qdList: QuoteD clist) =
    qdList
    |> AList.sortBy (fun x -> x.Date)
    |> AList.map (fun x -> quoteDtoTuple x candlePart)
    |> AList.toASet

// aggregation (quantization) using TimeSpan>
///
let aggregateByTimeSpan (timeSpan: TimeSpan) (quotes: Quote clist) =
    // handle no quotes scenario
    if quotes.IsEmpty then
        Ok AList.empty
    else if timeSpan <= TimeSpan.Zero then
        Error
            $"Quotes Aggregation must use a usable new size value (see documentation for options). Value: %A{timeSpan}"
    else
        // return aggregation
        quotes
        |> AList.sortBy (fun x -> x.Date)
        |> AList.groupBy (fun x -> roundDown x.Date timeSpan)
        |> AMap.map (fun x v ->
            { Quote.Date = x
              Open = IndexList.first v |> fun t -> t.Open
              Quote.High = Seq.maxBy (fun (t: Quote) -> t.High) v |> fun t -> t.High
              Low = Seq.minBy (fun (t: Quote) -> t.Low) v |> fun t -> t.Low
              Close = Seq.last v |> fun t -> t.Close
              Volume = Seq.sumBy (fun (t: Quote) -> t.Volume) v })

        |> AMap.toASetValues
        |> ASet.toAList
        |> Ok

let aggregateByTimeFrame (timeFrame: TimeFrame) (quotes: Quote clist) =
    if timeFrame <> TimeFrame.Month then
        // parameter conversion
        let newTimeSpan = toTimeSpan timeFrame

        // convert
        aggregateByTimeSpan newTimeSpan quotes

    else // month
        quotes
        |> AList.sortBy (fun x -> x.Date)
        |> AList.groupBy (fun x -> DateTime(x.Date.Year, x.Date.Month, 1))
        |> AMap.map (fun x v ->
            { Quote.Date = x
              Open = IndexList.first v |> fun t -> t.Open
              High = Seq.maxBy (fun (t: Quote) -> t.High) v |> fun t -> t.High
              Low = Seq.minBy (fun (t: Quote) -> t.Low) v |> fun t -> t.Low
              Close = Seq.last v |> fun t -> t.Close
              Volume = Seq.sumBy (fun (t: Quote) -> t.Volume) v })
        |> AMap.toASetValues
        |> ASet.toAList
        |> Ok

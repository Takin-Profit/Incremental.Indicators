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

type IBasicData =
    abstract Date: DateTime
    abstract Value: double

type BasicData =
    { Date: DateTime
      Value: double }

    interface IBasicData with
        member this.Date = this.Date
        member this.Value = this.Value
//    TODO uncomment this once Result module is complete
//    interface IReusableResult with
//        member this.Value = box this.Value }



//validate there are no quotes with duplicate dates
let validate (quotes: seq<Quote>) : Result<seq<Quote>, string> =
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

// convert seq<Quote> to (DateTime * double) cset sorted by date
let toSortedSet (candlePart: CandlePart) (quotes: Quote cset) =
    quotes
    |> ASet.sortBy (fun x -> x.Date)
    |> AList.map (fun x -> quoteToTuple x candlePart)
    |> AList.toASet

// convert cset<DateTime * double> clist(DateTime * double)  sorted by date
let toSortedList (tuples: (DateTime * double) cset) = tuples |> ASet.sortBy fst


// DOUBLE QUOTES
// convert to quotes in double precision
let internal toQuoteDSet (quotes: Quote cset) =
    quotes
    |> ASet.map (fun x ->
        { Date = x.Date
          Open = float x.Open
          High = float x.High
          Low = float x.Low
          Close = float x.Close
          Volume = float x.Volume })
    |> ASet.sortBy (fun x -> x.Date)
    |> AList.toASet

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
let internal quoteDSetToToTuples (candlePart: CandlePart) (qdList: QuoteD cset) =
    qdList
    |> ASet.sortBy (fun x -> x.Date)
    |> AList.map (fun x -> quoteDtoTuple x candlePart)
    |> AList.toASet

/// Convert TQuote element to basic data record
let toBasicData (candlePart: CandlePart) (q: Quote) =
    match candlePart with
    | CandlePart.Open -> { Date = q.Date; Value = double q.Open }
    | CandlePart.High -> { Date = q.Date; Value = double q.High }
    | CandlePart.Low -> { Date = q.Date; Value = double q.Low }
    | CandlePart.Close ->
        { Date = q.Date
          Value = double q.Close }
    | CandlePart.Volume ->
        { Date = q.Date
          Value = double q.Volume }
    | CandlePart.HL2 ->
        { Date = q.Date
          Value = (double (q.High + q.Low) / 2.0) }
    | CandlePart.HLC3 ->
        { Date = q.Date
          Value = (double (q.High + q.Low + q.Close) / 3.0) }
    | CandlePart.OC2 ->
        { Date = q.Date
          Value = (double (q.Open + q.Close) / 2.0) }
    | CandlePart.OHL3 ->
        { Date = q.Date
          Value = (double (q.Open + q.High + q.Low) / 3.0) }
    | CandlePart.OHLC4 ->
        { Date = q.Date
          Value = (double (q.Open + q.High + q.Low + q.Close) / 4.0) }


// aggregation (quantization) using TimeSpan>
///
let aggregateByTimeSpan (timeSpan: TimeSpan) (quotes: Quote cset) =
    // handle no quotes scenario
    if quotes.IsEmpty then
        Ok ASet.empty
    else if timeSpan <= TimeSpan.Zero then
        Error
            $"Quotes Aggregation must use a usable new size value (see documentation for options). Value: %A{timeSpan}"
    else
        // return aggregation
        quotes
        |> ASet.sortBy (fun x -> x.Date)
        |> AList.groupBy (fun x -> roundDown x.Date timeSpan)
        |> AMap.map (fun x v ->
            { Quote.Date = x
              Open = IndexList.first v |> fun t -> t.Open
              Quote.High = Seq.maxBy (fun (t: Quote) -> t.High) v |> fun t -> t.High
              Low = Seq.minBy (fun (t: Quote) -> t.Low) v |> fun t -> t.Low
              Close = Seq.last v |> fun t -> t.Close
              Volume = Seq.sumBy (fun (t: Quote) -> t.Volume) v })

        |> AMap.toASet
        |> Ok

let aggregateByTimeFrame (timeFrame: TimeFrame) (quotes: Quote cset) =
    if timeFrame <> TimeFrame.Month then
        // parameter conversion
        let newTimeSpan = toTimeSpan timeFrame

        // convert
        aggregateByTimeSpan newTimeSpan quotes

    else // month
        quotes
        |> ASet.sortBy (fun x -> x.Date)
        |> AList.groupBy (fun x -> DateTime(x.Date.Year, x.Date.Month, 1))
        |> AMap.map (fun x v ->
            { Quote.Date = x
              Open = IndexList.first v |> fun t -> t.Open
              High = Seq.maxBy (fun (t: Quote) -> t.High) v |> fun t -> t.High
              Low = Seq.minBy (fun (t: Quote) -> t.Low) v |> fun t -> t.Low
              Close = Seq.last v |> fun t -> t.Close
              Volume = Seq.sumBy (fun (t: Quote) -> t.Volume) v })
        |> AMap.toASet
        |> Ok

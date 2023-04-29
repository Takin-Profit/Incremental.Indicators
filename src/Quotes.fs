module Incremental.Indicators.Quotes

open System
open FSharp.Data.Adaptive
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
let validate (quotes: Quote seq) =
    // we cannot rely on date consistency when looking back, so we force sort
    let sortedQuotes = quotes |> Seq.sortBy (fun x -> x.Date)

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

// convert a sequence of quotes to a clist
let createQuotes (quotes: seq<Quote>) = validate quotes |> Result.map clist



let nativeCulture = System.Threading.Thread.CurrentThread.CurrentUICulture

// convert TQuote element to basic tuple of double precision values
let quoteToTuple (candlePart: CandlePart) (q: Quote) =
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

// convert Quote alist to (DateTime * double) alist
// candlePart determines the part of price to be used
let toTuples (candlePart: CandlePart) (quotes: Quote alist) =
    quotes |> AList.map (quoteToTuple candlePart) |> AList.sortBy fst

// convert to quotes in double precision
// QuoteD alist sorted by date
let internal toQuoteDList (quotes: Quote alist) =
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
let internal quoteDListToToTuples (candlePart: CandlePart) (qdList: QuoteD alist) =
    qdList
    |> AList.sortBy (fun x -> x.Date)
    |> AList.map (fun x -> quoteDtoTuple x candlePart)

// aggregation (quantization) using TimeSpan>
///
let aggregateByTimeSpan (timeSpan: TimeSpan) (quotes: Quote alist) =
    let count = AList.count quotes |> AVal.force
    // handle no quotes scenario
    if count < 1 then
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

let aggregateByTimeFrame (timeFrame: TimeFrame) (quotes: Quote alist) =
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


[<Serializable>]
type CandleProps =
    { High: decimal
      Low: decimal
      Open: decimal
      Close: decimal
      // raw sizes
      Size: decimal
      Body: decimal
      UpperWick: decimal
      LowerWick: decimal
      // percent sizes
      BodyPct: double
      UpperWickPct: double
      LowerWickPct: double
      // directional info
      IsBullish: bool
      IsBearish: bool }

let makeCandleProps (quote: Quote) =
    let high = quote.High
    let low = quote.Low
    let openPrice = quote.Open
    let close = quote.Close
    let size = high - low

    let body =
        if openPrice > close then
            openPrice - close
        else
            close - openPrice

    let upperWick = high - if openPrice > close then openPrice else close
    let lowerWick = if openPrice > close then close else openPrice - low

    { High = high
      Low = low
      Open = openPrice
      Close = close
      Size = size
      Body = body
      UpperWick = upperWick
      LowerWick = lowerWick
      BodyPct = if size <> 0m then double (body / size) else 1
      UpperWickPct = if size <> 0m then double (upperWick / size) else 1
      LowerWickPct = if size <> 0m then double (lowerWick / size) else 1
      IsBullish = close > openPrice
      IsBearish = close < openPrice }

[<Serializable>]
type CandleResult =
    { Date: DateTime
      Price: decimal
      Match: Match
      Candle: CandleProps }

// TODO: fix this constructor function to create price correctly as well as match Prop.
let makeCandleResult (quote: Quote) =
    { Date = quote.Date
      Price = quote.Close
      Match = Match.Neutral
      Candle = makeCandleProps quote }

module Incremental.Indicators.Candle

open FSharp.Data.Adaptive
open System
open Types

// properties of each candle
[<Serializable>]
type Props =
    { Date: DateTime
      High: decimal
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

let internal makeCandleProps (quote: Quote) =
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

    { Date = quote.Date
      High = high
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
      Candle: Props }

// TODO: fix this constructor function to create price correctly as well as match Prop.
let internal makeCandleResult (quote: Quote) =
    { Date = quote.Date
      Price = quote.Close
      Match = Match.None
      Candle = makeCandleProps quote }

let condense results =
    results |> AList.filter (fun t -> t.Match <> Match.None)

let internal fromQuote quote = makeCandleProps quote

/// convert/sort quotes into candles list
let toCandles quotes =
    quotes |> AList.map fromQuote |> AList.sortBy (fun t -> t.Date)

let internal toCandleResults quotes =
    quotes |> AList.map makeCandleResult |> AList.sortBy (fun t -> t.Date)

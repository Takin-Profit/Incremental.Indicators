module Incremental.Indicators.RSI

open FSharp.Data.Adaptive
open System

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

let internal getVal offset defaultVal list =
    aval {
        let! item = AList.sub offset 1 list |> AList.tryFirst
        return Option.defaultValue defaultVal item
    }

type RsiResult = { Value: double; Date: DateTime }

let calculateRSI (quotes: Quote alist) (period: int) =
    let newQuotes = toQuoteDList quotes

    alist {
        let! len = AList.count newQuotes
        let mutable gain: double = 0.0
        let mutable loss: double = 0.0
        let mutable rs: double = 0.0
        let mutable rsi: double = 0.0

        for i in 1..period do
            let! currentQuote = getVal i QuoteD.Empty newQuotes
            let! previousQuote = getVal (i - 1) QuoteD.Empty newQuotes
            let diff = currentQuote.Close - previousQuote.Close

            if diff > 0.0 then
                gain <- gain + diff
            else
                loss <- loss + abs (diff)

        rs <- gain / loss
        rsi <- 100.0 - (100.0 / (1.0 + rs))

        for i in (period + 1) .. len - 1 do
            let! currentQuote = getVal i QuoteD.Empty newQuotes
            let! previousQuote = getVal (i - 1) QuoteD.Empty newQuotes
            let diff = currentQuote.Close - previousQuote.Close

            if diff > 0.0 then
                gain <- gain + diff - (gain / double period)
                loss <- loss - (loss / double period)
            else
                loss <- loss + abs (diff) - (loss / double period)
                gain <- gain - (gain / double period)

            rs <- gain / loss
            rsi <- 100.0 - (100.0 / (1.0 + rs))

        rsi

    }

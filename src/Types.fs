module Incremental.Indicators.Types

open System
open Quotes

[<RequireQualifiedAccess>]
type CandlePart =
    | Open
    | High
    | Low
    | Close
    | Volume
    | HL2
    | HLC3
    | OC2
    | OHL3
    | OHLC4

[<RequireQualifiedAccess>]
type EndType =
    | Close
    | HighLow

type Match =
    | BullConfirmed = 200
    | BullSignal = 100
    | BullBasis = 10
    | Neutral = 1
    | None = 0
    | BearBasis = -10
    | BearSignal = -100
    | BearConfirmed = -200

[<RequireQualifiedAccess>]
type MaType =
    | ALMA
    | DEMA
    | EPMA
    | EMA
    | HMA
    | KAMA
    | MAMA
    | SMA
    | SMMA
    | TEMA
    | WMA

[<RequireQualifiedAccess>]
type TimeFrame =
    | Month
    | ThreeWeeks
    | TwoWeeks
    | Week
    | ThirtyDays
    | TwentyDays
    | FifteenDays
    | TenDays
    | FiveDays
    | ThreeDays
    | TwoDays
    | OneDay
    | TwentyHours
    | EightTeenHours
    | SixteenHours
    | FourteenHours
    | TwelveHours
    | EightHours
    | TenHours
    | SixHours
    | FourHours
    | ThreeHours
    | TwoHours
    | OneHour
    | ThreeHundredNinetyMin
    | TwoHundredSixtyMin
    | OneHundredThirtyMin
    | SixtyFiveMin
    | FortyFiveMin
    | ThirtyMin
    | TwentyFourMin
    | FifteenMin
    | TwelveMin
    | FiveMin
    | ThreeMin
    | OneMin

let toTimeSpan timeFrame =
    match timeFrame with
    // Month TimeSpan will conversion is incorrect but will never get called
    // this is here to satisfy the compiler
    | TimeFrame.Month -> TimeSpan.FromDays 30
    | TimeFrame.ThreeWeeks -> TimeSpan.FromDays 21
    | TimeFrame.TwoWeeks -> TimeSpan.FromDays 14
    | TimeFrame.Week -> TimeSpan.FromDays 7
    | TimeFrame.ThirtyDays -> TimeSpan.FromDays 30
    | TimeFrame.TwentyDays -> TimeSpan.FromDays 20
    | TimeFrame.FifteenDays -> TimeSpan.FromDays 15
    | TimeFrame.TenDays -> TimeSpan.FromDays 10
    | TimeFrame.FiveDays -> TimeSpan.FromDays 5
    | TimeFrame.ThreeDays -> TimeSpan.FromDays 3
    | TimeFrame.TwoDays -> TimeSpan.FromDays 2
    | TimeFrame.OneDay -> TimeSpan.FromDays 1
    | TimeFrame.TwentyHours -> TimeSpan.FromHours 20
    | TimeFrame.EightTeenHours -> TimeSpan.FromHours 18
    | TimeFrame.SixteenHours -> TimeSpan.FromHours 16
    | TimeFrame.FourteenHours -> TimeSpan.FromHours 14
    | TimeFrame.TwelveHours -> TimeSpan.FromHours 12
    | TimeFrame.TenHours -> TimeSpan.FromHours 10
    | TimeFrame.EightHours -> TimeSpan.FromHours 8
    | TimeFrame.SixHours -> TimeSpan.FromHours 6
    | TimeFrame.FourHours -> TimeSpan.FromHours 4
    | TimeFrame.ThreeHours -> TimeSpan.FromHours 3
    | TimeFrame.TwoHours -> TimeSpan.FromHours 2
    | TimeFrame.OneHour -> TimeSpan.FromHours 1
    | TimeFrame.ThreeHundredNinetyMin -> TimeSpan.FromMinutes 390
    | TimeFrame.TwoHundredSixtyMin -> TimeSpan.FromMinutes 260
    | TimeFrame.OneHundredThirtyMin -> TimeSpan.FromMinutes 130
    | TimeFrame.SixtyFiveMin -> TimeSpan.FromMinutes 65
    | TimeFrame.FortyFiveMin -> TimeSpan.FromMinutes 45
    | TimeFrame.ThirtyMin -> TimeSpan.FromMinutes 30
    | TimeFrame.TwentyFourMin -> TimeSpan.FromMinutes 24
    | TimeFrame.FifteenMin -> TimeSpan.FromMinutes 15
    | TimeFrame.TwelveMin -> TimeSpan.FromMinutes 12
    | TimeFrame.FiveMin -> TimeSpan.FromMinutes 5
    | TimeFrame.ThreeMin -> TimeSpan.FromMinutes 3
    | TimeFrame.OneMin -> TimeSpan.FromMinutes 1

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

module Incremental.Indicators.Types

open System

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

    member self.toTimeSpan() =
        match self with
        | ThreeWeeks -> TimeSpan.FromDays 21
        | TwoWeeks -> TimeSpan.FromDays 14
        | Week -> TimeSpan.FromDays 7
        | ThirtyDays -> TimeSpan.FromDays 30
        | TwentyDays -> TimeSpan.FromDays 20
        | FifteenDays -> TimeSpan.FromDays 15
        | TenDays -> TimeSpan.FromDays 10
        | FiveDays -> TimeSpan.FromDays 5
        | ThreeDays -> TimeSpan.FromDays 3
        | TwoDays -> TimeSpan.FromDays 2
        | OneDay -> TimeSpan.FromDays 1
        | TwentyHours -> TimeSpan.FromHours 20
        | EightTeenHours -> TimeSpan.FromHours 18
        | SixteenHours -> TimeSpan.FromHours 16
        | FourteenHours -> TimeSpan.FromHours 14
        | TwelveHours -> TimeSpan.FromHours 12
        | TenHours -> TimeSpan.FromHours 10
        | EightHours -> TimeSpan.FromHours 8
        | SixHours -> TimeSpan.FromHours 6
        | FourHours -> TimeSpan.FromHours 4
        | ThreeHours -> TimeSpan.FromHours 3
        | TwoHours -> TimeSpan.FromHours 2
        | OneHour -> TimeSpan.FromHours 1
        | ThreeHundredNinetyMin -> TimeSpan.FromMinutes 390
        | TwoHundredSixtyMin -> TimeSpan.FromMinutes 260
        | OneHundredThirtyMin -> TimeSpan.FromMinutes 130
        | SixtyFiveMin -> TimeSpan.FromMinutes 65
        | FortyFiveMin -> TimeSpan.FromMinutes 45
        | ThirtyMin -> TimeSpan.FromMinutes 30
        | TwentyFourMin -> TimeSpan.FromMinutes 24
        | FifteenMin -> TimeSpan.FromMinutes 15
        | TwelveMin -> TimeSpan.FromMinutes 12
        | FiveMin -> TimeSpan.FromMinutes 5
        | ThreeMin -> TimeSpan.FromMinutes 3
        | OneMin -> TimeSpan.FromMinutes 1

module TakinProfit.Incremental.Indicators.Types

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
    | Day
    | TwelveHours
    | EightHours
    | SixHours
    | FourHour
    | ThreeHour
    | TwoHour
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

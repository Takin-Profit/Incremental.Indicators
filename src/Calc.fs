module Incremental.Indicators.Calc

open System
open FSharp.Data.Adaptive


// adaptive mean deviation function
let meanDev (values: double alist) =

    aval {
        let! len = AList.count values

        if len <= 0 then
            return 0.0
        else
            let! mean = AList.average values
            let deviation = AList.map (fun x -> abs (x - mean)) values
            return! AList.average deviation
    }

// adaptive Standard Deviation
let stdDev (values: double alist) =
    aval {
        let! avg = AList.average values
        let squaredDiffs = AList.map (fun v -> (v - avg) ** 2.0) values
        return! AList.average squaredDiffs
    }

// this function does not do any error handling or input validation
// do not use outside of this module
let private zip (x: 'X alist) (y: 'Y alist) =
    alist {
        for i in x do
            for j in y do
                yield (i, j)
    }
// SLOPE of BEST FIT LINE
let slope (x: double alist) (y: double alist) =
    // validate params
    aval {
        let! xLen = AList.count x
        let! yLen = AList.count y

        if xLen <> yLen then
            return Error("Slope x and y must be the same size")
        else
            let! avgX = AList.average x
            let! avgY = AList.average y
            let zipped = zip x y
            let! numerator = AList.map (fun x -> (fst x - avgX) * (snd x - avgY)) zipped |> AList.sum
            let! denominator = AList.map (fun x -> (x - avgX) ** 2.0) x |> AList.sum
            return Ok(numerator / denominator)

    }


// DATE ROUNDING
let roundDown (date: DateTime) (interval: TimeSpan) =
    if interval = TimeSpan.Zero then
        date
    else
        date.AddTicks(-(date.Ticks % interval.Ticks))

// DETERMINE DECIMAL PLACES
// does not currently take into account trailing zeros
let getDecimalPlaces (n: decimal) : int =
    let mutable n = abs n % 1m
    let mutable decimalPlaces = 0

    while n > 0m do
        decimalPlaces <- decimalPlaces + 1
        n <- n * 10m
        n <- abs (n % 1m)

    decimalPlaces

module OptionalMath =
    let abs (value: double option) : double option =
        match value with
        | None -> None
        | Some v -> if v < 0.0 then Some(-v) else Some v

    let roundOptionDecimal (value: decimal option) (digits: int) : decimal option =
        match value with
        | None -> None
        | Some v -> Some(Math.Round(v, digits))

    let roundOptionDouble (value: double option) (digits: int) : double option =
        match value with
        | None -> None
        | Some v -> Some(Math.Round(v, digits))

    let roundDouble (value: double) (digits: int) : double = Math.Round(value, digits)

    let roundDecimal (value: decimal) (digits: int) : decimal = Math.Round(value, digits)

    let noneToNaN (value: double option) : double =
        match value with
        | None -> Double.NaN
        | Some v -> v

    let optionNaNToNone (value: double option) : double option =
        match value with
        | None -> None
        | Some v -> if Double.IsNaN(v) then None else Some v

    let naNToNone (value: double) : double option =
        if Double.IsNaN(value) then None else Some value

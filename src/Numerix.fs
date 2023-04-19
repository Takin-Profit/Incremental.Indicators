module Incremental.Indicators.Numerix

open System

// check for nulls, empty arrays, and existing of NaN or Infinity
let dataIsValid (list: double[]) =
    match list with
    | [||] -> Error("Values cannot be empty")
    | null -> Error("Values cannot be null")
    | _ -> Ok(list)

// calculate the mean
let mean (values: double[]) =
    match dataIsValid values with
    | Ok _ -> Array.sum values / double values.Length
    | Error _ -> Double.NaN


// Standard Deviation
let stdDev (values: double[]) =
    match dataIsValid values with
    | Error(A) -> Error(A)
    | Ok _ ->
        let n = double values.Length
        let avg = mean values
        let squaredDiffs = Array.map (fun v -> (v - avg) ** 2.0) values
        Ok(sqrt (Array.sum squaredDiffs / n))

// SLOPE of BEST FIT LINE
let slope (x: double[]) (y: double[]) =
    // validate params
    match dataIsValid x, dataIsValid y with
    | Error(A), _ -> Error(A)
    | _, Error(A) -> Error(A)
    | Ok(x), Ok(y) when x.Length <> y.Length -> Error("Slope x and y must be the same size")
    | _ ->
        let meanX = mean x
        let meanY = mean y
        let numerator = Array.map2 (fun x y -> (x - meanX) * (y - meanY)) x y |> Array.sum
        let denominator = Array.map (fun x -> (x - meanX) ** 2.0) x |> Array.sum
        Ok(numerator / denominator)

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

    let roundDecimal (value: decimal) (digits: int) : decimal = System.Math.Round(value, digits)

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

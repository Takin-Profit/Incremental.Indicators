module Incremental.Indicators.Numerix

open System

let (|IsNull|_|) value =
    if obj.ReferenceEquals(value, null) then None else Some()

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

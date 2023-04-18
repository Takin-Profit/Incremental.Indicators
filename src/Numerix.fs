module Incremental.Indicators.Numerix

open System

// Check if the array contains infinity as the division will fail
let containsInfinity (list: double[]) =
    list |> Array.contains Double.PositiveInfinity
    || list |> Array.contains Double.NegativeInfinity

// check for nulls, empty arrays, and existing of NaN or Infinity
let dataIsValid (list: double[]) =
    match list with
    | [||] -> Error("Values cannot be empty")
    | null -> Error("Values cannot be null")
    | [| _ |] when containsInfinity list -> Error("Infinity is not allowed")
    | [| _ |] when Array.exists Double.IsNaN list -> Error("NaN is not allowed")
    | _ -> Ok(list)

// calculate the mean
let mean (values: double[]) =
    match dataIsValid values with
    | Error(_) -> double Double.NaN
    | Ok(_) -> Array.sum values / double values.Length

// Standard Deviation
let stdDev (values: double[]) =
    let calc =
        let n = double values.Length
        let avg = mean values
        let squaredDiffs = Array.map (fun v -> (v - avg) ** 2.0) values
        Ok(sqrt (Array.sum squaredDiffs / n))

    match dataIsValid values with
    | Error(A) -> Error(A)
    | Ok(_) -> calc

// SLOPE of BEST FIT LINE
let slope (x: double[]) (y: double[]) =
    let calc =
        let meanX = mean x
        let meanY = mean y
        let numerator = Array.map2 (fun x y -> (x - meanX) * (y - meanY)) x y |> Array.sum
        let denominator = Array.map (fun x -> (x - meanX) ** 2.0) x |> Array.sum
        Ok(numerator / denominator)
    // validate params
    match dataIsValid x, dataIsValid y with
    | Error(A), _ -> Error(A)
    | _, Error(A) -> Error(A)
    | Ok(x), Ok(y) when x.Length <> y.Length -> Error("Slope x and y must be the same size")
    | _ -> calc
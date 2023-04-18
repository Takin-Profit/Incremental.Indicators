module Incremental.Indicators.Numerix

let mean (values: double[]) = Array.sum values / float values.Length

// Standard Deviation
let stdDev (values: double[]) =
    if values = null then
        Error("StdDev values cannot be null.")
    else
        let n = float values.Length
        let avg = mean values
        let squaredDiffs = Array.map (fun v -> (v - avg) ** 2.0) values
        Ok(sqrt (Array.sum squaredDiffs / n))

// SLOPE of BEST FIT LINE
let slope (x: double[]) (y: double[]) =
    // validate params
    let shouldCompute =
        match (x, y) with
        | null, _ -> false
        | _, null -> false
        | x, y when x.Length <> y.Length -> false
        | x, y when Seq.isEmpty x || Seq.isEmpty y -> false
        | _, _ -> true

    if shouldCompute = true then
        let meanX = mean x
        let meanY = mean y
        let numerator = Array.map2 (fun x y -> (x - meanX) * (y - meanY)) x y |> Array.sum
        let denominator = Array.map (fun x -> (x - meanX) ** 2.0) x |> Array.sum
        Ok(numerator / denominator)
    else
        Error("Slope x or y cannot be null and must be the same size")

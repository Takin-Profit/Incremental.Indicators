module TakinProfit.Indicators.Numerix

open FSharp.Stats

let stdDev (values: double[]) = Seq.stDev values

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
        let meanX = Seq.mean x
        let meanY = Seq.mean y
        let numerator = Array.map2 (fun x y -> (x - meanX) * (y - meanY)) x y |> Array.sum
        let denominator = Array.map (fun x -> (x - meanX) ** 2.0) x |> Array.sum
        Ok(numerator / denominator)
    else
        Error("Slope x or y cannot be null and must be the same size")

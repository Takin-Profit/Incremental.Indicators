module TakinProfit.Indicators.Numerix
open FSharp.Stats

let stdDev (values: seq<double>) =
    Seq.stDev values

let slope (x: seq<double>) (y: seq<double>) =
    // validate params
    let shouldCompute =
        match (x, y) with
        | null, _ -> false
        | _, null -> false
        | x, y when Seq.length x <> Seq.length y -> false
        | x,y when Seq.isEmpty x || Seq.isEmpty y -> false
        | _,_ -> true
    
    if shouldCompute = true then
        let meanX = Seq.mean x
        let meanY = Seq.mean y
        let numerator = Seq.map2 (fun x y -> (x - meanX) * (y - meanY)) x y |> Seq.sum
        let denominator = Seq.map (fun x -> (x - meanX) ** 2.0) x |> Seq.sum
        Ok(numerator / denominator)
    else Error("Slope x or y cannot be null and must be the same size")
            
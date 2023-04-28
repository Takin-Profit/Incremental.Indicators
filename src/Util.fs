module Incremental.Indicators.Util

open FSharp.Data.Adaptive

// gets a value from an AList without having to use AList.tryGet or AList.tryAt
// Which are not recommended
let internal getVal offset list defaultVal =
    aval {
        let! item = AList.sub offset 1 list |> AList.tryFirst
        return Option.defaultValue defaultVal item
    }

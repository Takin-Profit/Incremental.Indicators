module Incremental.Indicators.Util

open FSharp.Data.Adaptive
open System

/// unwraps an aval into an option type
let get value =
    let mutable t = None
    value |> AVal.map (fun v -> t <- v) |> ignore
    t

/// gets a value from an AList without having to use AList.tryGet or AList.tryAt
/// Which are not recommended
let getVal offset defaultVal list =
    aval {
        let! item = AList.sub offset 1 list |> AList.tryFirst
        return Option.defaultValue defaultVal item
    }

/// SEEK & FIND in SERIES
let inline find<'TSeries when 'TSeries: (member Date: DateTime)>
    (series: 'TSeries alist)
    (lookupDate: DateTime)
    (defaultVal: 'TSeries)
    =
    series |> AList.filter (fun x -> x.Date = lookupDate) |> getVal 0 defaultVal

/// REMOVE SPECIFIC PERIODS
let removeWarmupPeriods<'T> (series: 'T alist) (removePeriods: int) =
    if removePeriods < 0 then
        Error("the Remove Periods value must be greater than or equal to 0.")

    else
        Ok(series |> AList.skip removePeriods)

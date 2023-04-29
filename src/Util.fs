module Incremental.Indicators.Util

open FSharp.Data.Adaptive
open System

// gets a value from an AList without having to use AList.tryGet or AList.tryAt
// Which are not recommended
let internal getVal offset defaultVal list =
    aval {
        let! item = AList.sub offset 1 list |> AList.tryFirst
        return Option.defaultValue defaultVal item
    }

// slice an alist
let internal slice (startIndex: int) (endIndex: int) (xs: 'a alist) =
    xs |> AList.take (endIndex - startIndex + 1) |> AList.skip startIndex

// SEEK & FIND in SERIES
let inline find<'TSeries when 'TSeries: (member Date: DateTime)>
    (series: 'TSeries alist)
    (lookupDate: DateTime)
    (defaultVal: 'TSeries)
    =
    series |> AList.filter (fun x -> x.Date = lookupDate) |> getVal 0 defaultVal

// REMOVE SPECIFIC PERIODS
let removeWarmupPeriods<'T> (series: 'T clist) (removePeriods: int) =
    if removePeriods < 0 then
        Error("the Remove Periods value must be greater than or equal to 0.")

    else
        Ok(series |> AList.skip removePeriods)

module Incremental.Indicators.Series

open System
open System.Collections.ObjectModel
open FSharp.Data.Adaptive


let slice (startIndex: int) (endIndex: int) (xs: 'a alist) =
    xs |> AList.take (endIndex - startIndex + 1) |> AList.skip startIndex

// Generic Transforms
let toCollection<'T> (source: seq<'T>) =
    if isNull source then
        Error("null source")
    else
        let collection = Collection<'T>()

        for item in source do
            collection.Add(item)

        Ok(collection)

let inline toSortedCollection<'TSeries when 'TSeries: (member Date: DateTime)> (series: seq<'TSeries>) =
    series |> Seq.sortBy (fun s -> s.Date) |> toCollection

let inline toSortedList<'TSeries when 'TSeries: (member Date: DateTime)> (series: seq<'TSeries>) =
    series |> Seq.sortBy (fun s -> s.Date) |> Seq.toList

// SEEK & FIND in SERIES// SEEK & FIND in SERIES
let inline find<'TSeries when 'TSeries: (member Date: DateTime)>
    (series: seq<'TSeries>)
    (lookupDate: DateTime)
    : 'TSeries option =
    series |> Seq.tryFind (fun x -> x.Date = lookupDate)


// REMOVE AND PRUNING of SERIES
// REMOVE SPECIFIC PERIODS
let removeWarmupPeriods<'T> (series: 'T cset) (removePeriods: int) =
    if removePeriods < 0 then
        Error("the Remove Periods value must be greater than or equal to 0.")

    else
        Ok(series |> ASet.toAList |> AList.skip removePeriods |> AList.toASet)

// TODO: make this adaptive
// REMOVE PERIODS
let remove<'T> (series: 'T cset) (removePeriods: int) =
    let seriesList = series |> Seq.toList

    if seriesList.Length <= removePeriods then
        List.empty
    else
        if removePeriods > 0 then
            for i in 1..removePeriods do
                List.removeAt 0 seriesList |> ignore

        seriesList |> List.ofSeq

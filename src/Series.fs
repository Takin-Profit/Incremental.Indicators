module Incremental.Indicators.Series

open System
open System.Collections.ObjectModel

type ISeries =
    abstract Date: DateTime

// Generic Transforms
let toCollection<'T> (source: seq<'T>) =
    if isNull source then
        Error("null source")
    else
        let collection = Collection<'T>()

        for item in source do
            collection.Add(item)

        Ok(collection)

let toSortedCollection<'TSeries when 'TSeries :> ISeries> (series: seq<'TSeries>) =
    series |> Seq.sortBy (fun s -> s.Date) |> toCollection

let toSortedList<'TSeries when 'TSeries :> ISeries> (series: seq<'TSeries>) =
    series |> Seq.sortBy (fun s -> s.Date) |> Seq.toList

// SEEK & FIND in SERIES// SEEK & FIND in SERIES
let Find<'TSeries when 'TSeries :> ISeries> (series: seq<'TSeries>) (lookupDate: DateTime) : 'TSeries option =
    series |> Seq.tryFind (fun x -> x.Date = lookupDate)


// REMOVE AND PRUNING of SERIES
// REMOVE SPECIFIC PERIODS
let removeWarmupPeriods<'T> (series: seq<'T>) (removePeriods: int) =
    if removePeriods < 0 then
        Error("the Remove Periods value must be greater than or equal to 0.")

    else
        Ok(series |> Seq.skip removePeriods |> Seq.toList)

// REMOVE PERIODS
let remove<'T> (series: seq<'T>) (removePeriods: int) =
    let seriesList = series |> Seq.toList

    if seriesList.Length <= removePeriods then
        List.empty
    else
        if removePeriods > 0 then
            for i in 1..removePeriods do
                List.removeAt 0 seriesList |> ignore

        seriesList |> List.ofSeq

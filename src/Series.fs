namespace Incremental.Indicators

open FSharp.Data.Adaptive
open System

type Series = {| Date: DateTime; Value: double |} alist

type SeriesDec = {| Date: DateTime; Value: decimal |} alist


module Series =

    let values (series: Series) =
        series |> AList.sortBy (fun t -> t.Date) |> AList.map (fun t -> t.Value)


    let emptyVal =
        {| Date = DateTime.MaxValue
           Value = double 0.0 |}

    let isEmptyVal (result: {| Date: DateTime; Value: double |}) = result.Date = DateTime.MaxValue


    let findByDate date (series: Series) =
        let filtered = series |> AList.filter (fun t -> t.Date = date)
        let mutable value = emptyVal
        AList.tryFirst filtered |> Util.get |> Option.defaultValue emptyVal

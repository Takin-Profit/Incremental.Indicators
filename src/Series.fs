namespace Incremental.Indicators

open FSharp.Data.Adaptive
open System

type Series = {| Date: DateTime; Value: double |} alist

type SeriesDec = {| Date: DateTime; Value: decimal |} alist


module Series =

    let values (series: Series) =
        series |> AList.sortBy (fun t -> t.Date) |> AList.map (fun t -> t.Value)

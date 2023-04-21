module Incremental.Indicators.Results

open System
open Incremental.Indicators.Series

type IReusableResult =
    interface
        inherit ISeries
        abstract member Value: double option with get
    end

[<Serializable>]
type ResultBase =
    { Date: DateTime }

    interface ISeries with
        member this.Date = this.Date


type SyncType =
    | Prepend
    | AppendOnly
    | RemoveOnly
    | FullMatch

let syncIndex syncMe toMatch syncType =

    let mutable syncMeList = syncMe |> Seq.sortBy (fun x -> x.Date) |> List.ofSeq
    let toMatchList = toMatch |> Seq.sortBy (fun x -> x.Date) |> List.ofSeq

    if syncMeList.IsEmpty || toMatchList.IsEmpty then
        []
    else
        let prepend, append, remove =
            match syncType with
            | SyncType.Prepend -> true, false, false
            | SyncType.AppendOnly -> true, true, false
            | SyncType.RemoveOnly -> false, false, true
            | SyncType.FullMatch -> true, true, true

        let type_ = syncMeList[0].GetType()

        // add plugs for missing values
        if prepend || append then
            let toAppend =
                [ for m in toMatchList do
                      let r = syncMeList |> List.tryFind (fun x -> x.Date = m.Date)

                      match r with
                      | None when prepend ->
                          let n = unbox (Activator.CreateInstance(type_, m.Date))
                          Some n
                      | _ when not append -> None
                      | _ -> None ]
                |> List.choose id

            syncMeList <- List.append syncMeList toAppend

        // remove unmatched results
        if remove then
            let toRemove =
                [ for r in syncMeList do
                      let m = toMatchList |> List.tryFind (fun x -> x.Date = r.Date)

                      match m with
                      | None -> Some r
                      | _ -> None ]
                |> List.choose id

            syncMeList <- List.fold (fun acc x -> if toRemove |> List.contains x then acc else x :: acc) [] syncMeList

        syncMeList |> List.sortBy (fun x -> x.Date)

let inline checkNull v =
    match box v with
    | null -> true
    | :? double as d when Double.IsNaN(d) -> true
    | _ -> false

let nullToNaN (value: double option) =
    match value with
    | Some x -> x
    | None -> Double.NaN

let condense<'TResult when 'TResult :> IReusableResult> (results: seq<'TResult>) =
    let resultsList = results |> List.ofSeq

    let filteredResultsList =
        resultsList |> List.filter (fun x -> not (checkNull x.Value))

    filteredResultsList |> List.sortBy (fun x -> x.Date) |> Seq.ofList

let toTupleChainable (reusable: seq<IReusableResult>) : seq<DateTime * double> =
    reusable
    |> Seq.filter (fun x -> not (checkNull x.Value))
    |> Seq.map (fun x -> x.Date, nullToNaN x.Value)


let toTupleNaN (reusable: seq<IReusableResult>) : seq<DateTime * double> =
    reusable |> Seq.map (fun x -> x.Date, nullToNaN x.Value)

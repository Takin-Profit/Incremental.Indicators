module Incremental.Indicators.Results

open FSharp.Data.Adaptive
open System
open Incremental.Indicators.Quotes
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

let private get value =
    let mutable t = None
    value |> AVal.map (fun v -> t <- v) |> ignore
    t

let getType value =
    let mutable t = Type.GetType("None")
    value |> AVal.map (fun v -> t <- v) |> ignore
    t


let mapper (m: ResultBase) (syncMeList: Quote alist) syncType type_ =
    let prepend, append, remove =
        match syncType with
        | SyncType.Prepend -> true, false, false
        | SyncType.AppendOnly -> true, true, false
        | SyncType.RemoveOnly -> false, false, true
        | SyncType.FullMatch -> true, true, true

    let r = syncMeList |> AList.filter (fun x -> x.Date = m.Date) |> AList.tryFirst

    match get r with
    | None when prepend ->
        let n = unbox (Activator.CreateInstance(type_, m.Date))
        Some n
    | _ when not append -> None
    | _ -> None
(* TODO: Come back to this function later.
let syncIndex (syncMe: Quote cset) (toMatch: ResultBase cset) syncType =
    if syncMe.IsEmpty || toMatch.IsEmpty then
        []
    else
        let syncMeList = syncMe |> ASet.sortBy (fun x -> x.Date)
        let toMatchList = toMatch |> ASet.sortBy (fun x -> x.Date)

        let prepend, append, remove =
            match syncType with
            | SyncType.Prepend -> true, false, false
            | SyncType.AppendOnly -> true, true, false
            | SyncType.RemoveOnly -> false, false, true
            | SyncType.FullMatch -> true, true, true

        let type_ = AList.tryFirst syncMeList |> get |> (fun t -> t.Value.GetType())

        // add plugs for missing values
        if prepend || append then
            let toAppend =
                toMatchList
                |> AList.map (fun m -> mapper m syncMeList syncType type_)
                |> List.choose id

            AList.append syncMeList toAppend

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
*)

let inline checkNull v =
    match box v with
    | null -> true
    | :? double as d when Double.IsNaN(d) -> true
    | _ -> false

let nullToNaN (value: double option) =
    match value with
    | Some x -> x
    | None -> Double.NaN

let condense<'TResult when 'TResult :> IReusableResult> (results: cset<'TResult>) =

    let filteredResultsList = results |> ASet.filter (fun x -> not (checkNull x.Value))

    filteredResultsList |> ASet.sortBy (fun x -> x.Date) |> AList.toASet

let toTupleChainable (reusable: cset<IReusableResult>) =
    reusable
    |> ASet.filter (fun x -> not (checkNull x.Value))
    |> ASet.map (fun x -> x.Date, nullToNaN x.Value)


let toTupleNaN (reusable: IReusableResult cset) =
    reusable |> ASet.map (fun x -> x.Date, nullToNaN x.Value)

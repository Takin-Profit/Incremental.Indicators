module Incremental.Indicators.Results

open System.Collections.Generic
open FSharp.Data.Adaptive
open System
open FSharp.Data.Adaptive.ComputationExpressions
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

[<RequireQualifiedAccess>]
type SyncType =
    | Prepend
    | AppendOnly
    | RemoveOnly
    | FullMatch

let private getSyncType syncType =
    match syncType with
    | SyncType.Prepend -> true, false, false
    | SyncType.AppendOnly -> true, true, false
    | SyncType.RemoveOnly -> false, false, true
    | SyncType.FullMatch -> true, true, true

let private get value =
    let mutable t = None
    value |> AVal.map (fun v -> t <- v) |> ignore
    t

let syncIndex<'TResultA, 'TResultB
    when 'TResultA :> ISeries and 'TResultB :> ISeries and 'TResultA: equality and 'TResultB: equality>
    (syncMe: 'TResultA cset)
    (toMatch: 'TResultB cset)
    syncType
    =
    let syncMeList = syncMe |> ASet.sortBy (fun x -> x.Date)
    let toMatchList = toMatch |> ASet.sortBy (fun x -> x.Date)

    let prepend, append, remove = getSyncType syncType

    let type_ = AList.tryFirst syncMeList |> get |> (fun t -> t.Value.GetType())

    if syncMe.IsEmpty || toMatch.IsEmpty then
        ASet.empty
    // add plugs for missing values
    elif prepend || append then
        let toAppend =
            alist {
                for m in toMatchList do
                    let! r = syncMeList |> AList.filter (fun x -> x.Date = m.Date) |> AList.tryFirst

                    match r with
                    | None when prepend ->
                        let n: 'TResultA = unbox (Activator.CreateInstance(type_, m.Date))
                        yield n
                    | _ when not append -> ()
                    | _ -> ()
            }

        AList.append syncMeList toAppend |> AList.toASet
    // remove unmatched results
    elif remove then
        let toRemove =
            alist {
                for r in syncMeList do
                    let! m = toMatchList |> AList.filter (fun x -> x.Date = r.Date) |> AList.tryFirst

                    match m with
                    | None -> yield r
                    | _ -> ()
            }

        let ls =
            aset {
                for s in syncMeList do
                    for r in toRemove do
                        if s <> r then
                            yield s
            }

        ls
    else
        ASet.empty

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

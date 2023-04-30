module Incremental.Indicators.Results

open FSharp.Data.Adaptive
open System

[<RequireQualifiedAccess>]
type SyncType =
    | Prepend
    | AppendOnly
    | RemoveOnly
    | FullMatch



let inline syncIndex<'TResultA, 'TResultB when 'TResultA: (member Date: DateTime) and 'TResultA: equality>
    (syncMe: 'TResultA alist)
    (toMatch: 'TResultA alist)
    syncType
    =
    let get value =
        let mutable t = None
        value |> AVal.map (fun v -> t <- v) |> ignore
        t

    let syncMeList = syncMe |> AList.sortBy (fun x -> x.Date)
    let toMatchList = toMatch |> AList.sortBy (fun x -> x.Date)

    let prepend, append, remove =
        match syncType with
        | SyncType.Prepend -> true, false, false
        | SyncType.AppendOnly -> true, true, false
        | SyncType.RemoveOnly -> false, false, true
        | SyncType.FullMatch -> true, true, true

    // TODO: this code is fairly brittle and should be removed or replaced at some point
    let type_ = AList.tryFirst syncMeList |> get |> (fun t -> t.Value.GetType())

    let isEmpty =
        adaptive {
            let! l = AList.isEmpty syncMe
            let! r = AList.isEmpty toMatch
            return l || r
        }

    if AVal.force isEmpty then
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

// convert any Result type in to (DateTime * double) alist
let inline toTuples<'TResult when 'TResult: (member Date: DateTime) and 'TResult: (member Value: double)>
    (reusable: 'TResult clist)
    =
    reusable |> AList.map (fun x -> (x.Date, x.Value)) |> AList.sortBy fst

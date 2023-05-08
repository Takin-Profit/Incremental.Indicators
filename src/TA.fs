namespace Incremental.Indicators

open System
open FSharp.Data.Adaptive

[<AbstractClass; Sealed>]
type TA private () =
    static member change(source: Series, ?length: int) =
        // Use provided length or default to 1 if not provided
        let length = defaultArg length 1

        let seriesLen = (AList.count source |> Util.getAValInt) - 1

        if seriesLen < length then
            raise (ArgumentException($"Length argument {length} is out of range, only {seriesLen} candles in series"))

        if length < 1 then
            raise (ArgumentException("Length should be at least 1"))

        let change =
            aval {
                let! count = AList.count source
                let lastIndex = count - 1
                let origin = lastIndex - length
                let! current = Util.getVal lastIndex Series.emptyVal source
                let! previous = Util.getVal origin Series.emptyVal source
                return current.Value - previous.Value
            }

        change

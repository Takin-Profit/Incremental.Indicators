namespace Incremental.Indicators

open System
open System.Runtime.CompilerServices
open FSharp.Data.Adaptive

[<assembly: InternalsVisibleTo("Incremental.Indicators.Tests")>]
do ()


[<AbstractClass; Sealed>]
type TA private () =
    static member change(source: Series, ?length: int) =
        // Use provided length or default to 1 if not provided
        let length = defaultArg length 1

        if length < 1 then
            raise (ArgumentException("Length should be at least 1"))

        let change =
            aval {
                let! count = AList.count source
                let lastIndex = count - 1
                let origin = lastIndex - length

                if lastIndex < length then
                    return double 0.0
                else
                    let! current = Util.getVal lastIndex Series.emptyVal source
                    let! previous = Util.getVal origin Series.emptyVal source
                    return current.Value - previous.Value
            }

        change

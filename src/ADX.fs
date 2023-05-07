module Incremental.Indicators.ADX

open System

type AdxResult =
    { Date: DateTime
      Pdi: double
      Mdi: double
      Adx: double
      Adxr: double }

    member x.Value = x.Adx

let internal calcAdx (lookbackPeriods: int) : (QuoteD -> AdxResult) =
    let mutable prevQuote = QuoteD.Empty
    let mutable trueRanges = List.empty
    let mutable dmPlus = List.empty
    let mutable dmMinus = List.empty
    let mutable trSum = 0.0
    let mutable dmPlusSum = 0.0
    let mutable dmMinusSum = 0.0
    let mutable prevAdx = 0.0
    let mutable prevAdxr = 0.0

    let calculateTrueRange (prevQuote: QuoteD) (quote: QuoteD) =
        max (quote.High - quote.Low) (max (abs (quote.High - prevQuote.Close)) (abs (quote.Low - prevQuote.Close)))

    let calculateAdx (pdi: double) (mdi: double) : double =
        let dx = abs (pdi - mdi) / (pdi + mdi) * 100.0
        let adx = (prevAdx * (double (lookbackPeriods - 1)) + dx) / double lookbackPeriods
        prevAdx <- adx
        adx

    let calculateAdxr (adx: double) : double =
        let adxr = (prevAdxr + adx) / 2.0
        prevAdxr <- adx
        adxr

    fun (quote: QuoteD) ->
        if QuoteD.IsEmpty prevQuote then
            prevQuote <- quote

            { Date = quote.Date
              Pdi = 0.0
              Mdi = 0.0
              Adx = 0.0
              Adxr = 0.0 }
        else
            let tr = calculateTrueRange prevQuote quote
            let upMove = quote.High - prevQuote.High
            let downMove = prevQuote.Low - quote.Low

            let dmPlusVal = if upMove > downMove && upMove > 0.0 then upMove else 0.0

            let dmMinusVal =
                if downMove > upMove && downMove > 0.0 then
                    downMove
                else
                    0.0

            dmPlus <- dmPlusVal :: dmPlus
            dmMinus <- dmMinusVal :: dmMinus

            if List.length trueRanges >= lookbackPeriods then
                trSum <- trSum - List.head trueRanges + tr
                dmPlusSum <- dmPlusSum - List.head dmPlus + dmPlusVal
                dmMinusSum <- dmMinusSum - List.head dmMinus + dmMinusVal
                trueRanges <- List.tail trueRanges
                dmPlus <- List.tail dmPlus
                dmMinus <- List.tail dmMinus
            else
                trSum <- trSum + tr
                dmPlusSum <- dmPlusSum + dmPlusVal
                dmMinusSum <- dmMinusSum + dmMinusVal

            trueRanges <- tr :: trueRanges

            let pdi = dmPlusSum / trSum * 100.0
            let mdi = dmMinusSum / trSum * 100.0
            let adx = calculateAdx pdi mdi
            let adxr = calculateAdxr adx

            prevQuote <- quote

            { Date = quote.Date
              Pdi = pdi
              Mdi = mdi
              Adx = adx
              Adxr = adxr }

let internal adxFunc = calcAdx 14

let internal adx quotes =
    Seq.map adxFunc quotes |> Seq.sortBy (fun t -> t.Date)

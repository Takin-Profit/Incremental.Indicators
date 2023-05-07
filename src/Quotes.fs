namespace Incremental.Indicators

open System
open FSharp.Data.Adaptive
open Types
open Calc

type Quote =
    { Date: DateTime
      Open: decimal
      High: decimal
      Low: decimal
      Close: decimal
      Volume: decimal }

    static member Empty =
        { Date = DateTime.MaxValue
          Open = 0m
          Close = 0m
          High = 0m
          Low = 0m
          Volume = 0m }

    static member IsEmpty q = q.Date = DateTime.MaxValue


type QuoteD =
    { Date: DateTime
      Open: double
      High: double
      Low: double
      Close: double
      Volume: double }

    static member Empty =
        { Date = DateTime.MaxValue
          Open = 0.0
          Close = 0.0
          High = 0.0
          Low = 0.0
          Volume = 0.0 }

    static member IsEmpty q = q.Date = DateTime.MaxValue


module internal Quotes =
    // check if a quote with the sam date already exists
    let isValid (quote: Quote) (quotes: Quote alist) =
        let found =
            quotes
            |> AList.filter (fun q -> q.Date = quote.Date)
            |> AList.count
            |> AVal.force

        found = 0

    //validate there are no quotes with duplicate dates
    let validate (quotes: Quote seq) =
        // we cannot rely on date consistency when looking back, so we force sort
        let sortedQuotes = quotes |> Seq.sortBy (fun x -> x.Date)

        // Check for duplicates
        let mutable lastDate = DateTime.MinValue

        let duplicates =
            sortedQuotes
            |> Seq.tryFind (fun q ->
                let foundDuplicate = lastDate = q.Date
                lastDate <- q.Date
                foundDuplicate)

        match duplicates with
        | Some duplicateQuote -> Error $"Duplicate date found on %A{duplicateQuote.Date}."
        | None -> Ok(sortedQuotes)

    // convert a sequence of quotes to a clist
    let createList (quotes: seq<Quote>) = validate quotes |> Result.map clist

    /// Convert TQuote element to basic data record
    let toSeriesValue (candlePart: CandlePart) (q: QuoteD) =
        match candlePart with
        | CandlePart.Open -> {| Date = q.Date; Value = q.Open |}
        | CandlePart.High -> {| Date = q.Date; Value = q.High |}
        | CandlePart.Low -> {| Date = q.Date; Value = q.Low |}
        | CandlePart.Close -> {| Date = q.Date; Value = q.Close |}
        | CandlePart.Volume -> {| Date = q.Date; Value = q.Volume |}
        | CandlePart.HL2 ->
            {| Date = q.Date
               Value = (q.High + q.Low) / 2.0 |}
        | CandlePart.HLC3 ->
            {| Date = q.Date
               Value = (q.High + q.Low + q.Close) / 3.0 |}
        | CandlePart.OC2 ->
            {| Date = q.Date
               Value = (q.Open + q.Close) / 2.0 |}
        | CandlePart.OHL3 ->
            {| Date = q.Date
               Value = (q.Open + q.High + q.Low) / 3.0 |}
        | CandlePart.OHLC4 ->
            {| Date = q.Date
               Value = (q.Open + q.High + q.Low + q.Close) / 4.0 |}

    let toSeriesValueDec (candlePart: CandlePart) (q: Quote) =
        match candlePart with
        | CandlePart.Open -> {| Date = q.Date; Value = q.Open |}
        | CandlePart.High -> {| Date = q.Date; Value = q.High |}
        | CandlePart.Low -> {| Date = q.Date; Value = q.Low |}
        | CandlePart.Close -> {| Date = q.Date; Value = q.Close |}
        | CandlePart.Volume -> {| Date = q.Date; Value = q.Volume |}
        | CandlePart.HL2 ->
            {| Date = q.Date
               Value = (q.High + q.Low) / 2.0m |}
        | CandlePart.HLC3 ->
            {| Date = q.Date
               Value = (q.High + q.Low + q.Close) / 3.0m |}
        | CandlePart.OC2 ->
            {| Date = q.Date
               Value = (q.Open + q.Close) / 2.0m |}
        | CandlePart.OHL3 ->
            {| Date = q.Date
               Value = (q.Open + q.High + q.Low) / 3.0m |}
        | CandlePart.OHLC4 ->
            {| Date = q.Date
               Value = (q.Open + q.High + q.Low + q.Close) / 4.0m |}

    // convert Quote element to basic tuple of double precision values
    let toTuple (candlePart: CandlePart) (q: Quote) =
        match candlePart with
        | CandlePart.Open -> (q.Date, double q.Open)
        | CandlePart.High -> (q.Date, double q.High)
        | CandlePart.Low -> (q.Date, double q.Low)
        | CandlePart.Close -> (q.Date, double q.Close)
        | CandlePart.Volume -> (q.Date, double q.Volume)
        | CandlePart.HL2 -> (q.Date, double (q.High + q.Low) / 2.0)
        | CandlePart.HLC3 -> (q.Date, double (q.High + q.Low + q.Close) / 3.0)
        | CandlePart.OC2 -> (q.Date, double (q.Open + q.Close) / 2.0)
        | CandlePart.OHL3 -> (q.Date, double (q.Open + q.High + q.Low) / 3.0)
        | CandlePart.OHLC4 -> (q.Date, double (q.Open + q.High + q.Low + q.Close) / 4.0)


    let listToSeries candlePart quotes =
        quotes |> AList.map (toSeriesValue candlePart)

    let listToSeriesDec candlePart quotes =
        quotes |> AList.map (toSeriesValueDec candlePart)

    // convert Quote alist to (DateTime * double) alist
    // candlePart determines the part of price to be used
    let listToTuples (candlePart: CandlePart) (quotes: Quote alist) =
        quotes |> AList.map (toTuple candlePart) |> AList.sortBy fst

    // convert to quotes in double precision
    // QuoteD alist sorted by date
    let toQuoteDList (quotes: Quote alist) =
        quotes
        |> AList.map (fun x ->
            { Date = x.Date
              Open = double x.Open
              High = double x.High
              Low = double x.Low
              Close = double x.Close
              Volume = double x.Volume })
        |> AList.sortBy (fun x -> x.Date)


    let quoteDtoTuple (candlePart: CandlePart) (q: QuoteD) =
        match candlePart with
        | CandlePart.Open -> (q.Date, q.Open)
        | CandlePart.High -> (q.Date, q.High)
        | CandlePart.Low -> (q.Date, q.Low)
        | CandlePart.Close -> (q.Date, q.Close)
        | CandlePart.Volume -> (q.Date, q.Volume)
        | CandlePart.HL2 -> (q.Date, (q.High + q.Low) / 2.0)
        | CandlePart.HLC3 -> (q.Date, (q.High + q.Low + q.Close) / 3.0)
        | CandlePart.OC2 -> (q.Date, (q.Open + q.Close) / 2.0)
        | CandlePart.OHL3 -> (q.Date, (q.Open + q.High + q.Low) / 3.0)
        | CandlePart.OHLC4 -> (q.Date, (q.Open + q.High + q.Low + q.Close) / 4.0)

    // convert quoteD list to tuples
    let quoteDListToToTuples (candlePart: CandlePart) (qdList: QuoteD alist) =
        qdList |> AList.sortBy (fun x -> x.Date) |> AList.map (quoteDtoTuple candlePart)

    // aggregation (quantization) using TimeSpan>
    let aggregateByTimeSpan (timeSpan: TimeSpan) (quotes: Quote alist) =
        let count = AList.count quotes |> AVal.force
        // handle no quotes scenario
        if count < 1 then
            Ok AList.empty
        else if timeSpan <= TimeSpan.Zero then
            Error
                $"Quotes Aggregation must use a usable new size value (see documentation for options). Value: %A{timeSpan}"
        else
            // return aggregation
            quotes
            |> AList.sortBy (fun x -> x.Date)
            |> AList.groupBy (fun x -> roundDown x.Date timeSpan)
            |> AMap.map (fun x v ->
                { Quote.Date = x
                  Open = IndexList.first v |> fun t -> t.Open
                  High = IndexList.toSeq v |> Seq.maxBy (fun t -> t.High) |> (fun t -> t.High)
                  Low = IndexList.toSeq v |> Seq.minBy (fun t -> t.Low) |> (fun t -> t.Low)
                  Close = IndexList.last v |> fun t -> t.Close
                  Volume = IndexList.sumBy (fun (t: Quote) -> t.Volume) v })

            |> AMap.toASetValues
            |> ASet.toAList
            |> AList.sortBy (fun t -> t.Date)
            |> Ok

    let aggregateByTimeFrame (timeFrame: TimeFrame) (quotes: Quote alist) =
        if timeFrame <> TimeFrame.Month then
            // parameter conversion
            let newTimeSpan = toTimeSpan timeFrame

            // convert
            aggregateByTimeSpan newTimeSpan quotes

        else // month
            quotes
            |> AList.sortBy (fun x -> x.Date)
            |> AList.groupBy (fun x -> DateTime(x.Date.Year, x.Date.Month, 1))
            |> AMap.map (fun x v ->
                { Quote.Date = x
                  Open = IndexList.first v |> fun t -> t.Open
                  High = IndexList.toSeq v |> Seq.maxBy (fun t -> t.High) |> (fun t -> t.High)
                  Low = IndexList.toSeq v |> Seq.minBy (fun t -> t.Low) |> (fun t -> t.Low)
                  Close = IndexList.last v |> fun t -> t.Close
                  Volume = IndexList.sumBy (fun (t: Quote) -> t.Volume) v })
            |> AMap.toASetValues
            |> ASet.toAList
            |> AList.sortBy (fun x -> x.Date)
            |> Ok


type Quotes =
    private
        { quotes: Quote clist
          doubleQuotes: QuoteD alist
          sortedQuotes: Quote alist }

    member internal x.Quotes = x.quotes

    /// convert quotes to double precision quotes (QuoteD)
    member internal x.DoublePrecis = x.doubleQuotes

    // convert quotes to (DateTime * double) alist
    // candlePart determines the part of price to be used
    member private x.toTuples candlePart = Quotes.listToTuples candlePart x.quotes

    member private x.prices candlePart =
        Quotes.quoteDListToToTuples candlePart x.doubleQuotes |> AList.map snd

    /// convert quotes to (DateTime * double) alist with open prices
    member x.OpenTuples = x.toTuples CandlePart.Open
    /// convert quotes to double alist with open prices
    member x.Open = x.prices CandlePart.Open
    /// convert quotes to (DateTime * double) alist with close prices
    member x.CloseTuples = x.toTuples CandlePart.Close
    /// convert quotes to double alist with close prices
    member x.Close = x.prices CandlePart.Close
    /// convert quotes to (DateTime * double) alist with high prices
    member x.HighTuples = x.toTuples CandlePart.High
    /// convert quotes to double alist with high prices
    member x.High = x.prices CandlePart.High
    /// convert quotes to (DateTime * double) alist with low prices
    member x.LowTuples = x.toTuples CandlePart.Low
    /// convert quotes to double alist with open prices
    member x.Low = x.prices CandlePart.Low
    /// convert quotes to (DateTime * double) alist with volume
    member x.VolumeTuples = x.toTuples CandlePart.Volume
    /// convert quotes to double alist with volume
    member x.Volume = x.prices CandlePart.Volume
    /// convert quotes to (DateTime * double) alist with HL2 prices
    member x.Hl2Tuples = x.toTuples CandlePart.HL2
    /// convert quotes to double alist with HL2 prices
    member x.HL2 = x.prices CandlePart.HL2
    /// convert quotes to (DateTime * double) alist with HLC3 prices
    member x.HLC3Tuples = x.toTuples CandlePart.HLC3
    /// convert quotes to double alist with HLC3 prices
    member x.HLC3 = x.prices CandlePart.HLC3
    /// convert quotes to (DateTime * double) alist with OC2 prices
    member x.OC2Tuples = x.toTuples CandlePart.OC2
    /// convert quotes to double alist with OC2 prices
    member x.OC2 = x.prices CandlePart.OC2
    /// convert quotes to (DateTime * double) alist with OHL3 prices
    member x.OHL3Tuples = x.toTuples CandlePart.OHL3
    /// convert quotes to double alist with OHL3 prices
    member x.OHL3 = x.prices CandlePart.OHL3
    /// convert quotes to (DateTime * double) alist with OHLC4 prices
    member x.OHLC4Tuples = x.toTuples CandlePart.OHLC4
    /// convert quotes to double alist with OHLC4 prices
    member x.OHLC4 = x.prices CandlePart.OHLC4

    member x.Series: Series =
        x.doubleQuotes |> AList.map (fun t -> {| Date = t.Date; Value = t.Close |})

    member x.SeriesDec: SeriesDec =
        x.sortedQuotes |> AList.map (fun t -> {| Date = t.Date; Value = t.Close |})

    member x.seriesFrom candlePart : Series =
        x.doubleQuotes |> AList.map (Quotes.toSeriesValue candlePart)

    member x.seriesDecFrom candlePart : SeriesDec =
        x.sortedQuotes |> AList.map (Quotes.toSeriesValueDec candlePart)

    member x.toArray = AList.force x.quotes |> IndexList.toArray

    member x.toList = AList.force x.quotes |> IndexList.toList

    member x.toSeq = AList.force x.quotes |> IndexList.toSeq

    member x.toArrayDouble = AList.force x.DoublePrecis |> IndexList.toArray

    member x.toListDouble = AList.force x.DoublePrecis |> IndexList.toList

    member x.toSeqDouble = AList.force x.DoublePrecis |> IndexList.toSeq

    /// add a single quote to the list, will return an Error if a quote
    /// with the same Date is already in the list
    member x.add(quote: Quote) =
        if Quotes.isValid quote x.Quotes then
            transact (fun () -> x.quotes.Add quote) |> ignore
            Ok("Quote added successfully")
        else
            Error($"Quote with date '{quote.Date}' already exists")

    static member create quotes =
        match Quotes.createList quotes with
        | Error a -> Error a
        | Ok q ->
            Ok
                { quotes = q
                  doubleQuotes = Quotes.toQuoteDList q |> AList.sortBy (fun x -> x.Date)
                  sortedQuotes = q |> AList.sortBy (fun x -> x.Date) }

    static member empty =
        { quotes = clist []
          doubleQuotes = AList.empty
          sortedQuotes = AList.empty }

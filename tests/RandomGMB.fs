module Incremental.Indicators.Tests.RandomGMB

open System
open Incremental.Indicators

let generateRandomGbm (bars: int) (volatility: double) (drift: double) (seed: double) : Quote seq =
    let randomPrice (seed: double) (volatility: double) (drift: double) =
        let rnd = Random(int DateTime.UtcNow.Ticks)
        let u1 = 1.0 - rnd.NextDouble()
        let u2 = 1.0 - rnd.NextDouble()
        let z = Math.Sqrt(-2.0 * Math.Log(u1)) * Math.Sin(2.0 * Math.PI * u2)
        seed * Math.Exp(drift - (volatility * volatility * 0.5) + (volatility * z))

    let mutable seed = seed
    let volatility = volatility * 0.01
    let drift = drift * 0.01

    let quotes = ResizeArray<Quote>()

    for i in 0 .. (bars - 1) do
        let date = DateTime.Today.AddDays(double (bars - i))
        let openPrice = randomPrice seed (volatility * volatility) drift
        let close = randomPrice openPrice volatility drift

        let ocMax = Math.Max(openPrice, close)
        let high = randomPrice seed (volatility * 0.5) 0.0
        let high = if high < ocMax then (2.0 * ocMax) - high else high

        let ocMin = Math.Min(openPrice, close)
        let low = randomPrice seed (volatility * 0.5) 0.0
        let low = if low > ocMin then (2.0 * ocMin) - low else low

        let volume = randomPrice (seed * 10.0) (volatility * 2.0) 0.0

        let quote: Quote =
            { Date = date
              Open = decimal openPrice
              High = decimal high
              Low = decimal low
              Close = decimal close
              Volume = decimal volume }

        seed <- close
        quotes.Add(quote)

    quotes

module FsStats.Normal
open FsStats

open Special

type Distribution(mu: float, sigma: float) =
    member self.Mean = mu
    member self.Variance = sigma * sigma
    member self.StdDev = sigma

    /// Cumulative distribution function
    member self.Probability x = (1.0 + erf((x - mu) / (sigma * sqrt 2.0))) * 0.5

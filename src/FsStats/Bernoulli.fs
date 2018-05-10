module FsStats.Bernoulli

/// Bernoulli distribution
type Distribution(p: float) =
    let variance = p * (1.0 - p)
    let rnd = new System.Random()
    member self.Mean = p
    member self.Variance = variance
    member self.StdDev = sqrt variance

    member self.Sample = if rnd.NextDouble() < p then 1 else 0

    member self.Samples k = Array.init k (fun _ -> self.Sample)

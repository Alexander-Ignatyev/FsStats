namespace FsStats

/// Bernoulli distribution
type BernoulliDistribution(p: float) =
    let variance = p * (1.0 - p)
    let rnd = new System.Random()
    member self.Mean = p
    member self.Variance = variance
    member self.StdDev = sqrt variance

    member self.Sample = rnd.NextDouble() < p

    member self.Samples k = Array.init k (fun _ -> self.Sample)

namespace FsStats

/// Bernoulli distribution
type BernoulliDistribution(p: float) =
    inherit IntegerDistribution()
    let variance = p * (1.0 - p)
    let rnd = new System.Random()
    override self.Mean = p
    override self.Variance = variance
    override self.StdDev = sqrt variance

    override self.Probability k = 
        match k with 
        | 0 -> 1.0 - p
        | 1 -> p
        | _ -> 0.0

    override this.CumulativeProbability k = Array.sumBy this.Probability [|0 .. k|]

    override self.Sample = if rnd.NextDouble() < p then 1 else 0

namespace FsStats.OOD

open FsStats.NormalDistribution


type NormalDistribution(mu: float, sigma: float) =
    inherit ContinuousDistribution()
    let d = create mu sigma
    let rnd = new System.Random()
    override self.Mean = mu
    override self.Variance = sigma * sigma
    override self.StdDev = sigma

    /// Cumulative distribution function
    override self.Probability x = cdf d x

    override self.Sample = sample d rnd

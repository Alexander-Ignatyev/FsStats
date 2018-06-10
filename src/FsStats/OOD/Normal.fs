namespace FsStats.OOD

open FsStats.NormalDistribution


type NormalDistribution(mu: float, sigma: float) =
    inherit ContinuousDistribution()
    let d = create mu sigma
    let rnd = new System.Random()
    override __.Mean = mean d
    override __.Variance = variance d
    override __.StdDev = stddev d

    /// Cumulative distribution function
    override __.Probability x = cdf d x

    override __.Random = random d rnd

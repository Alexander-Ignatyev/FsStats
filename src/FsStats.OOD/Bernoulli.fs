namespace FsStats.OOD

open FsStats.BernoulliDistribution

/// Bernoulli distribution
type BernoulliDistribution(p: float) =
    inherit DiscreteDistribution()
    let d = create p
    let rnd = new System.Random()
    override __.Mean = mean d
    override __.Variance = variance d
    override __.StdDev = stddev d

    override __.Probability k = pmf d k

    override __.CumulativeProbability k = cdf d k

    override __.Random = random d rnd

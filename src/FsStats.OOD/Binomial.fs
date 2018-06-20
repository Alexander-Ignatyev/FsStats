namespace FsStats.OOD

open FsStats.BinomialDistribution


/// Binomial Distribution
/// n - number of trials
/// p - probability of success
type BinomialDistribution(n: int, p: float) =
    inherit DiscreteDistribution()
    let d = create n p
    let rnd = new System.Random()

    override __.Mean = mean d
    override __.Variance = variance d
    override __.StdDev = stddev d

    override __.Probability k = pmf d k

    override __.CumulativeProbability k = cdf d k

    override __.Random = random d rnd

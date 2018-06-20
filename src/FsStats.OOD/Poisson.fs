namespace FsStats.OOD

open FsStats.PoissonDistribution

/// Poisson distribution
/// where mu is observed mean rate 
/// of independent events 
/// occurring within a fixed interval
type PoissonDistribution(mu: float) = 
    inherit DiscreteDistribution()
    let pd = create mu

    let rnd = new System.Random()
    override __.Mean = mean pd
    override __.Variance = variance pd
    override __.StdDev = stddev pd
    override __.Probability k = pmf pd k
    override __.CumulativeProbability k = cdf pd k
    override __.Random = random pd rnd

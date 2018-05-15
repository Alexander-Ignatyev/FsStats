namespace FsStats

/// Discrete Probability Distribution base class
[<AbstractClass>]
type DiscreteDistribution() =
    inherit Distribution<int>()
    override this.CumulativeProbability (m, n) = this.CumulativeProbability n - this.CumulativeProbability (m - 1)

namespace FsStats

/// Continuous Probability Distribution base class
[<AbstractClass>]
type ContinuousDistribution() =
    inherit Distribution<float>()
    // There is no well-defined Probability Mass Function (PMF) for Real distrinution
    // so Cumulative Distribution Function is the same as PMF.
    override this.CumulativeProbability x = this.Probability x
    override this.CumulativeProbability (m, n) = this.CumulativeProbability n - this.CumulativeProbability m

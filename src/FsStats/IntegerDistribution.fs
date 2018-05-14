module FsStats.IntegerDistribution

open FsStats.Distribution

[<AbstractClass>]
type IntegerDistribution() =
    inherit Distribution<int>()
    override this.CumulativeProbability (m, n) = this.CumulativeProbability n - this.CumulativeProbability (m - 1)

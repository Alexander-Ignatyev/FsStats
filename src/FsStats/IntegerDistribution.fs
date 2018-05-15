namespace FsStats

[<AbstractClass>]
type IntegerDistribution() =
    inherit Distribution<int>()
    override this.CumulativeProbability (m, n) = this.CumulativeProbability n - this.CumulativeProbability (m - 1)

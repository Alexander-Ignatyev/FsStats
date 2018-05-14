module FsStats.IntegerDistribution


[<AbstractClass>]
type IntegerDistribution() =
    abstract member Mean : float with get
    abstract member Variance : float with get
    default this.Variance = this.StdDev ** 2.0
    abstract member  StdDev : float with get
    default this.StdDev = sqrt this.Variance
    abstract member Probability : int -> float
    abstract member CumulativeProbability : int -> float
    abstract member CumulativeProbability : int*int -> float
    default self.CumulativeProbability (m, n) = self.CumulativeProbability n - self.CumulativeProbability (m-1)
    abstract member Sample : int with get
    member self.Samples m = Array.init m (fun _ -> self.Sample)



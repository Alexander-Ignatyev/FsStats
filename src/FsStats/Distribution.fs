module FsStats.Distribution

/// Abstract Distribution class
[<AbstractClass>]
type Distribution<'T>() =
    abstract member Mean : float with get
    abstract member Variance : float with get
    default this.Variance = this.StdDev ** 2.0
    abstract member  StdDev : float with get
    default this.StdDev = sqrt this.Variance
    abstract member Probability : 'T -> float
    /// P(X <= x)
    abstract member CumulativeProbability : 'T -> float
    /// P(x1 <= X <= x2)
    abstract member CumulativeProbability : 'T*'T -> float
    abstract member Sample : 'T with get
    member this.Samples m = Array.init m (fun _ -> this.Sample)

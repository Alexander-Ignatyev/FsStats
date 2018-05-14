module FsStats.Binomial

open FsStats.IntegerDistribution

/// Binomial Coefficient n choose k
let coefficient n k = 
    let k = max k (n-k)
    let a = Array.fold (*) 1 [|k+1 .. n|]
    let b = Array.fold (*) 1 [|1 .. n-k|]
    in a / b

/// Probability mass function.
/// The probability of getting exactly k successes in n trials
/// with probability of every single success equals p.
let pmf k n p =
    let numOutcomes = coefficient n k |> float
    let probSuccesses = p ** float k
    let probFailures = (1.0 - p) ** float (n - k)
    in numOutcomes * probSuccesses * probFailures


/// Binomial Distribution
/// n - number of trials
/// p - probability of success
type Distribution(n: int, p: float) =
    inherit IntegerDistribution()
    let mean = p * float n
    let variance = p * (1.0 - p) * float n
    let bernoulli = new Bernoulli.Distribution(p)

    override self.Mean = mean
    override self.Variance = variance
    override self.StdDev = sqrt variance

    override self.Probability k = pmf k n p

    override self.CumulativeProbability k = Array.sumBy self.Probability [|0 .. k|]

    override self.Sample = bernoulli.Samples n |> Array.sumBy (fun b -> if b then 1 else 0)

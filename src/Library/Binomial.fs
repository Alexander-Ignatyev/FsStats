module FsStats.Binomial

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

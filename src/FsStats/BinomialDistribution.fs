namespace FsStats


module BinomialDistribution =
    type T = { NuberOfTrials : int; P : float }


    /// Create Binomial Distribution.
    /// Takes number of trial and probability of success
    let create n p = { NuberOfTrials = n; P = p }


    let mean { NuberOfTrials = n; P = p } = p * float n

    let variance { NuberOfTrials = n; P = p } = p * (1.0 - p) * float n

    let stddev = sqrt << variance

    /// Binomial Coefficient n choose k
    let coefficient n k = 
        let k = max k (n-k)
        let a = Array.fold (*) 1 [|k+1 .. n|]
        let b = Array.fold (*) 1 [|1 .. n-k|]
        in a / b
    
    /// Probability Mass Function.
    /// The probability of getting exactly k successes in n trials
    /// with probability of every single success equals p.
    let pmf { NuberOfTrials = n; P = p } k =
        let numOutcomes = coefficient n k |> float
        let probSuccesses = p ** float k
        let probFailures = (1.0 - p) ** float (n - k)
        in numOutcomes * probSuccesses * probFailures
    
    /// Cumulative Distribution Function.
    let cdf d k = Array.sumBy (pmf d) [|0 .. k|]

    let sample { NuberOfTrials = n; P = p } (rnd : System.Random) = 
        let bd = BernoulliDistribution.create p
        BernoulliDistribution.samples bd rnd n |> Array.sum

    let samples d rnd m = Array.init m (fun _ -> sample d rnd)

namespace FsStats


module BinomialDistribution =
    type T = { NumberOfTrials : int; P : float }


    /// Create Binomial Distribution.
    /// Takes number of trial and probability of success
    let create n p = { NumberOfTrials = n; P = p }


    let mean { NumberOfTrials = n; P = p } = p * float n

    let variance { NumberOfTrials = n; P = p } = p * (1.0 - p) * float n

    let stddev = sqrt << variance


    /// Normal Approximation to the Binomial
    /// If true the binomial distribution might be approximated
    /// by normal distribution with mean = BinomialDistribution.mean
    /// and stddev = BinomialDistrinution.stddev
    let isNormalApproximationApplicable { NumberOfTrials = n; P = p } =
       p * float n >= 10.0 && (1.0 - p) * float n >= 10.0

    /// Binomial Coefficient n choose k
    let coefficient n k =
        if k > n then 0.0
        else
            let k = max k (n-k)
            let a = Array.fold (*) 1.0 [|k + 1.0 .. n|]
            let b = Array.fold (*) 1.0 [|1.0 .. n - k|]
            in a / b
    
    /// Probability Mass Function.
    /// The probability of getting exactly k successes in n trials
    /// with probability of every single success equals p.
    let pmf { NumberOfTrials = n; P = p } k =
        let n_, k_ = float n, float k
        let numOutcomes = coefficient n_ k_
        let probSuccesses = p ** k_
        let probFailures = (1.0 - p) ** (n_ - k_)
        in numOutcomes * probSuccesses * probFailures
    
    /// Cumulative Distribution Function.
    let cdf d k = Array.sumBy (pmf d) [|0 .. k|]

    /// Generates a random number from Binomial distribution
    let random { NumberOfTrials = n; P = p } (rnd : System.Random) = 
        let bd = BernoulliDistribution.create p
        BernoulliDistribution.sample bd rnd n |> Array.sum

    /// Generates a random sample of size m from Binomial distribution
    let sample d rnd m = Array.init m (fun _ -> random d rnd)

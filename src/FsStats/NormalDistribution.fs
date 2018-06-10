namespace FsStats

open FsStats.Special

module NormalDistribution =
    let rec private nextPair (rnd: System.Random) =
        let u = rnd.NextDouble() * 2.0 - 1.0
        let v = rnd.NextDouble() * 2.0 - 1.0
        let s = u*u + v*v
        if s > 0.0 && s < 1.0
        then (u, v)
        else nextPair rnd


    /// Marsaglia polar sampling method
    /// for generating a pair of independent 
    /// standard normal random variables
    let private polarMethod (rnd: System.Random) =
        let u, v = nextPair rnd
        let s = u*u + v*v
        let a = sqrt ((-2.0 * log s)/ s)
        (u*a, v*a)


    let private fromStandard (mu : float) sigma x = (x * sigma) + mu


    type T = { Mu: float; Sigma: float }

    let create mu sigma =
        { Mu = mu; Sigma = sigma }

    let mean { T.Mu = mu } = mu

    let variance { Sigma = sigma } = sigma * sigma

    let stddev { Sigma = sigma } = sigma

    let cdf { Mu = mu; Sigma = sigma } x =
        (1.0 + erf((x - mu) / (sigma * sqrt 2.0))) * 0.5

    /// Generates a random number from Normal distribution
    let random { Mu = mu; Sigma = sigma } (rnd : System.Random) =
        let x, _ = polarMethod rnd
        fromStandard mu sigma x

    /// Generates a random sample of size m from Normal distribution
    let sample d rnd m = Array.init m (fun _ -> random d rnd)

    /// z-score is the number of standard deviations from the mean a data point is.
    /// It takes mean mu, standard deviation sigma and the data point x. 
    let zScore { Mu = mu; Sigma = sigma } x = (x - mu) / sigma


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

    let sample { Mu = mu; Sigma = sigma } (rnd : System.Random) =
        let x, _ = polarMethod rnd
        fromStandard mu sigma x

    let samples d rnd m = Array.init m (fun _ -> sample d rnd)


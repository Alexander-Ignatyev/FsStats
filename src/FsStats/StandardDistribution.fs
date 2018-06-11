namespace FsStats

open System

open FsStats.Special

/// Standard Normal Distribution
module StandardDistribution =
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

    let mean = 0.0

    let variance = 1.0

    let stddev = 1.0

    /// Probability density function
    let pdf x = 1.0/(2.0 * Math.PI |> sqrt) * Math.Exp (-0.5 * (x**2.0))

    /// Cumulative distrinution function
    let cdf x = (1.0 + erf(x / (sqrt 2.0))) * 0.5

    /// Generates a random number 
    /// from Standard Normal distribution
    let random (rnd : System.Random) = 
        let x, _ = polarMethod rnd in x

    /// Generates a random sample of size m 
    /// from Standard Normal distribution
    let sample rnd m =
        let a = Array.zeroCreate m

        let populate i _ = 
            if i % 2 = 0 then
                let x, y = polarMethod rnd
                Array.set a i x
                if i + 1 < m then 
                    Array.set a (i + 1) y

        Array.iteri populate a
        a

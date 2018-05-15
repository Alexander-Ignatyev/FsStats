namespace FsStats

open FsStats.Special
open FsStats.RealDistribution


module Normal =
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
    let polarMethod (rnd: System.Random) =
        let u, v = nextPair rnd
        let s = u*u + v*v
        let a = sqrt ((-2.0 * log s)/ s)
        (u*a, v*a)


    let fromStandard mu sigma x = (x * sigma) + mu


type NormalDistribution(mu: float, sigma: float) =
    inherit RealDistribution()
    let rnd = new System.Random()
    override self.Mean = mu
    override self.Variance = sigma * sigma
    override self.StdDev = sigma

    /// Cumulative distribution function
    override self.Probability x = (1.0 + erf((x - mu) / (sigma * sqrt 2.0))) * 0.5

    override self.Sample = 
        let x, _ = Normal.polarMethod rnd
        Normal.fromStandard mu sigma x

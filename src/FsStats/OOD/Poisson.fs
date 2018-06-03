namespace FsStats.OOD

/// Poisson distribution
/// where mu is observed mean rate 
/// of independent events 
/// occurring within a fixed interval
type PoissonDistribution(mu: float) = 
    inherit DiscreteDistribution()
    let factorial x = 
        let rec util(value, acc) = 
            match value with
            |0 | 1 -> acc
            | _    -> util(value - 1, acc * value)
        util(x,1)

    let rnd = new System.Random()
    let knuthsSample mu (rnd : System.Random) =
        let l = exp (-mu)
        let rec util(k, p) =
            let p' = p * rnd.NextDouble()
            if p' > l
            then util(k+1, p')
            else k
        util(0, 1.0)

    override self.Mean = mu
    override self.Variance = mu
    override self.StdDev = sqrt mu
    override self.Probability k = (mu ** float k) * exp (-mu) / (float (factorial k))
    override self.CumulativeProbability k = 
        let s = Array.sumBy (fun i -> (mu ** float i) / (float (factorial i))) [|0 .. k|]
        exp (-mu) * s
    override self.Sample = knuthsSample mu rnd

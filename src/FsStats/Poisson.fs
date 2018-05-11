module FsStats.Poisson

let factorial x = 
    let rec util(value, acc) = 
        match value with
        |0 | 1 -> acc
        | _    -> util(value - 1, acc * value)
    util(x,1)

/// Poisson distribution
/// where mu is observed mean rate 
/// of independent events 
/// occurring within a fixed interval
type Distribution(mu: float) = 
    let rnd = new System.Random()
    let knuthsSample mu (rnd : System.Random) =
        let l = exp (-mu)
        let rec util(k, p) =
            let p' = p * rnd.NextDouble()
            if p' > l
            then util(k+1, p')
            else k
        util(0, 1.0)

    member self.Mean = mu
    member self.Variance = mu
    member self.StdDev = sqrt mu
    member self.Probability k = (mu ** float k) * exp (-mu) / (float (factorial k))
    member self.Sample = knuthsSample mu rnd
    member self.Samples k = Array.init k (fun _ -> self.Sample)

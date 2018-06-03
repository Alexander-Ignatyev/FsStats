namespace FsStats

/// Poisson distribution
/// where mu is observed mean rate 
/// of independent events 
/// occurring within a fixed interval
module PoissonDistribution =
    type T = { Mu : float }

    /// Create a Poisson distribution
    let create mu = { T.Mu = mu }

    let mean { T.Mu = mu } = mu

    let variance { T.Mu = mu } = mu

    let stddev { T.Mu = mu } = sqrt mu

    let private factorial x = 
        let rec util(value, acc) = 
            match value with
            |0 | 1 -> acc
            | _    -> util(value - 1, acc * value)
        util(x,1)

    let private knuthsSample mu (rnd : System.Random) =
        let l = exp (-mu)
        let rec util(k, p) =
            let p' = p * rnd.NextDouble()
            if p' > l
            then util(k+1, p')
            else k
        util(0, 1.0)
    
    /// Probability Mass Function.
    let pmf { T.Mu = mu } k = (mu ** float k) * exp (-mu) / (float (factorial k))

    /// Cumulative Distribution Function.
    let cdf { T.Mu = mu } k = 
        let s = Array.sumBy (fun i -> (mu ** float i) / (float (factorial i))) [|0 .. k|]
        exp (-mu) * s

    let sample { T.Mu = mu } (rnd : System.Random) = knuthsSample mu rnd

    let samples d rnd m = Array.init m (fun _ -> sample d rnd)

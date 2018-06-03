namespace FsStats


/// Student's t-distribution
module StudentsDistribution =

    /// Cumulative distribution function
    /// takes degrees of freedom and t score
    let rec cdf df t =
        if t < 0.0 then 1.0 - cdf df -t
        else 
            let nu = float df
            let x = nu / (t * t + nu)
            1.0 - 0.5 * Special.betainc (nu * 0.5) 0.5 x

namespace FsStats

open System

/// Student's t-distribution
module StudentsDistribution =

    type T = private { Nu : float }

    let create sampleSize = 
        if sampleSize > 1 then { Nu = float (sampleSize - 1); } 
        else invalidArg "sampleSize" "Student's t-distribution is defined only for sampleSize bigger than 1"

    let mean (_ : T) = 0.0
    let median (_ : T) = 0.0
    let mode (_ : T) = 0.0

    let variance { Nu = df } =
        if df > 2.0 then df / (df - 2.0)
        elif df > 1.0 then Double.PositiveInfinity
        else Double.NaN


    /// Probability density function
    let pdf { Nu = nu } x =
        let a = 0.5 * (nu + 1.0)
        let b = (Special.gammaLanczos a)
        let c = (sqrt (nu * Math.PI)) * Special.gammaLanczos (0.5 * nu)
        let d = 1.0 + x ** 2.0 / nu
        b / c * d ** -a


    /// Cumulative distribution function
    /// takes degrees of freedom and t score
    let rec cdf { Nu = nu } t =
        if t < 0.0 then 1.0 - cdf { Nu = nu } -t
        else 
            let x = nu / (t * t + nu)
            1.0 - 0.5 * Special.betainc (nu * 0.5) 0.5 x

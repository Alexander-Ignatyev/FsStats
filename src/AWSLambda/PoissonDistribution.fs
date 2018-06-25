namespace FsStats.AWSLambda

open System
open FsStats

module PoissonDistribution =
    type Request = {
        Params : PoissonDistribution.T
        Curve : bool
        Pmf : int option
        Cdf : int option
        Sample : int option
    }

    type Response = {
        Params : PoissonDistribution.T
        Mean : float
        StdDev : float
        Variance : float
        Curve : (int[] * float[]) option
        Pmf : float option
        Cdf : float option
        Sample : int[] option
    }

    let handleCurve (d : PoissonDistribution.T) b =
        if b then
            let s = PoissonDistribution.stddev d
            let start = Convert.ToInt32(Math.Floor (d.Mu - 4.0 * s))
            let finish = Convert.ToInt32(Math.Ceiling (d.Mu + 4.0 * s + 1.0))
            let x = [| start .. finish |]
            let y = Array.map (PoissonDistribution.pmf d) x
            Some (x, y)
        else None
        

    let rnd = Random()

    let handle (r: Request) = {
            Params = r.Params
            Mean = PoissonDistribution.mean r.Params
            StdDev = PoissonDistribution.stddev r.Params
            Variance = PoissonDistribution.variance r.Params
            Curve = handleCurve r.Params r.Curve
            Pmf = Option.map (PoissonDistribution.pmf r.Params) r.Pmf
            Cdf = Option.map (PoissonDistribution.cdf r.Params) r.Cdf
            Sample = Option.map (PoissonDistribution.sample r.Params rnd) r.Sample
        }

namespace FsStats.AWSLambda

open FsStats

module PoissonDistribution =
    type Request = {
        Params : PoissonDistribution.T
        Curve : int option
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

    let handleCurve (d : PoissonDistribution.T) n =
        let x = [| for x in 0 .. n -> x |]
        let y = Array.map (PoissonDistribution.pmf d) x
        (x, y)

    let rnd = System.Random()

    let handle (r: Request) = {
            Params = r.Params
            Mean = PoissonDistribution.mean r.Params
            StdDev = PoissonDistribution.stddev r.Params
            Variance = PoissonDistribution.variance r.Params
            Curve = Option.map (handleCurve r.Params) r.Curve
            Pmf = Option.map (PoissonDistribution.pmf r.Params) r.Pmf
            Cdf = Option.map (PoissonDistribution.cdf r.Params) r.Cdf
            Sample = Option.map (PoissonDistribution.sample r.Params rnd) r.Sample
        }

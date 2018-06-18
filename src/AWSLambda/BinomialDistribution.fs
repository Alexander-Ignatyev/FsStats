namespace FsStats.AWSLambda

open FsStats

module BinomialDistribution =
    type Request = {
        Params : BinomialDistribution.T
        Curve : int option
        Pmf : int option
        Cdf : int option
        Sample : int option
    }

    type Response = {
        Params : BinomialDistribution.T
        Mean : float
        StdDev : float
        Variance : float
        IsNormalApproximationApplicable : bool
        Curve : (int[] * float[]) option
        Pmf : float option
        Cdf : float option
        Sample : int[] option
    }

    let handleCurve (d : BinomialDistribution.T) n =
        let x = [| for x in 0 .. n -> x |]
        let y = Array.map (BinomialDistribution.pmf d) x
        (x, y)

    let rnd = System.Random()

    let handle (r: Request) = {
            Params = r.Params
            Mean = BinomialDistribution.mean r.Params
            StdDev = BinomialDistribution.stddev r.Params
            Variance = BinomialDistribution.variance r.Params
            IsNormalApproximationApplicable = BinomialDistribution.isNormalApproximationApplicable r.Params
            Curve = Option.map (handleCurve r.Params) r.Curve
            Pmf = Option.map (BinomialDistribution.pmf r.Params) r.Pmf
            Cdf = Option.map (BinomialDistribution.cdf r.Params) r.Cdf
            Sample = Option.map (BinomialDistribution.sample r.Params rnd) r.Sample
        }

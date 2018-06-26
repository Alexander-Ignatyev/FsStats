namespace FsStats.AWSLambda

open System
open FsStats

module BinomialDistribution =
    type Request = {
        Params : BinomialDistribution.T
        Curve : bool
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

    let handleCurve (d : BinomialDistribution.T) =
        let mu = BinomialDistribution.mean d
        let s = BinomialDistribution.stddev d
        let start = max 0 (Convert.ToInt32(Math.Floor (mu - 4.0 * s)))
        let finish = Convert.ToInt32(Math.Ceiling (mu + 4.0 * s + 1.0))
        let x = [| start .. finish |]
        let y = Array.map (BinomialDistribution.pmf d) x
        Some (x, y)


    let rnd = Random()

    let handle (r: Request) = {
            Params = r.Params
            Mean = BinomialDistribution.mean r.Params
            StdDev = BinomialDistribution.stddev r.Params
            Variance = BinomialDistribution.variance r.Params
            IsNormalApproximationApplicable = BinomialDistribution.isNormalApproximationApplicable r.Params
            Curve = if r.Curve then handleCurve r.Params else None
            Pmf = Option.map (BinomialDistribution.pmf r.Params) r.Pmf
            Cdf = Option.map (BinomialDistribution.cdf r.Params) r.Cdf
            Sample = Option.map (BinomialDistribution.sample r.Params rnd) r.Sample
        }

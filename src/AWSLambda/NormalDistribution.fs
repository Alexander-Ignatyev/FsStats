namespace FsStats.AWSLambda

open FsStats

module NormalDistribution =
    type Request = {
        Params : NormalDistribution.T
        Curve : int option
        Pdf : float option
        Cdf : float option
        Quantile : float option
        Sample : int option
    }

    type Response = {
        Params : NormalDistribution.T
        Curve : (float[] * float[]) option
        Pdf : float option
        Cdf : float option
        Quantile : float option
        Sample : float[] option
    }

    let handleCurve (d : NormalDistribution.T) n =
        let step = 8.0 * d.Sigma / float n
        let start = d.Mu - 4.0 * d.Sigma
        let x = [| for x in 0 .. n -> start + step * float x |]
        let y = Array.map (NormalDistribution.pdf d) x
        (x, y)
        

    let rnd = System.Random()

    let handle (r: Request) =
        let d = NormalDistribution.create r.Params.Mu r.Params.Sigma
        {
            Params = r.Params
            Curve = Option.map (handleCurve r.Params) r.Curve
            Pdf = Option.map (NormalDistribution.pdf r.Params) r.Pdf
            Cdf = Option.map (NormalDistribution.cdf r.Params) r.Cdf
            Quantile = Option.map (NormalDistribution.quantile r.Params) r.Quantile
            Sample = Option.map (NormalDistribution.sample r.Params rnd) r.Sample
        }

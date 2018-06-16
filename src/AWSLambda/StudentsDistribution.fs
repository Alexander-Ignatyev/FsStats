namespace FsStats.AWSLambda

open FsStats

module StudentsDistribution =
    type Request = {
        Params : StudentsDistribution.T
        Curve : int option
        Pdf : float option
        Cdf : float option
    }

    type Response = {
        Params : StudentsDistribution.T
        Mean : float
        StdDev : float
        Variance : float
        Curve : (float[] * float[]) option
        Pdf : float option
        Cdf : float option
    }

    let handleCurve (d : StudentsDistribution.T) n =
        let s = StudentsDistribution.stddev d
        let xBar = StudentsDistribution.mean d
        let step = 8.0 * s / float n
        let start = xBar - 4.0 * s
        let x = [| for x in 0 .. n -> start + step * float x |]
        let y = Array.map (StudentsDistribution.pdf d) x
        (x, y)

    let handle (r: Request) = {
            Params = r.Params
            Mean = StudentsDistribution.mean r.Params
            StdDev = StudentsDistribution.stddev r.Params
            Variance = StudentsDistribution.variance r.Params
            Curve = Option.map (handleCurve r.Params) r.Curve
            Pdf = Option.map (StudentsDistribution.pdf r.Params) r.Pdf
            Cdf = Option.map (StudentsDistribution.cdf r.Params) r.Cdf
        }

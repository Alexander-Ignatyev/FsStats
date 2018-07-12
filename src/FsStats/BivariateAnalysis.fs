namespace FsStats

open LanguagePrimitives

/// Determining the empirical relationship between two variables
module BivariateAnalysis =
    let create = SummaryStatistics.create

    let covariance (x : SummaryStatistics.T) (y : SummaryStatistics.T) = 
        let sampleLength = SummaryStatistics.size x - 1
        Array.zip x.Data y.Data
            |> Array.sumBy (fun (xp, yp) -> (xp - x.Mean) * (yp - y.Mean))
            |> fun s -> DivideByInt s sampleLength
          
    let correlation x y =
        (covariance x y) / (x.StdDev * y.StdDev)

    type Line = {
        Slope : float
        YIntercept : float
    }

    let linearRegression x y =
        let r = correlation x y
        let m = r * SummaryStatistics.stddev y / SummaryStatistics.stddev x
        let b = SummaryStatistics.mean y - m * SummaryStatistics.mean x
        {Slope = m; YIntercept = b}

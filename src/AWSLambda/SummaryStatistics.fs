namespace FsStats.AWSLambda

open FsStats


module SummaryStatistics =
    type Request = {
        Params : float[]
        Percentile : float option
        Correlation : float[] option
    }

    type Response = {
        Params : float[]
        Mean : float
        StdDev : float
        Variance : float
        Skewness : float
        Kurtosis : float
        Minimum : float
        Q2 : float
        Median : float
        Q4 : float
        Maximum : float
        IQR : float
        Percentile : float option
        Correlation : float option
    }

    let correlation lhs rhs = 
        let l = SummaryStatistics.create lhs
        let r = SummaryStatistics.create rhs
        SummaryStatistics.correlation l r

    let handle (r: Request) =
        let s = SummaryStatistics.createAndSort r.Params
        let minimum, q2, median, q4, maximum = SummaryStatistics.fivenum s
        {
            Params = r.Params
            Mean = SummaryStatistics.mean s
            StdDev = SummaryStatistics.stddev s
            Variance = SummaryStatistics.variance s
            Skewness = SummaryStatistics.skewness s
            Kurtosis = SummaryStatistics.kurtosis s
            Minimum = minimum
            Q2 = q2
            Median = median
            Q4 = q4
            Maximum = maximum
            IQR = SummaryStatistics.iqr s
            Percentile = Option.map (SummaryStatistics.percentile s) r.Percentile
            Correlation = Option.map (correlation r.Params) r.Correlation
        }
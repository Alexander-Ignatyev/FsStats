namespace FsStats

open System

module SummaryStatistics =

    type T = { Mean : float; StdDev : float; Data : float []; Sorted : bool }

    let private meanVariance data = 
        let mean = Array.average data
        let variance =
            let sampleLength = (Array.length data) - 1 
            let s = Array.sumBy (fun x -> (x - mean) ** 2.0) data
            LanguagePrimitives.DivideByInt s sampleLength
        (mean, variance)
        

    /// Create a SummaryStatitics object without data sorting. 
    /// It will require sorting for every percentile amd fivenum statistics call.
    let create data = 
        let mean, variance = meanVariance data
        { Mean = mean; StdDev = sqrt variance; Data = data; Sorted = false }

    /// Create a SummaryStatitics object with sorted data.
    let createWithSorted data = 
        let mean, variance = meanVariance data
        { Mean = mean; StdDev = sqrt variance; Data = data; Sorted = false }


    /// Create a SummaryStatitics object and sort data.
    let createAndSort = createWithSorted << Array.sort

    let mean { Mean = mu } =
        mu

    /// Standard Deviation
    let stddev { StdDev = std } =
        std

    /// Standard Error
    let stderr { StdDev = std; Data = data } =
        std / (Array.length data |> float |> sqrt)

    [<Obsolete("stdDev is depricated. Please use stddev instead")>]
    let stdDev = stddev

    let variance d = (stddev d) ** 2.0

    /// Calculates sample skewness,
    /// a measure of the asymmetry
    let skewness { Mean = mean; StdDev = sd; Data = data } =
        let n =(Array.length data) - 1
        let m = Array.sumBy (fun x -> (x - mean) ** 3.0) data
        (LanguagePrimitives.DivideByInt m n) / (sd ** 3.0)

    /// Calculates sample kurtosis,
    /// a measure of the "tailedness"
    let kurtosis { Mean = mean; StdDev = sd; Data = data } =
        let n = (Array.length data) - 1
        let m = Array.sumBy (fun x -> (x - mean) ** 4.0) data
        (LanguagePrimitives.DivideByInt m n) / (sd ** 4.0)
 
    let private percentileForSorted data k =
        let p = k * (Array.length data |> float)
        if abs (Math.Floor(p) - p) < Double.Epsilon
        then
            let i = int p
            (data.[i-1] + data.[i]) * 0.5
        else
            let i = Math.Ceiling p |> int
            data.[i-1]

    /// Calculates k-th percentile
    let percentile { Data = data; Sorted = sorted } k =
        if k <= 0.0 || k >= 1.0 then invalidArg "k" "k must be between 0 and 1"
        if sorted then percentileForSorted data k
        else percentileForSorted (Array.sort data) k

    let median sd = percentile sd 0.5

    /// Five-number summary
    /// (sample minimum, first quartile, median, third quartile, sample maximum)
    let fivenum { Data = data; Sorted = sorted } =
        let d = if sorted then data else Array.sort data
        let prc = percentileForSorted d
        (d.[0], prc 0.25, prc 0.5, prc 0.75, d.[Array.length d - 1])

    
    /// Interquartile range
    let iqr { Data = data; Sorted = sorted } =
        let d = if sorted then data else Array.sort data
        let prc = percentileForSorted d
        prc 0.75 - prc 0.25


    let size { Data = data } =
        Array.length data


    /// Sample Correlation
    let correlation x y =
        let sampleLength = (Array.length x.Data) - 1
        let cov = Array.zip x.Data y.Data
                  |> Array.sumBy (fun (xp, yp) -> (xp - x.Mean) * (yp - y.Mean))
                  |> fun s -> LanguagePrimitives.DivideByInt s sampleLength
        cov / (x.StdDev * y.StdDev)

    let marginOfError s confidenceLevel =
        let se = stderr s
        let zValue = StandardDistribution.zValue confidenceLevel
        zValue * se

    let isNormalApproximationApplicable { Data = data } =
        let boundary = 30
        Array.length data >= boundary

    /// Calculates confidence interval for the population mean
    /// with given confidence level
    let confidenceInterval s level =
        let moe = marginOfError s level
        let xBar = mean s
        (xBar - moe, xBar + moe)

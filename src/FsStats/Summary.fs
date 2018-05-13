module FsStats.Summary

let DivideByInt = LanguagePrimitives.DivideByInt

/// z-score is the number of standard deviations from the mean a data point is.
/// It takes mean mu, standard deviation sigma and the data point x. 
let zScore mu sigma x = (x - mu) / sigma


/// Sample Summary Statistics
type SummaryStatistics(data: float []) =
    let sampleLength = (Array.length data) - 1
    let mean = Array.average data
    let variance = 
        let s = Array.sumBy (fun x -> (x - mean) ** 2.0) data
        DivideByInt s sampleLength

    member self.SampleLength = sampleLength
    /// Sample mean
    member self.Mean = mean

    /// Sample variance
    member self.Variance = variance

    /// Sample standard deviation
    member self.StdDev = sqrt variance

    /// Sample z-score
    member self.zScore = zScore mean (sqrt variance)


/// Sample Correlation
let correlation dataX dataY =
    let summaryX = new SummaryStatistics(dataX)
    let summaryY = new SummaryStatistics(dataY)
    let muX = summaryX.Mean
    let muY = summaryY.Mean
    let cov = Array.zip dataX dataY
              |> Array.sumBy (fun (x, y) -> (x - muX) * (y - muY))
              |> fun s -> DivideByInt s summaryX.SampleLength
    cov / (summaryX.StdDev * summaryY.StdDev)

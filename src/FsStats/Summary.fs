namespace FsStats

/// Sample Summary Statistics
type SummaryStatistics(data: float []) =
    let sampleLength = (Array.length data) - 1
    let mean = Array.average data
    let variance = 
        let s = Array.sumBy (fun x -> (x - mean) ** 2.0) data
        LanguagePrimitives.DivideByInt s sampleLength

    member self.SampleLength = sampleLength
    /// Sample mean
    member self.Mean = mean

    /// Sample variance
    member self.Variance = variance

    /// Sample standard deviation
    member self.StdDev = sqrt variance

module Summary =

    /// Sample Correlation
    let correlation dataX dataY =
        let summaryX = new SummaryStatistics(dataX)
        let summaryY = new SummaryStatistics(dataY)
        let muX = summaryX.Mean
        let muY = summaryY.Mean
        let cov = Array.zip dataX dataY
                  |> Array.sumBy (fun (x, y) -> (x - muX) * (y - muY))
                  |> fun s -> LanguagePrimitives.DivideByInt s summaryX.SampleLength
        cov / (summaryX.StdDev * summaryY.StdDev)

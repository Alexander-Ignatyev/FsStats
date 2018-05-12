module FsStats.Summary

type SummaryStatistics(data: float []) =
    let mean = Array.average data
    let variance = 
        let s = Array.sumBy (fun x -> (x - mean) ** 2.0) data
        LanguagePrimitives.DivideByInt s ((Array.length data) - 1)

    // Sample mean
    member self.Mean = mean

    // Sample variance
    member self.Variance = variance

    // Sample standard deviation
    member self.StdDev = sqrt variance

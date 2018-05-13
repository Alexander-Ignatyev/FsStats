module FsStats.Summary


/// z-score is the number of standard deviations from the mean a data point is.
/// It takes mean mu, standard deviation sigma and the data point x. 
let zScore mu sigma x = (x - mu) / sigma


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

    /// Sample z-score
    member self.zScore = zScore mean (sqrt variance)

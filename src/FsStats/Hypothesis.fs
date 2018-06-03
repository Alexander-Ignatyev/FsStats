namespace FsStats

module Hypothesis = 
    /// z-score is the number of standard deviations from the mean a data point is.
    /// It takes mean mu, standard deviation sigma and the data point x. 
    let zScore mu sigma x = (x - mu) / sigma


    type TestType =
        | LowerTailed
        | UpperTailed
        | TwoTailed


    let private performTest cdf testType score =
        let p = cdf -(abs score)
        let pValue = match testType with
                     | LowerTailed -> if score < 0.0 then p else 1.0 - p
                     | UpperTailed -> if score < 0.0 then 1.0 - p else p
                     | TwoTailed -> 2.0 * p
        pValue


    /// Perform Z-Test for the given true mean, true standard deviation, sample mean and and size of the sample.  
    let zTest trueMean trueStd sampleMean sampleSize testType =
        let standard = NormalDistribution.create 0.0 1.0
        let standardError = trueStd / sqrt (float sampleSize)
        let z = (sampleMean - trueMean) / standardError
        performTest (NormalDistribution.cdf standard) testType z

        
    /// Perform Z-Test for the given true mean, true standard deviation and array of sample data.
    let zTestForSample trueMean trueStd sample testType =
        let summary = SummaryStatistics(sample)
        zTest trueMean trueStd summary.Mean (Array.length sample) testType


    /// Perform Student's T-Test for the given true mean and array of sample data.
    let tTestForSample trueMean sample testType = 
        let summary = new SummaryStatistics(sample)
        let sampleSize = Array.length sample
        let standardError = summary.StdDev / sqrt (sampleSize |> float)
        let tScore = (summary.Mean - trueMean) / standardError
        performTest (Student.cdf (sampleSize - 1)) testType tScore



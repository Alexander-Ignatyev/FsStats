namespace FsStats

module Hypothesis = 
    /// Test Hypothesis
    /// Returns True if we cannot reject the hypothesis
    /// for the given distribution, threshould value alpha and random value x
    let testHypothesis  (distribution : Distribution<'T>) (alpha : float) (x : 'T) =
        let p = distribution.CumulativeProbability x
        if (System.Convert.ToDouble x) < distribution.Mean
        then p >= alpha
        else (1.0 - p) >= alpha

    /// z-score is the number of standard deviations from the mean a data point is.
    /// It takes mean mu, standard deviation sigma and the data point x. 
    let zScore mu sigma x = (x - mu) / sigma


    type TestType =
        | LowerTailed
        | UpperTailed
        | TwoTailed


    /// Perform Z-Test for the given true mean, true standard deviation, sample mean and and size of the sample.  
    let zTest trueMean trueStd sampleMean sampleSize testType =
        let standard = new NormalDistribution(0.0, 1.0)
        let standardError = trueStd / sqrt (float sampleSize)
        let z = (sampleMean - trueMean) / standardError
        let p = standard.CumulativeProbability -(abs z)
        let pValue = match testType with
                     | LowerTailed -> if z < 0.0 then p else 1.0 - p
                     | UpperTailed -> if z < 0.0 then 1.0 - p else p
                     | TwoTailed -> 2.0 * p
        pValue


    /// Perform Z-Test for the given true mean, true standard deviation and array of sample data.
    let zTestForArray trueMean trueStd sample testType =
        let summary = SummaryStatistics(sample)
        zTest trueMean trueStd summary.Mean (Array.length sample) testType

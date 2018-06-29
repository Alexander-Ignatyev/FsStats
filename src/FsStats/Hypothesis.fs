namespace FsStats

module Hypothesis =
    type TestType =
        | LowerTailed
        | UpperTailed
        | TwoTailed


    // One-sample tests
    module OneSample =
        let private performTest cdf testType score =
            let p = cdf -(abs score)
            let pValue = match testType with
                         | LowerTailed -> if score < 0.0 then p else 1.0 - p
                         | UpperTailed -> if score < 0.0 then 1.0 - p else p
                         | TwoTailed -> 2.0 * p
            pValue

        let score trueMean std sampleMean sampleSize =
            let standardError = std / sqrt (float sampleSize)
            (sampleMean - trueMean) / standardError
            
        /// Perform Z-Test for the given true mean, true standard deviation, sample mean and and size of the sample.  
        let zTest trueMean trueStd sampleMean sampleSize testType =
            let z = score trueMean trueStd sampleMean sampleSize
            performTest StandardDistribution.cdf testType z

        /// Perform Student's T-Test for the given true mean, sample mean, sample standard deviation and size of the sample.
        let tTest trueMean sampleMean sampleStd sampleSize testType =
            let t = score trueMean sampleStd sampleMean sampleSize
            let d = StudentsDistribution.create sampleSize
            performTest (StudentsDistribution.cdf d) testType t


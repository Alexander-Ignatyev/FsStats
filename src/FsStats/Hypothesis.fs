namespace FsStats

open LanguagePrimitives

module Hypothesis =
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


    /// One Population Mean Tests
    module OnePopulationMean =
        type T = { 
            PopulationMean : float
            SampleMean : float
            SampleSize : int
            StdDev : float
            TestType : TestType
        }

        let score { PopulationMean = trueMean; SampleMean = sampleMean; SampleSize = sampleSize; StdDev = std } = 
            let standardError = std / sqrt (float sampleSize)
            (sampleMean - trueMean) / standardError

        let zTest opm =
            let z = score opm
            performTest StandardDistribution.cdf opm.TestType z

        let tTest opm =
            let t = score opm
            let d = StudentsDistribution.create opm.SampleSize
            performTest (StudentsDistribution.cdf d) opm.TestType t


    /// One-sample tests
    [<System.Obsolete("OneSample is depricated. Please use OnePopulationMean module instead")>]
    module OneSample =
        open OnePopulationMean
        let score trueMean std sampleMean sampleSize =
            let opm = {
                PopulationMean = trueMean
                SampleMean = sampleMean
                SampleSize = sampleSize
                StdDev = std
                TestType = TwoTailed
            }
            OnePopulationMean.score opm
            
        /// Perform Z-Test for the given true mean, true standard deviation, sample mean and and size of the sample.  
        let zTest trueMean trueStd sampleMean sampleSize testType =
            let opm = {
                PopulationMean = trueMean
                SampleMean = sampleMean
                SampleSize = sampleSize
                StdDev = trueStd
                TestType = testType
            }
            OnePopulationMean.zTest opm

        /// Perform Student's T-Test for the given true mean, sample mean, sample standard deviation and size of the sample.
        let tTest trueMean sampleMean sampleStd sampleSize testType =
            let opm = {
                PopulationMean = trueMean
                SampleMean = sampleMean
                SampleSize = sampleSize
                StdDev = sampleStd
                TestType = testType
            }
            OnePopulationMean.tTest opm


    /// One Population Proportion Tests
    module OnePopulationProportion =
        type T = { 
            PopulationProportion : float
            SampleProportion : float
            SampleSize : int
            TestType : TestType
        }

        let score { PopulationProportion = p0; SampleProportion = p; SampleSize = n } = 
            let standardError = DivideByInt (p0 * (1.0 - p0)) n |> sqrt
            (p - p0) / standardError

        let zTest opp =
            let z = score opp
            performTest StandardDistribution.cdf opp.TestType z

        let tTest opp =
            let t = score opp
            let d = StudentsDistribution.create opp.SampleSize
            performTest (StudentsDistribution.cdf d) opp.TestType t

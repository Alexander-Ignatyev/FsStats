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
        }

        let score { PopulationMean = trueMean; SampleMean = sampleMean; SampleSize = sampleSize; StdDev = std } = 
            let standardError = std / sqrt (float sampleSize)
            (sampleMean - trueMean) / standardError

        let zTest opm testType =
            let z = score opm
            performTest StandardDistribution.cdf testType z

        let tTest opm testType =
            let t = score opm
            let d = StudentsDistribution.create opm.SampleSize
            performTest (StudentsDistribution.cdf d) testType t


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
            }
            OnePopulationMean.score opm
            
        /// Perform Z-Test for the given true mean, true standard deviation, sample mean and and size of the sample.  
        let zTest trueMean trueStd sampleMean sampleSize testType =
            let opm = {
                PopulationMean = trueMean
                SampleMean = sampleMean
                SampleSize = sampleSize
                StdDev = trueStd
            }
            OnePopulationMean.zTest opm testType

        /// Perform Student's T-Test for the given true mean, sample mean, sample standard deviation and size of the sample.
        let tTest trueMean sampleMean sampleStd sampleSize testType =
            let opm = {
                PopulationMean = trueMean
                SampleMean = sampleMean
                SampleSize = sampleSize
                StdDev = sampleStd
            }
            OnePopulationMean.tTest opm testType


    /// One Population Proportion Tests
    module OnePopulationProportion =
        type T = { 
            PopulationProportion : float
            SampleProportion : float
            SampleSize : int
        }

        let score { PopulationProportion = p0; SampleProportion = p; SampleSize = n } = 
            let standardError = DivideByInt (p0 * (1.0 - p0)) n |> sqrt
            (p - p0) / standardError

        let zTest opp testType =
            let z = score opp
            performTest StandardDistribution.cdf testType z

        let tTest opp testType =
            let t = score opp
            let d = StudentsDistribution.create opp.SampleSize
            performTest (StudentsDistribution.cdf d) testType t

    
    /// Compare Two Population Averages
    module TwoPopulationAverages =
        type T = {
            Mean : float     // Sample mean
            StdDev : float   // Sample standard deviation
            Size : int       // Sample size
        }

        let standardError s1 s2 =
            let se2 {StdDev = std; Size = n} = DivideByInt (std ** 2.0) n
            sqrt (se2 s1 + se2 s2)

        let score s1 s2 =
            (s1.Mean - s2.Mean) / standardError s1 s2


        let zTest s1 s2 testType =
            let z = score s1 s2
            performTest StandardDistribution.cdf testType z

        let tTest s1 s2 testType =
            let t = score s1 s2
            let d = StudentsDistribution.create (min s1.Size s2.Size)
            performTest (StudentsDistribution.cdf d) testType t


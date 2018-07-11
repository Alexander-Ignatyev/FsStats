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


    /// Compare Two Population Proportions
    module TwoPopulationProportions =
        type T = {
            Proportion : float
            Size : int
        }

        let standardError {Proportion = p1; Size = n1} {Proportion = p2; Size = n2} =
            let overallProportion = DivideByInt (p1 * float n1 + p2 * float n2) (n1 + n2)
            let variance = overallProportion * (1.0 - overallProportion)
            let n = (DivideByInt 1.0 n1) + (DivideByInt 1.0 n2)
            sqrt (variance * n)

        let score pp1 pp2 =
            (pp1.Proportion - pp2.Proportion) / standardError pp1 pp2

        let zTest pp1 pp2 testType =
            let z = score pp1 pp2
            performTest StandardDistribution.cdf testType z

        let tTest pp1 pp2 testType =
            let t = score pp1 pp2
            let d = StudentsDistribution.create (min pp1.Size pp2.Size)
            performTest (StudentsDistribution.cdf d) testType t

    /// Paired Average Difference Test
    module PairedAverageDifference =
        open OnePopulationMean

        let create sample1 sample2 =
            if Array.length sample1 <> Array.length sample2 then invalidArg "sample1, sample2" "both samples must have the same length"
            let diff = Array.map2 (-) sample1 sample2
            let ss = SummaryStatistics.create diff
            {
                PopulationMean = 0.0
                SampleMean = SummaryStatistics.mean ss
                StdDev = SummaryStatistics.stddev ss
                SampleSize = SummaryStatistics.size ss
            }

        let score = OnePopulationMean.score

        let zTest = OnePopulationMean.zTest

        let tTest = OnePopulationMean.tTest

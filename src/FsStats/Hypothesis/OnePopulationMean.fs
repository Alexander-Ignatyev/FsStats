﻿namespace FsStats.Hypothesis

open FsStats


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
        Test.perform StandardDistribution.cdf testType z

    let tTest opm testType =
        let t = score opm
        let d = StudentsDistribution.create opm.SampleSize
        Test.perform (StudentsDistribution.cdf d) testType t

   
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

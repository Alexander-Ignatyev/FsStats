namespace FsStats.AWSLambda

open FsStats

module Hypothesis =
    type OneSampleMeanTest = {
        TrueMean : float
        StdDev : float   // Population std dev for Z-Test or Sample std dev for t-Test
        SampleMean : float
        SampleSize : int
        TestType : Hypothesis.TestType
    }

    type Request = {
        OneSampleZTest : OneSampleMeanTest option
        OneSampleTTest : OneSampleMeanTest option
    }

    type Response = {
        OneSampleZTest : float option
        OneSampleTTest : float option
    }

    let doOneSampleZTest {TrueMean = mu; StdDev = std; SampleMean = mean; SampleSize = size; TestType = testType} =
        Hypothesis.OneSample.zTest mu std mean size testType

    let doOneSampleTTest {TrueMean = mu; StdDev = std; SampleMean = mean; SampleSize = size; TestType = testType} =
        Hypothesis.OneSample.tTest mu mean std size testType

    let handle (r: Request) = {
        OneSampleZTest = Option.map doOneSampleZTest r.OneSampleZTest
        OneSampleTTest = Option.map doOneSampleTTest r.OneSampleTTest
    }

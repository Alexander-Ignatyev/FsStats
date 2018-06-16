namespace FsStats.AWSLambda

open FsStats

module Hypothesis =
    type OneSampleZTest = {
        TrueMean : float
        TrueStdDev : float
        SampleMean : float
        SampleSize : int
        TestType : Hypothesis.TestType
    }

    type OneSampleTTest = {
        TrueMean : float
        SampleMean : float
        SampleStdDev : float
        SampleSize : int
        TestType : Hypothesis.TestType
    }

    type Request = {
        OneSampleZTest : OneSampleZTest option
        OneSampleTTest : OneSampleTTest option
    }

    type Response = {
        OneSampleZTest : float option
        OneSampleTTest : float option
    }

    let doOneSampleZTest {TrueMean = mu; TrueStdDev = std; SampleMean = mean; SampleSize = size; TestType = testType} =
        Hypothesis.OneSample.zTest mu std mean size testType

    let doOneSampleTTest {TrueMean = mu; SampleMean = mean; SampleStdDev = std; SampleSize = size; TestType = testType} =
        Hypothesis.OneSample.tTest mu mean std size testType

    let handle (r: Request) = {
        OneSampleZTest = Option.map doOneSampleZTest r.OneSampleZTest
        OneSampleTTest = Option.map doOneSampleTTest r.OneSampleTTest
    }

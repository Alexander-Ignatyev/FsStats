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

    type OneSampleMeanTestResult = {
        PValue : float
        Score : float
        // True of Hypothesis rejected at the given significance level
        RejectedAtSignificanceLevel001 : bool
        RejectedAtSignificanceLevel005 : bool
        RejectedAtSignificanceLevel010 : bool
    }

    type Request = {
        OneSampleZTest : OneSampleMeanTest option
        OneSampleTTest : OneSampleMeanTest option
    }

    type Response = {
        OneSampleZTest : OneSampleMeanTestResult option
        OneSampleTTest : OneSampleMeanTestResult option
    }

    let doOneSampleZTest {TrueMean = mu; StdDev = std; SampleMean = mean; SampleSize = size; TestType = testType} =
        let score = Hypothesis.OneSample.score mu std mean size
        let pValue = Hypothesis.OneSample.zTest mu std mean size testType
        {
            PValue = pValue
            Score = score
            RejectedAtSignificanceLevel001 = pValue < 0.01
            RejectedAtSignificanceLevel005 = pValue < 0.05
            RejectedAtSignificanceLevel010 = pValue < 0.10

        }

    let doOneSampleTTest {TrueMean = mu; StdDev = std; SampleMean = mean; SampleSize = size; TestType = testType} =
        let score = Hypothesis.OneSample.score mu std mean size
        let pValue = Hypothesis.OneSample.tTest mu mean std size testType
        {
            PValue = pValue
            Score = score
            RejectedAtSignificanceLevel001 = pValue < 0.01
            RejectedAtSignificanceLevel005 = pValue < 0.05
            RejectedAtSignificanceLevel010 = pValue < 0.10

        }

    let handle (r: Request) = {
        OneSampleZTest = Option.map doOneSampleZTest r.OneSampleZTest
        OneSampleTTest = Option.map doOneSampleTTest r.OneSampleTTest
    }

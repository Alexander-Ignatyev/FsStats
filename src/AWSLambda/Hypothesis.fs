namespace FsStats.AWSLambda

open FsStats

module Hypothesis =
    open Hypothesis.OnePopulationMean

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

    let convert {TrueMean = mu; StdDev = std; SampleMean = mean; SampleSize = size; TestType = testType} =
        ({PopulationMean = mu; StdDev = std; SampleMean = mean; SampleSize = size}, testType)

    let doOneSampleZTest (opm, testType) =
        let score = score opm
        let pValue = zTest opm testType
        {
            PValue = pValue
            Score = score
            RejectedAtSignificanceLevel001 = pValue < 0.01
            RejectedAtSignificanceLevel005 = pValue < 0.05
            RejectedAtSignificanceLevel010 = pValue < 0.10

        }

    let doOneSampleTTest (opm, testType) =
        let score = score opm
        let pValue = tTest opm testType
        {
            PValue = pValue
            Score = score
            RejectedAtSignificanceLevel001 = pValue < 0.01
            RejectedAtSignificanceLevel005 = pValue < 0.05
            RejectedAtSignificanceLevel010 = pValue < 0.10

        }

    let handle (r: Request) = {
        OneSampleZTest = Option.map doOneSampleZTest (Option.map convert r.OneSampleZTest)
        OneSampleTTest = Option.map doOneSampleTTest (Option.map convert r.OneSampleTTest)
    }

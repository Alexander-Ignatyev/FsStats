namespace FsStats.AWSLambda

open FsStats.Hypothesis

module OnePopulationMeanTest =
    open OnePopulationMean

    type Request = {
        Params : OnePopulationMean.T
        TestType : TestType
    }

    type Response = {
        Score : float
        ZTest : float
        TTest : float
    }

    let handle (r: Request) = {
        Score = score r.Params
        ZTest = zTest r.Params r.TestType
        TTest = tTest r.Params r.TestType
    }

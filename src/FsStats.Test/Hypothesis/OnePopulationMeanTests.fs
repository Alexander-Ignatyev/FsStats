namespace FsStats.Tests

#nowarn "44"  // Hypothesis.OneSample is depricated but still requires to be tested

open Xunit
open FsUnit.Xunit

open FsStats
open FsStats.Hypothesis
open Utils

module ``One Population Mean`` =
    open OnePopulationMean

    [<Theory>]
    [<InlineData(55.0, 10.0, 47, 20, 0.00017)>]
    [<InlineData(-55.0, 10.0, -47, 20, 0.99983)>]
    [<InlineData(35.0, 10.0, 47, 20, 1.0)>]
    [<InlineData(-35.0, 10.0, -47, 20, 0.0)>]
    let ``Z-Test Should Work`` (trueMean, trueStd, sampleMean, sampleSize, lowerP) =
        let (lowerTail, upperTail, twoTail) = getTestExpectedResults 1e-5 lowerP
        let opm = {
            PopulationMean = trueMean
            SampleMean = sampleMean
            SampleSize = sampleSize
            StdDev = trueStd
        }
        zTest opm LowerTailed |> lowerTail
        zTest opm UpperTailed |> upperTail
        zTest opm TwoTailed |> twoTail


    [<Theory>]
    [<InlineData(55.0, 10.0, "35.0, 45.0, 58.0, 44.0, 48.0", 0.02209)>]
    [<InlineData(-55.0, 10.0, "-35.0, -45.0, -58.0, -44.0, -48.0", 0.97791)>]
    [<InlineData(35.0, 10.0, "35.0, 45.0, 58.0, 44.0, 48.0",  0.99305)>]
    [<InlineData(-35.0, 10.0, "-35.0, -45.0, -58.0, -44.0, -48.0", 0.00695)>]
    let ``Z-Test for Sample Should Work`` (trueMean, trueStd, sampleString: string, lowerP) =
        let sample = sampleString.Split(',') |> Array.map float
        let (lowerTail, upperTail, twoTail) = getTestExpectedResults 1e-5 lowerP
        let summary = SummaryStatistics.create sample
        let sampleMean = SummaryStatistics.mean summary
        let sampleSize = Array.length sample
        let opm = {
            PopulationMean = trueMean
            SampleMean = sampleMean
            SampleSize = sampleSize
            StdDev = trueStd
        }
        zTest opm LowerTailed |> lowerTail
        zTest opm UpperTailed |> upperTail
        zTest opm TwoTailed |> twoTail


    [<Theory>]
    [<InlineData(55.0, "35.0, 45.0, 58.0, 44.0, 48.0", 0.03593)>]
    [<InlineData(-55.0, "-35.0, -45.0, -58.0, -44.0, -48.0", 0.96407)>]
    [<InlineData(35.0, "35.0, 45.0, 58.0, 44.0, 48.0", 0.97947)>]
    [<InlineData(-35.0, "-35.0, -45.0, -58.0, -44.0, -48.0", 0.02053)>]
    let ``T-Test for Sample Should Work`` (trueMean, sampleString: string, lowerP) =
        let sample = sampleString.Split(',') |> Array.map float
        let (lowerTail, upperTail, twoTail) = getTestExpectedResults 1e-5 lowerP
        let summary = SummaryStatistics.create sample
        let sampleMean = SummaryStatistics.mean summary
        let sampleStd = SummaryStatistics.stddev summary
        let sampleSize = Array.length sample
        let opm = {
            PopulationMean = trueMean
            SampleMean = sampleMean
            SampleSize = sampleSize
            StdDev = sampleStd
        }
        tTest opm LowerTailed |> lowerTail
        tTest opm UpperTailed |> upperTail
        tTest opm TwoTailed |> twoTail


    [<Theory>]
    [<InlineData(55.0, 10.0, 47, 20, -3.57771)>]
    [<InlineData(-55.0, 10.0, -47, 20, 3.57771)>]
    [<InlineData(35.0, 10.0, 47, 20, 5.36656)>]
    [<InlineData(-35.0, 10.0, -47, 20, -5.36656)>]
    let ``score Should Work`` (trueMean, trueStd, sampleMean, sampleSize, expected) =
        let opm = {
            PopulationMean = trueMean
            SampleMean = sampleMean
            SampleSize = sampleSize
            StdDev = trueStd
        }
        score opm |> should (equalWithin 1e-5) expected


module ``One Sample Tests`` =
    let getZTestExpectedResults lowerP =
        if lowerP < 0.5 then
            (should lessThan lowerP, should greaterThan (1.0 - lowerP), should lessThan (2.0 * lowerP))
        else
            (should greaterThan lowerP, should lessThan (1.0 - lowerP), should lessThan (2.0 * (1.0 - lowerP)))


    [<Theory>]
    [<InlineData(55.0, 10.0, 47, 20, 0.05)>]
    [<InlineData(35.0, 10.0, 47, 20, 0.95)>]
    [<InlineData(-35.0, 10.0, -47, 20, 0.05)>]
    [<InlineData(-55.0, 10.0, -47, 20, 0.95)>]
    let ``One-sample Z-Test Should Work`` (trueMean, trueStd, sampleMean, sampleSize, lowerP) =
        let (lowerTail, upperTail, twoTail) = getZTestExpectedResults lowerP
        OneSample.zTest trueMean trueStd sampleMean sampleSize LowerTailed |> lowerTail
        OneSample.zTest trueMean trueStd sampleMean sampleSize UpperTailed |> upperTail
        OneSample.zTest trueMean trueStd sampleMean sampleSize TwoTailed |> twoTail


    [<Theory>]
    [<InlineData(55.0, 10.0, "35.0, 45.0, 58.0, 44.0, 48.0", 0.05)>]
    [<InlineData(35.0, 10.0, "35.0, 45.0, 58.0, 44.0, 48.0", 0.95)>]
    [<InlineData(-35.0, 10.0, "-35.0, -45.0, -58.0, -44.0, -48.0", 0.05)>]
    [<InlineData(-55.0, 10.0, "-35.0, -45.0, -58.0, -44.0, -48.0", 0.95)>]
    let ``One-sample Z-Test for Sample Should Work`` (trueMean, trueStd, sampleString: string, lowerP) =
        let sample = sampleString.Split(',') |> Array.map float
        let (lowerTail, upperTail, twoTail) = getZTestExpectedResults lowerP
        let summary = SummaryStatistics.create sample
        let sampleMean = SummaryStatistics.mean summary
        let sampleSize = Array.length sample
        OneSample.zTest trueMean trueStd sampleMean sampleSize LowerTailed |> lowerTail
        OneSample.zTest trueMean trueStd sampleMean sampleSize UpperTailed |> upperTail
        OneSample.zTest trueMean trueStd sampleMean sampleSize TwoTailed |> twoTail


    [<Theory>]
    [<InlineData(55.0, "35.0, 45.0, 58.0, 44.0, 48.0", 0.05)>]
    [<InlineData(35.0, "35.0, 45.0, 58.0, 44.0, 48.0", 0.95)>]
    [<InlineData(-35.0, "-35.0, -45.0, -58.0, -44.0, -48.0", 0.05)>]
    [<InlineData(-55.0, "-35.0, -45.0, -58.0, -44.0, -48.0", 0.95)>]
    let ``One-sample T-Test for Sample Should Work`` (trueMean, sampleString: string, lowerP) =
        let sample = sampleString.Split(',') |> Array.map float
        let (lowerTail, upperTail, twoTail) = getZTestExpectedResults lowerP
        let summary = SummaryStatistics.create sample
        let sampleMean = SummaryStatistics.mean summary
        let sampleStd = SummaryStatistics.stddev summary
        let sampleSize = Array.length sample
        OneSample.tTest trueMean sampleMean sampleStd sampleSize LowerTailed |> lowerTail
        OneSample.tTest trueMean sampleMean sampleStd sampleSize UpperTailed |> upperTail
        OneSample.tTest trueMean sampleMean sampleStd sampleSize TwoTailed |> twoTail



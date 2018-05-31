module ``Statistical Hypothesis Tests``

open Xunit
open FsUnit.Xunit

open FsStats
open FsStats.Hypothesis


[<Fact>]
let ``Test Hypothesises of Integer Distribution`` () =
    let bd = new BinomialDistribution(20, 18.0/38.0)
    testHypothesis bd 0.05 4 |> should be False
    testHypothesis bd 0.05 7 |> should be True

[<Fact>]
let ``Test Hypothesises of Real Distribution`` () =
    let nd = new NormalDistribution(10.0, 3.0)
    testHypothesis nd 0.05 2.0 |> should be False
    testHypothesis nd 0.05 7.0 |> should be True

[<Theory>]
[<InlineData(0.0, 1.0, 2.0, 2.0)>]
[<InlineData(10.0, 3.0, 1.0, -3.0)>]
let ``z-score tests`` (mu, sigma, x, expectedZScore) = 
    zScore mu sigma x |> should (equalWithin 1e-5) expectedZScore


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
let ``Z-Test Should Work`` (trueMean, trueStd, sampleMean, sampleSize, lowerP) =
    let (lowerTail, upperTail, twoTail) = getZTestExpectedResults lowerP
    zTest trueMean trueStd sampleMean sampleSize LowerTailed |> lowerTail
    zTest trueMean trueStd sampleMean sampleSize UpperTailed |> upperTail
    zTest trueMean trueStd sampleMean sampleSize TwoTailed |> twoTail


[<Theory>]
[<InlineData(55.0, 10.0, "35.0, 45.0, 58.0, 44.0, 48.0", 0.05)>]
[<InlineData(35.0, 10.0, "35.0, 45.0, 58.0, 44.0, 48.0", 0.95)>]
[<InlineData(-35.0, 10.0, "-35.0, -45.0, -58.0, -44.0, -48.0", 0.05)>]
[<InlineData(-55.0, 10.0, "-35.0, -45.0, -58.0, -44.0, -48.0", 0.95)>]
let ``Z-Test for Array Should Work`` (trueMean, trueStd, sampleString: string, lowerP) =
    let sample = sampleString.Split(',') |> Array.map float
    let (lowerTail, upperTail, twoTail) = getZTestExpectedResults lowerP
    zTestForArray trueMean trueStd sample LowerTailed |> lowerTail
    zTestForArray trueMean trueStd sample UpperTailed |> upperTail
    zTestForArray trueMean trueStd sample TwoTailed |> twoTail

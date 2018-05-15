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

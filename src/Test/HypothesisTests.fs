module ``Statistical Hypothesis Tests``

open Xunit
open FsUnit.Xunit

open FsStats
open FsStats.Hypothesis


[<Fact>]
let ``Test Hypothesises of Integer Distribution`` () =
    let bd = new Binomial.Distribution(20, 18.0/38.0)
    testHypothesis bd 0.05 4 |> should be False
    testHypothesis bd 0.05 7 |> should be True

[<Fact>]
let ``Test Hypothesises of Real Distribution`` () =
    let nd = new Normal.Distribution(10.0, 3.0)
    testHypothesis nd 0.05 2.0 |> should be False
    testHypothesis nd 0.05 7.0 |> should be True

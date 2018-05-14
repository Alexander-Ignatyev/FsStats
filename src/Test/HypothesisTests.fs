module ``Statistical Hypothesis Tests``

open Xunit
open FsUnit.Xunit

open FsStats
open FsStats.Hypothesis


[<Fact>]
let ``Test Hypothesises of Binomial Distribution`` () =
    let bd = new Binomial.Distribution(20, 18.0/38.0)
    testHypothesis bd 0.05 4 |> should be False
    testHypothesis bd 0.05 7 |> should be True

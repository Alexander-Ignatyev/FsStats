module ``Binomial Distribution Tests``

open Xunit
open FsUnit.Xunit

open FsStats

[<Fact>]
let ``10 choose 3 equals 120`` () =
    BinomialDistribution.coefficient 10 3 |> should equal 120

[<Fact>]
let ``4 choose 2 equals 6`` () =
    BinomialDistribution.coefficient 4 2 |> should equal 6

[<Fact>]
let ``8 choose 5 equals 70`` () =
    BinomialDistribution.coefficient 8 4 |> should equal 70

[<Fact>]
let ``Probability of getting 3 heads after 6 tosses of loaded coin`` () =
    let d = BinomialDistribution.create 6 0.3
    BinomialDistribution.pmf d 3 |> should (equalWithin 1e-5) 0.18522

[<Fact>]
let ``PMF (3 10 0.7) equals  0.009001692`` () = 
    let d = BinomialDistribution.create 10 0.7
    BinomialDistribution.pmf d 3 |> should (equalWithin 1e-8) 0.00900169

[<Fact>]
let ``Binomial Distribution of 10 tosses of fair coin`` () =
    let rnd = new System.Random()
    let bd = BinomialDistribution.create 10 0.5
    BinomialDistribution.mean bd |> should (equalWithin 1e-10) 5
    BinomialDistribution.variance bd |> should (equalWithin 1e-10) 2.5
    BinomialDistribution.stddev bd |> should (equalWithin 1e-10) 1.58113883008
    BinomialDistribution.pmf bd 3 |> should (equalWithin 1e-10) 0.1171875
    BinomialDistribution.pmf bd 15 |> should (equalWithin 1e-10) 0.0
    BinomialDistribution.cdf bd 10 |> should (equalWithin 1e-10) 1.0
    BinomialDistribution.cdf bd 15 |> should (equalWithin 1e-10) 1.0
    BinomialDistribution.cdf bd 5 |> should (equalWithin 1e-10) 0.623046875
    BinomialDistribution.random bd rnd |> should be (lessThan 11)
    BinomialDistribution.sample bd rnd 20 |> should haveLength 20

[<Fact>]
let ``Binomial Distribution of 7 tosses of loaded coin`` () =
    let rnd = new System.Random()
    let bd = BinomialDistribution.create 7 0.3
    BinomialDistribution.mean bd |> should (equalWithin 1e-10) 2.1
    BinomialDistribution.variance bd |> should (equalWithin 1e-10) 1.47
    BinomialDistribution.stddev bd |> should (equalWithin 1e-10) 1.2124355653
    BinomialDistribution.pmf bd 3 |> should (equalWithin 1e-10) 0.2268945
    BinomialDistribution.pmf bd 10 |> should (equalWithin 1e-10) 0.0
    BinomialDistribution.cdf bd 7 |> should (equalWithin 1e-10) 1.0
    BinomialDistribution.cdf bd 3 |> should (equalWithin 1e-10) 0.873964
    BinomialDistribution.cdf bd 10 |> should (equalWithin 1e-10) 1.0
    BinomialDistribution.random bd rnd |> should be (lessThan 8)
    BinomialDistribution.sample bd rnd 20 |> should haveLength 20

[<Fact>]
let ``Probabily of winning in at least 4 out of 10 ten Roulette games`` () =
    let bd = BinomialDistribution.create 10 (18.0/38.0)
    Array.sumBy (BinomialDistribution.pmf bd) [|4..10|]
    |> should (equalWithin 1e-7) 0.7815551


[<Theory>]
[<InlineData(40, 0.3, true)>]
[<InlineData(20, 0.3, false)>]
[<InlineData(20, 0.7, false)>]
[<InlineData(20, 0.5, true)>]
let ``Is Normal Approximation Applicable`` (n, p, expected) =
    let d = BinomialDistribution.create n p
    BinomialDistribution.isNormalApproximationApplicable d |> should equal expected

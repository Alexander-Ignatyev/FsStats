module ``OOD Binomial Distribution Tests``

open Xunit
open FsUnit.Xunit

open FsStats.OOD

[<Fact>]
let ``Binomial Distribution of 10 tosses of fair coin`` () = 
    let bd = new BinomialDistribution(10, 0.5)
    bd.Mean |> should (equalWithin 1e-10) 5
    bd.Variance |> should (equalWithin 1e-10) 2.5
    bd.StdDev |> should (equalWithin 1e-10) 1.58113883008
    bd.Probability 3 |> should (equalWithin 1e-10) 0.1171875
    bd.CumulativeProbability (0, 10) |> should (equalWithin 1e-10) 1.0
    bd.CumulativeProbability (0, 5) |> should (equalWithin 1e-10) 0.623046875
    bd.CumulativeProbability 10 |> should (equalWithin 1e-10) 1.0
    bd.CumulativeProbability 5 |> should (equalWithin 1e-10) 0.623046875
    bd.Random |> should be (lessThan 11)
    bd.Sample 20 |> should haveLength 20

[<Fact>]
let ``Binomial Distribution of 7 tosses of loaded coin`` () = 
    let bd = new BinomialDistribution(7, 0.3)
    bd.Mean |> should (equalWithin 1e-10) 2.1
    bd.Variance |> should (equalWithin 1e-10) 1.47
    bd.StdDev |> should (equalWithin 1e-10) 1.2124355653
    bd.Probability 3 |> should (equalWithin 1e-10) 0.2268945
    bd.CumulativeProbability (0, 7) |> should (equalWithin 1e-10) 1.0
    bd.CumulativeProbability (0, 3) |> should (equalWithin 1e-10) 0.873964
    bd.CumulativeProbability 7 |> should (equalWithin 1e-10) 1.0
    bd.CumulativeProbability 3 |> should (equalWithin 1e-10) 0.873964
    bd.Random |> should be (lessThan 8)
    bd.Sample 20 |> should haveLength 20

[<Fact>]
let ``Probabily of winning in at least 4 out of 10 ten Roulette games`` () = 
    let bd = new BinomialDistribution(10, 18.0/38.0)
    Array.sumBy bd.Probability [|4..10|]
    |> should (equalWithin 1e-7) 0.7815551

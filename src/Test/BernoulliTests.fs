module ``Bernoiulli distribution tests``

open Xunit
open FsUnit.Xunit

open FsStats

[<Fact>]
let ``Bernoulli distribution of fair coin tosses`` () =
    let d = new BernoulliDistribution(0.5)
    d.Mean |> should (equalWithin 1e-10) 0.5
    d.Variance |> should (equalWithin 1e-10) 0.25
    d.StdDev |> should (equalWithin 1e-10) 0.5
    d.Sample |> should be ofExactType<int>
    d.Samples 20 |> should haveLength 20
    d.Probability 0 |> should (equalWithin 1e-10) (1.0 - 0.5)
    d.Probability 1 |> should (equalWithin 1e-10) 0.5
    d.Probability 2 |> should (equalWithin 1e-10) 0.0

[<Fact>]
let ``Bernoulli distribution of loaded coin tosses`` () =
    let d = new BernoulliDistribution(0.7)
    d.Mean |> should (equalWithin 1e-10) 0.7
    d.Variance |> should (equalWithin 1e-10) 0.21
    d.StdDev |> should (equalWithin 1e-10) 0.45825756949
    d.Sample |> should be ofExactType<int>
    d.Samples 20 |> should haveLength 20
    d.Probability 0 |> should (equalWithin 1e-10) (1.0 - 0.7)
    d.Probability 1 |> should (equalWithin 1e-10) 0.7
    d.Probability 2 |> should (equalWithin 1e-10) 0.0

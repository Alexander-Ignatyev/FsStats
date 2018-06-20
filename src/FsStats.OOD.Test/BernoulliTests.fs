module ``OOD Bernoiulli distribution tests``

open Xunit
open FsUnit.Xunit

open FsStats.OOD

[<Theory>]
[<InlineData(0.5, 0.25)>]
[<InlineData(0.7, 0.21)>]
[<InlineData(0.3, 0.21)>]
let ``Bernoulli distribution of fair coin tosses`` (p, var) =
    let d = new BernoulliDistribution(p)
    d.Mean |> should (equalWithin 1e-10) p
    d.Variance |> should (equalWithin 1e-10) var
    d.StdDev |> should (equalWithin 1e-10) (sqrt var)
    d.Random |> should be ofExactType<int>
    d.Sample 20 |> should haveLength 20
    d.Probability 0 |> should (equalWithin 1e-10) (1.0 - p)
    d.Probability 1 |> should (equalWithin 1e-10) p
    d.Probability 2 |> should (equalWithin 1e-10) 0.0

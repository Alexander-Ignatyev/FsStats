module ``Bernoiulli distribution tests``

open Xunit
open FsUnit.Xunit

open FsStats

[<Theory>]
[<InlineData(0.5, 0.25)>]
[<InlineData(0.7, 0.21)>]
[<InlineData(0.3, 0.21)>]
let ``Bernoulli distribution of fair coin tosses`` (p, var) =
    let rnd = new System.Random()
    let d = BernoulliDistribution.create p
    BernoulliDistribution.mean d |> should (equalWithin 1e-10) p
    BernoulliDistribution.variance d |> should (equalWithin 1e-10) var
    BernoulliDistribution.stddev d |> should (equalWithin 1e-10) (sqrt var)

    BernoulliDistribution.sample d rnd |> should be ofExactType<int>
    BernoulliDistribution.samples d rnd 20 |> should haveLength 20

    BernoulliDistribution.pmf d 0 |> should (equalWithin 1e-10) (1.0 - p)
    BernoulliDistribution.pmf d 1 |> should (equalWithin 1e-10) p
    BernoulliDistribution.pmf d 2 |> should (equalWithin 1e-10) 0.0

    BernoulliDistribution.cdf d -1 |> should (equalWithin 1e-10) 0.0
    BernoulliDistribution.cdf d 0 |> should (equalWithin 1e-10) (1.0 - p)
    BernoulliDistribution.cdf d 1 |> should (equalWithin 1e-10) 1.0

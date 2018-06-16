module ``Poisson Distribution tests``

open Xunit
open FsUnit.Xunit

open FsStats

[<Fact>]
let ``Poisson Distribution`` () = 
    let rnd = new System.Random()
    let pd = PoissonDistribution.create 7.1
    PoissonDistribution.mean pd |> should (equalWithin 1e-10) 7.1
    PoissonDistribution.variance pd |> should (equalWithin 1e-10) 7.1
    PoissonDistribution.stddev pd |> should (equalWithin 1e-10) 2.66458251889
    PoissonDistribution.pmf pd 10 |> should (equalWithin 1e-5) 0.07402
    PoissonDistribution.pmf pd 7 |> should (equalWithin 1e-5) 0.14890
    PoissonDistribution.pmf pd 19 |> should (equalWithin 1e-5) 0.00010
    PoissonDistribution.cdf pd 10 |> should (equalWithin 1e-5) 0.89423
    PoissonDistribution.cdf pd 7 |> should (equalWithin 1e-5) 0.58382
    PoissonDistribution.random pd rnd |> should be ofExactType<int>
    PoissonDistribution.sample pd rnd 20 |> should haveLength 20


[<Fact>]
let ``Average of randomly generated values should go to mean value`` () =
    let rnd = new System.Random()
    let mu = 7.1
    let pd = PoissonDistribution.create mu
    PoissonDistribution.sample pd rnd 100
    |> Array.averageBy float
    |> should (equalWithin 1.0) mu


[<Fact>]
let ``Poisson's pmd and cdf should be correctly calculated for big k``() =
    let pd = PoissonDistribution.create 41.0
    PoissonDistribution.pmf pd 41 |> should (equalWithin 1e-5) 0.06218
    PoissonDistribution.pmf pd 54 |> should (equalWithin 1e-5) 0.00834
    PoissonDistribution.cdf pd 41 |> should (equalWithin 1e-5) 0.54141
    PoissonDistribution.cdf pd 54 |> should (equalWithin 1e-5) 0.97887

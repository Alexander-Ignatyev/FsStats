module ``Poisson Distribution tests``

open Xunit
open FsUnit.Xunit

open FsStats.OOD

[<Fact>]
let ``Poisson Distribution`` () = 
    let bd = new PoissonDistribution(7.1)
    bd.Mean |> should (equalWithin 1e-10) 7.1
    bd.Variance |> should (equalWithin 1e-10) 7.1
    bd.StdDev |> should (equalWithin 1e-10) 2.66458251889
    bd.Probability 10 |> should (equalWithin 1e-5) 0.07402
    bd.Probability 7 |> should (equalWithin 1e-5) 0.14890
    bd.CumulativeProbability 10 |> should (equalWithin 1e-5) 0.89423
    bd.CumulativeProbability 7 |> should (equalWithin 1e-5) 0.58382
    bd.Sample |> should be ofExactType<int>
    bd.Samples 20 |> should haveLength 20


[<Fact>]
let ``Average of randomly generated values should go to mean value`` () = 
    let mu = 7.1
    let bd = new PoissonDistribution(mu)
    bd.Samples 100
    |> Array.averageBy (fun e -> float e)
    |> should (equalWithin 1.0) mu

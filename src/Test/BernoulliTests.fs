module ``Bernoiulli distribution tests``

open Xunit
open FsUnit.Xunit

open FsStats.Bernoulli

[<Fact>]
let ``Bernoulli distribution of fair coin tosses`` () =
    let d = new Distribution(0.5)
    d.Mean |> should (equalWithin 1e-10) 0.5
    d.Variance |> should (equalWithin 1e-10) 0.25
    d.StdDev |> should (equalWithin 1e-10) 0.5
    d.Sample |> should be ofExactType<bool>
    d.Samples 20 |> should haveLength 20

[<Fact>]
let ``Bernoulli distribution of loaded coin tosses`` () =
    let d = new Distribution(0.7)
    d.Mean |> should (equalWithin 1e-10) 0.7
    d.Variance |> should (equalWithin 1e-10) 0.21
    d.StdDev |> should (equalWithin 1e-10) 0.45825756949
    d.Sample |> should be ofExactType<bool>
    d.Samples 20 |> should haveLength 20
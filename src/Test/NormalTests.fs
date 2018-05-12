module ``Normal Distribution tests``

open Xunit
open FsUnit.Xunit

open FsStats.Normal

[<Fact>]
let ``Normal distribution`` () =
    let mu, sigma = 10.0, 2.0
    let nd = new Distribution(mu, sigma)
    nd.Mean |> should (equalWithin 1e-5) mu
    nd.Variance |> should (equalWithin 1e-5) (sigma*sigma)
    nd.StdDev |> should (equalWithin 1e-5) sigma
    nd.Probability mu |> should (equalWithin 1e-5) 0.5
    nd.Probability 100.0 |> should (equalWithin 1e-5) 1.0
    nd.Probability (mu - sigma) |> should (equalWithin 1e-4) (0.5 - 0.6827*0.5)
    nd.Probability (mu + sigma) |> should (equalWithin 1e-4) (0.5 + 0.6827*0.5)
    nd.Sample |> should be ofExactType<float>
    nd.Samples 20 |> should haveLength 20

[<Theory>]
[<InlineData(0.0, 1.0)>]
[<InlineData(10.0, 2.0)>]
let ``Normal distribution confidence intervals`` (mu, sigma) =
    let nd = new Distribution(mu, sigma)
    let interval x = (nd.Probability (mu + x)) - (nd.Probability (mu - x))
    interval sigma |> should (equalWithin 1e-4) 0.6827
    interval (1.96*sigma) |> should (equalWithin 1e-4) 0.95
    interval (2.0*sigma) |> should (equalWithin 1e-4) 0.9545
    interval (3.0*sigma) |> should (equalWithin 1e-4) 0.9973

[<Fact>]
let ``Mean of generated random values should be close to the distribution's mean`` () =
    let mu, sigma = 11.0, 3.0
    let nd = new Distribution(mu, sigma)
    nd.Samples 1000 |> Array.average |> should (equalWithin <| 0.1*sigma) mu

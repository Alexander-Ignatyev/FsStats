module ``Binomial Distribution Tests``

open Xunit
open FsUnit.Xunit

open FsStats.Binomial

[<Fact>]
let ``10 choose 3 equals 120`` () =
    coefficient 10 3 |> should equal 120

[<Fact>]
let ``4 choose 2 equals 6`` () =
    coefficient 4 2 |> should equal 6

[<Fact>]
let ``8 choose 5 equals 70`` () =
    coefficient 8 4 |> should equal 70

[<Fact>]
let ``Probability of getting 3 heads after 6 tosses of loaded coin`` () =
    pmf 3 6 0.3 |> should (equalWithin 1e-5) 0.18522

[<Fact>]
let ``PMF (3 10 0.7) equals  0.009001692`` () = 
    pmf 3 10 0.7 |> should (equalWithin 1e-8) 0.00900169

[<Fact>]
let ``Binomial Distribution of 10 tosses of fair coin`` () = 
    let bd = new Distribution(10, 0.5)
    bd.Mean |> should (equalWithin 1e-10) 5
    bd.Variance |> should (equalWithin 1e-10) 2.5
    bd.StdDev |> should (equalWithin 1e-10) 1.58113883008
    bd.Probability 3 |> should (equalWithin 1e-10) 0.1171875
    bd.Sample |> should be (lessThan 11)
    bd.Samples 20 |> should haveLength 20

[<Fact>]
let ``Binomial Distribution of 7 tosses of loaded coin`` () = 
    let bd = new Distribution(7, 0.3)
    bd.Mean |> should (equalWithin 1e-10) 2.1
    bd.Variance |> should (equalWithin 1e-10) 1.47
    bd.StdDev |> should (equalWithin 1e-10) 1.2124355653
    bd.Probability 3 |> should (equalWithin 1e-10) 0.2268945
    bd.Sample |> should be (lessThan 8)
    bd.Samples 20 |> should haveLength 20


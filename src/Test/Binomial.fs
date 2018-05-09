module Fsharp.Statistics.Binomial.Tests

open Xunit

open Fsharp.Statistics.Binomial


[<Fact>]
let ``10 choose 3 equals 120`` () =
    Assert.Equal(120, (coefficient 10 3))

[<Fact>]
let ``4 choose 2 equals 6`` () =
    Assert.Equal(6, coefficient 4 2)

[<Fact>]
let ``8 choose 5 equals 70`` () =
    Assert.Equal(70, coefficient 8 4)
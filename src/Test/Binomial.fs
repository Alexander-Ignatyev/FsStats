module Fsharp.Statistics.Binomial.Tests

open Xunit
open FsUnit.Xunit

open Fsharp.Statistics.Binomial


[<Fact>]
let ``10 choose 3 equals 120`` () =
    coefficient 10 3 |> should equal 120

[<Fact>]
let ``4 choose 2 equals 6`` () =
    coefficient 4 2 |> should equal 6

[<Fact>]
let ``8 choose 5 equals 70`` () =
    coefficient 8 4 |> should equal 70
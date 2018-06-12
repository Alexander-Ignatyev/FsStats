module ``Student's t-distribution tests``

open Xunit
open FsUnit.Xunit

open FsStats


[<Fact>]
let ``Student's t-distribution`` () =
    (fun() -> StudentsDistribution.create 0 |> ignore) |> should throw typeof<System.ArgumentException>

    let d = StudentsDistribution.create 11
    StudentsDistribution.mean d |> should (equalWithin 1e-5) 0.0
    StudentsDistribution.median d |> should (equalWithin 1e-5) 0.0
    StudentsDistribution.mode d |> should (equalWithin 1e-5) 0.0
    StudentsDistribution.variance d |> should (equalWithin 1e-5) 1.25

    StudentsDistribution.variance (StudentsDistribution.create 2) |> should be NaN
    StudentsDistribution.variance (StudentsDistribution.create 3) |> should equal System.Double.PositiveInfinity


[<Theory>]
[<InlineData(21, 0.0, 0.39399)>]
[<InlineData(21, 0.7, 0.30557)>]
[<InlineData(16, 0.33, 0.37029)>]
[<InlineData(18, 1.0, 0.23503)>]
[<InlineData(18, -0.1, 0.39105)>]
[<InlineData(22, -0.7, 0.30588)>]
let ``Probability density function`` (sampleSize, x, expected) =
    let d = StudentsDistribution.create sampleSize
    StudentsDistribution.pdf d x |> should (equalWithin 1e-5) expected


[<Theory>]
[<InlineData(21, 0.0, 0.5)>]
[<InlineData(21, 0.7, 0.7540)>]
[<InlineData(16, 0.33, 0.62702)>]
[<InlineData(18, 1.0, 0.83433)>]
[<InlineData(18, -0.1, 0.46076)>]
[<InlineData(22, -0.7, 0.2458)>]
let ``cumulative distribution function`` (sampleSize, t, expected) =
    let d = StudentsDistribution.create sampleSize
    StudentsDistribution.cdf d t |> should (equalWithin 1e-5) expected

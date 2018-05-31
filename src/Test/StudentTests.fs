module ``Student's t-distribution tests``

open Xunit
open FsUnit.Xunit

open FsStats

[<Theory>]
[<InlineData(20, 0.0, 0.5)>]
[<InlineData(20, 0.7, 0.7540)>]
[<InlineData(15, 0.33, 0.6270)>]
[<InlineData(17, 1.0, 0.8343)>]
[<InlineData(17, -0.1, 0.4608)>]
[<InlineData(21, -0.7, 0.2458)>]
let ``cumulative distribution function`` (df, t, expected) =
    Student.cdf df t |> should (equalWithin 1e-4) expected

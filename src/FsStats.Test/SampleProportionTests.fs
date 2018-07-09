module ``Sample Proportion tests``

open Xunit
open FsUnit.Xunit

open FsStats

[<Theory>]
[<InlineData(-0.5, 100)>]
[<InlineData(0.0, 100)>]
[<InlineData(1.0, 100)>]
[<InlineData(1.5, 100)>]
[<InlineData(0.5, 0)>]
[<InlineData(0.5, -1)>]
let ``SampleProportion.create must fail is incorrect data passed``(p, n) =
    (fun() -> SampleProportion.create p n |> ignore) |> should throw typeof<System.ArgumentException>

[<Theory>]
[<InlineData(0.4, 100, 0.0489897949)>]
[<InlineData(0.25, 1000, 0.0136930639)>]
let ``Sample Proportion mean and standard error`` (p, n, se) =
    let sp = SampleProportion.create p n
    SampleProportion.mean sp |> should (equalWithin 1e-5) p
    SampleProportion.stderr sp |> should (equalWithin 1e-5) se


[<Theory>]
[<InlineData(0.3, 20, true)>]
[<InlineData(0.3, 10, false)>]
[<InlineData(0.7, 10, false)>]
[<InlineData(0.5, 10, true)>]
let ``Is Normal Approximation Applicable`` (p, n, expected) =
    let sp = SampleProportion.create p n
    SampleProportion.isNormalApproximationApplicable sp |> should equal expected


[<Theory>]
[<InlineData(0.25, 1000, 0.95, 0.02683)>]
let ``Margin of Error`` (p, n, level, me) =
    let sp = SampleProportion.create p n
    SampleProportion.marginOfError sp level |> should (equalWithin 1e-5) me

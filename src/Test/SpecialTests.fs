module ``Special Functions tests``

open Xunit
open FsUnit.Xunit

open FsStats.Special


[<Fact>]
let ``Gauss error function`` () = 
    let eps = 1e-5
    erf 0.0 |> should (equalWithin eps) 0.0
    erf 0.1 |> should (equalWithin eps) 0.1124629
    erf 0.5 |> should (equalWithin eps) 0.5204999
    erf 1.0 |> should (equalWithin eps) 0.8427008
    erf 3.0 |> should (equalWithin eps) 0.9999779
    erf -0.1 |> should (equalWithin eps) -0.1124629
    erf -0.5 |> should (equalWithin eps) -0.5204999
    erf -1.0 |> should (equalWithin eps) -0.8427008
    erf -3.0 |> should (equalWithin eps) -0.9999779

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


[<Fact>]
let ``Lanczos Approximation to the Gamma Function`` () =
    let eps = 1e-14
    gammaLanczos -7.5 |> should (equalWithin eps) 0.000223849328859689
    gammaLanczos -0.5 |> should (equalWithin eps) -3.54490770181103
    gammaLanczos 0.1 |> should (equalWithin eps) 9.51350769866874
    gammaLanczos 0.5 |> should (equalWithin eps) 1.772453850905517
    gammaLanczos 1.0 |> should (equalWithin eps) 1.0
    gammaLanczos 1.5 |> should (equalWithin eps) 0.8862269254527587
    gammaLanczos 10.0 |> should (equalWithin eps) 362880.000000001  
    

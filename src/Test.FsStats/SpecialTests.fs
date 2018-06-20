module ``Special Functions tests``

open Xunit
open FsUnit.Xunit

open FsStats.Special


[<Theory>]
[<InlineData(0.0, 0.0)>]
[<InlineData(0.1, 0.1124629)>]
[<InlineData(0.5, 0.5204999)>]
[<InlineData(1.0, 0.8427008)>]
[<InlineData(3.0, 0.9999779)>]
[<InlineData(-0.1, -0.1124629)>]
[<InlineData(-0.5, -0.5204999)>]
[<InlineData(-1.0, -0.8427008)>]
[<InlineData(-3.0, -0.9999779)>]
let ``Gauss error function`` (x, expected) = 
    let eps = 1e-5
    erf x |> should (equalWithin eps) expected

[<Theory>]
[<InlineData(0.0, 0.0)>]
[<InlineData(0.99, 1.82138637 )>]
[<InlineData(0.5, 0.4769363)>]
[<InlineData(0.1, 0.088856 )>]
[<InlineData(-0.99, -1.82138637 )>]
[<InlineData(-0.5, -0.4769363)>]
[<InlineData(-0.1, -0.088856 )>]
let ``Inverse error function`` (x, expected) = 
    let eps = 0.0035
    erfinv x |> should (equalWithin eps) expected


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
    

[<Theory>]
[<InlineData(5.0, 15.0, 0.3, 5.35066)>]
[<InlineData(5.0, 15.0, 0.7, 35659.84786)>]
[<InlineData(5.0, -15.0, 0.9, 0.35891)>]
let ``helper function contfractbeta should work`` (a, b, x, expected) =
    BetaHelper.contfractbeta a b x 1000 1e-5 |> should (equalWithin 1e-5) expected


[<Theory>]
[<InlineData(112.0, 415.03231)>]
[<InlineData(-23.56, -52.21866)>]
let ``lgamma should work as expected`` (x, expected) =
    gammaln x |> should (equalWithin 1e-5) expected


[<Theory>]
[<InlineData(5.0, 30.0, 7.18762578345123E-07)>]
[<InlineData(25.0, 35.0,  1.32082924168603e-18)>]
[<InlineData(10.0, 30.0, 1.57295673125096e-10)>]
let ``beta should work as expected`` (a, b, expected) =
    beta a b |> should (equalWithin 1e-25) expected


[<Theory>]
[<InlineData(5.0, 30.0, 0.0, 0.0)>]
[<InlineData(15.0, 3.0, 1.0, 1.0)>]
[<InlineData(25.0, 35.0, 0.3, 0.02929)>]
[<InlineData(10.0, 30.0, 0.3, 0.77587)>]
let ``betainc should work as expected`` (a, b, x, expected) =
    betainc a b x |> should (equalWithin 1e-5) expected


[<Fact>]
let ``Properties of betainc function`` () =
    betainc 10.0 10.0 0.0 |> should (equalWithin 1e-5) 0
    betainc 10.0 11.0 1.0 |> should (equalWithin 1e-5) 1.0
    betainc 10.0 1.0 0.7 |> should (equalWithin 1e-15) (0.7 ** 10.0)
    betainc 10.0 3.0 0.7 |> should (equalWithin 1e-15) (1.0 - betainc 3.0 10.0 (1.0 - 0.7))
    let a, b, x = 11.0, 12.0, 0.8
    let iabx = betainc a b x
    let n = x ** a * (1.0 - x) ** b / (beta a b)
    betainc (a + 1.0) b x |> should (equalWithin 1e-10) (iabx - n/a)
    betainc a (b + 1.0) x |> should (equalWithin 1e-10) (iabx + n/b)

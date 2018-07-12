module ``Bivariate Analysis tests``

open Xunit
open FsUnit.Xunit

open FsStats.BivariateAnalysis

[<Theory>]
[<InlineData(2.0, 3.0)>]
[<InlineData(-2.0, 5.0)>]
let ``Ideal case should give ideal results`` (m, b) =
    let x = create [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
    let y = Array.map (fun x -> x * m + b) x.Data |> create
    correlation x y |> should (equalWithin 1e-5) (if m > 0.0 then 1.0 else -1.0)
    let line = linearRegression x y
    line.Slope |> should (equalWithin 1e-5) m
    line.YIntercept |> should (equalWithin 1e-5) b


[<Fact>]
let ``Temperature and ice cream sales should have strong positive correlation`` () =
    let temperature = create [|14.2; 16.4; 11.9; 15.2; 18.5; 22.1; 19.4; 25.1; 23.4; 18.1; 22.6; 17.2|]
    let iceCreamSales = create [|215.0; 325.0; 185.0; 332.0; 406.0; 522.0; 412.0; 614.0; 544.0; 421.0; 445.0; 408.0|]
    covariance temperature iceCreamSales |> should (equalWithin 1e-5) 484.09318
    correlation temperature iceCreamSales |> should (equalWithin 1e-5) 0.95751
    let line = linearRegression temperature iceCreamSales
    line.Slope |> should (equalWithin 1e-5) 30.08786
    line.YIntercept |> should (equalWithin 1e-5) -159.47415
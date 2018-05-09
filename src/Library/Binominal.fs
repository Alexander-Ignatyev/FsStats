module Fsharp.Statistics.Binomial

/// Binomial Coefficient n choose k
let coefficient n k = 
    let k = max k (n-k)
    let a = Array.fold (*) 1 [|k+1 .. n|]
    let b = Array.fold (*) 1 [|1 .. n-k|]
    in a / b

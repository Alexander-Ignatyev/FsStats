namespace  FsStats

open System

module Special =
    /// Gauss error function
    /// maximum approximation error: 1.5×10−7
    /// https://en.wikipedia.org/wiki/Error_function#Approximation_with_elementary_functions
    let erf x = 
        let sign = if x < 0.0 then -1.0 else 1.0
        let x = abs x
        let a1 =  0.254829592
        let a2 = -0.284496736
        let a3 =  1.421413741
        let a4 = -1.453152027
        let a5 =  1.061405429
        let p  =  0.3275911

        let t = 1.0/(1.0 + p*x)
        let y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*exp(-x*x)
        sign*y


    //// Lanczos approximation to the Gamma function
    let rec gammaLanczos z =
        if z < 0.5
        then 
            Math.PI / ((Math.Sin (Math.PI*z)) * (gammaLanczos (1.0 - z)))
        else 
            let coefficient0 = 0.99999999999980993
            let coefficients = [|676.5203681218851; -1259.1392167224028; 
                                 771.32342877765313; -176.61502916214059; 
                                 12.507343278686905; -0.13857109526572012; 
                                 9.9843695780195716e-6; 1.5056327351493116e-7|]
            let t = z - 1.5 + (Array.length coefficients |> float)
            let x = coefficients
                    |> Array.mapi (fun i v -> v / (z + float i))
                    |> Array.sum 
            Math.Sqrt (2.0 * Math.PI) * t ** (z - 0.5) * Math.Exp (-t) * (x + coefficient0)



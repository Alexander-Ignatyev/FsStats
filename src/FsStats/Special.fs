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


    /// Logarithm of the absolute value of the Gamma function.
    let gammaln x = x |> gammaLanczos |> abs |> Math.Log

    module BetaHelper = 
        let contfractbeta a b x itermax eps =
            let qab = a + b
            let qap = a + 1.0
            let qam = a - 1.0
            let bz = 1.0 - qab * x / qap
            let rec helper am bm az bz iter =
                let em = iter + 1 |> float
                let tem = em + em
                let d = em * (b - em) * x / ((qam + tem) * (a + tem))
                let ap = az + d * am
                let bp = bz + d * bm
                let dp = -(a + em) * (qab + em) * x / ((qap + tem) * (a + tem))
                let app = ap + dp * az
                let bpp = bp + dp * bz
                let am' = ap / bpp
                let bm' = bp / bpp
                let az' = app / bpp
                let bz' = 1.0
                if abs (az' - az) < (eps * abs(az)) || iter = itermax
                then az'
                else helper am' bm' az' bz' (iter + 1)
            helper 1.0 1.0 1.0 bz 0


    /// Beta function
    let beta a b = 
        gammaLanczos a * gammaLanczos b / gammaLanczos (a + b)


    /// Regularized Incomlete Beta function
    // https://malishoaib.wordpress.com/2014/04/15/the-beautiful-beta-functions-in-raw-python/
    // Numerical Recipes in C.
    let betainc a b = function
        | 0.0 -> 0.0
        | 1.0 -> 1.0
        | x ->
            let contfractbeta a b x = BetaHelper.contfractbeta a b x 1000 1e-7
            let lbeta = gammaln (a + b) - gammaln a - gammaln b + a * Math.Log x + b * Math.Log (1.0 - x)
            if x < (a + 1.0) / (a + b + 2.0)
            then Math.Exp lbeta * contfractbeta a b x / a
            else 1.0 - Math.Exp lbeta * contfractbeta b a (1.0 - x) / b


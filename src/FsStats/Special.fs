namespace  FsStats


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

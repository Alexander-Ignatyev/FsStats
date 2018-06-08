namespace FsStats


module SummaryStatistics =

    type T = { Mean : float; StdDev : float; Data : float [] }

    let create data = 
        let mean = Array.average data
        let variance =
            let sampleLength = (Array.length data) - 1 
            let s = Array.sumBy (fun x -> (x - mean) ** 2.0) data
            LanguagePrimitives.DivideByInt s sampleLength
        { Mean = mean; StdDev = sqrt variance; Data = data }

    let mean { Mean = mu } =
        mu

    let stdDev { StdDev = std } =
        std

    /// Calculates sample skewness,
    /// a measure of the asymmetry
    let skewness { Mean = mean; StdDev = sd; Data = data } =
        let n =(Array.length data) - 1
        let m = Array.sumBy (fun x -> (x - mean) ** 3.0) data
        (LanguagePrimitives.DivideByInt m n) / (sd ** 3.0)

    /// Calculates sample kurtosis,
    /// a measure of the "tailedness"
    let kurtosis { Mean = mean; StdDev = sd; Data = data } =
        let n = (Array.length data) - 1
        let m = Array.sumBy (fun x -> (x - mean) ** 4.0) data
        (LanguagePrimitives.DivideByInt m n) / (sd ** 4.0)
 
    /// Five-number summary
    /// (sample minimum, first quartile, median, third quartile, sample maximum)
    let fivenum {  Data = data } =
        let median (d : float []) start finish =
            let n = finish - start
            if n % 2 = 0
            then (d.[start + n / 2 - 1] + d.[start + n / 2]) * 0.5
            else d.[start + n / 2]

        let sorted = Array.sort data
        let n = Array.length sorted
        let q1_value = median sorted 0 (n/2)
        let median_value = median sorted 0 n
        let q3_value = median sorted (n - n/2) n
        (sorted.[0], q1_value, median_value, q3_value, sorted.[n - 1])

    /// Sample Correlation
    let correlation x y =
        let sampleLength = (Array.length x.Data) - 1
        let cov = Array.zip x.Data y.Data
                  |> Array.sumBy (fun (xp, yp) -> (xp - x.Mean) * (yp - y.Mean))
                  |> fun s -> LanguagePrimitives.DivideByInt s sampleLength
        cov / (x.StdDev * y.StdDev)

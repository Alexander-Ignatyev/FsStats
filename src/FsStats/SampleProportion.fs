namespace FsStats

open LanguagePrimitives

module SampleProportion =

    type T = { Proportion : float; SampleSize : int }

    let create proportion size = 
        if proportion <= 0.0 || proportion >= 1.0 then invalidArg "propertion" "Value must me between 0.0 and 1.0"
        else if size < 1 then invalidArg "size" "Value must be positive integer (> 0)"
        else { Proportion = proportion; SampleSize = size }

    let mean { Proportion = p } = p

    let stderr { Proportion = p; SampleSize = n } =
        DivideByInt (p * (1.0 - p)) n |> sqrt

    let isNormalApproximationApplicable { Proportion = p; SampleSize = n } =
        let boundary = 5.0
        p * float n >= boundary && (1.0 - p) * float n >= boundary

    let marginOfError sp confidenceLevel =
        let se = stderr sp
        let zValue = StandardDistribution.zValue confidenceLevel
        zValue * se

    /// Calculates confidence interval for the population proportion
    /// with given confidence level
    let confidenceInterval sp level =
        let moe = marginOfError sp level
        (sp.Proportion - moe, sp.Proportion + moe)

    /// the difference of two proportions
    module Two =
        let stderr sp1 sp2 =
            let se2 { Proportion = p; SampleSize = n } = DivideByInt (p * (1.0 - p)) n
            sqrt (se2 sp1 + se2 sp2)

        /// Calculate Margin of Error for the difference of two proportions
        /// with given confidence level
        let marginOfError sp1 sp2 level =
            let se =  stderr sp1 sp2
            let zValue = StandardDistribution.zValue level
            zValue * se

        /// Calculate Confidence Level for the difference of two proportions
        /// with given confidence level
        let confidenceInterval s1 s2 level =
            let moe = marginOfError s1 s2 level
            let diff = mean s1 - mean s2
            (diff - moe, diff + moe)

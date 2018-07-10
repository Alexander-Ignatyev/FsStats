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

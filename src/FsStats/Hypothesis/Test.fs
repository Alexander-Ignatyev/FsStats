namespace FsStats.Hypothesis

type TestType =
    | LowerTailed
    | UpperTailed
    | TwoTailed

module Test =
     let perform cdf testType score =
        let p = cdf -(abs score)
        let pValue = match testType with
                     | LowerTailed -> if score < 0.0 then p else 1.0 - p
                     | UpperTailed -> if score < 0.0 then 1.0 - p else p
                     | TwoTailed -> 2.0 * p
        pValue

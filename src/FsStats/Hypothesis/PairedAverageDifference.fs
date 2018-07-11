namespace FsStats.Hypothesis

open FsStats


/// Paired Average Difference Test
module PairedAverageDifference =
    open OnePopulationMean

    let create sample1 sample2 =
        if Array.length sample1 <> Array.length sample2 then invalidArg "sample1, sample2" "both samples must have the same length"
        let diff = Array.map2 (-) sample1 sample2
        let ss = SummaryStatistics.create diff
        {
            PopulationMean = 0.0
            SampleMean = SummaryStatistics.mean ss
            StdDev = SummaryStatistics.stddev ss
            SampleSize = SummaryStatistics.size ss
        }

    let score = OnePopulationMean.score

    let zTest = OnePopulationMean.zTest

    let tTest = OnePopulationMean.tTest

module ``Statistical Hypothesis Tests``

#nowarn "44"  // Hypothesis.OneSample is depricated but still requires to be tested

open Xunit
open FsUnit.Xunit

open FsStats
open FsStats.Hypothesis

let getTestExpectedResults eps lowerP =
    let cond = should (equalWithin eps)
    let twoTailed = if lowerP < 0.5 then 2.0 * lowerP else 2.0 * (1.0 - lowerP)
    (cond lowerP, cond (1.0 - lowerP), cond twoTailed)


module ``One Population Mean`` =
    open OnePopulationMean

    [<Theory>]
    [<InlineData(55.0, 10.0, 47, 20, 0.00017)>]
    [<InlineData(-55.0, 10.0, -47, 20, 0.99983)>]
    [<InlineData(35.0, 10.0, 47, 20, 1.0)>]
    [<InlineData(-35.0, 10.0, -47, 20, 0.0)>]
    let ``Z-Test Should Work`` (trueMean, trueStd, sampleMean, sampleSize, lowerP) =
        let (lowerTail, upperTail, twoTail) = getTestExpectedResults 1e-5 lowerP
        let opm = {
            PopulationMean = trueMean
            SampleMean = sampleMean
            SampleSize = sampleSize
            StdDev = trueStd
        }
        zTest opm LowerTailed |> lowerTail
        zTest opm UpperTailed |> upperTail
        zTest opm TwoTailed |> twoTail


    [<Theory>]
    [<InlineData(55.0, 10.0, "35.0, 45.0, 58.0, 44.0, 48.0", 0.02209)>]
    [<InlineData(-55.0, 10.0, "-35.0, -45.0, -58.0, -44.0, -48.0", 0.97791)>]
    [<InlineData(35.0, 10.0, "35.0, 45.0, 58.0, 44.0, 48.0",  0.99305)>]
    [<InlineData(-35.0, 10.0, "-35.0, -45.0, -58.0, -44.0, -48.0", 0.00695)>]
    let ``Z-Test for Sample Should Work`` (trueMean, trueStd, sampleString: string, lowerP) =
        let sample = sampleString.Split(',') |> Array.map float
        let (lowerTail, upperTail, twoTail) = getTestExpectedResults 1e-5 lowerP
        let summary = SummaryStatistics.create sample
        let sampleMean = SummaryStatistics.mean summary
        let sampleSize = Array.length sample
        let opm = {
            PopulationMean = trueMean
            SampleMean = sampleMean
            SampleSize = sampleSize
            StdDev = trueStd
        }
        zTest opm LowerTailed |> lowerTail
        zTest opm UpperTailed |> upperTail
        zTest opm TwoTailed |> twoTail


    [<Theory>]
    [<InlineData(55.0, "35.0, 45.0, 58.0, 44.0, 48.0", 0.03593)>]
    [<InlineData(-55.0, "-35.0, -45.0, -58.0, -44.0, -48.0", 0.96407)>]
    [<InlineData(35.0, "35.0, 45.0, 58.0, 44.0, 48.0", 0.97947)>]
    [<InlineData(-35.0, "-35.0, -45.0, -58.0, -44.0, -48.0", 0.02053)>]
    let ``T-Test for Sample Should Work`` (trueMean, sampleString: string, lowerP) =
        let sample = sampleString.Split(',') |> Array.map float
        let (lowerTail, upperTail, twoTail) = getTestExpectedResults 1e-5 lowerP
        let summary = SummaryStatistics.create sample
        let sampleMean = SummaryStatistics.mean summary
        let sampleStd = SummaryStatistics.stddev summary
        let sampleSize = Array.length sample
        let opm = {
            PopulationMean = trueMean
            SampleMean = sampleMean
            SampleSize = sampleSize
            StdDev = sampleStd
        }
        tTest opm LowerTailed |> lowerTail
        tTest opm UpperTailed |> upperTail
        tTest opm TwoTailed |> twoTail


    [<Theory>]
    [<InlineData(55.0, 10.0, 47, 20, -3.57771)>]
    [<InlineData(-55.0, 10.0, -47, 20, 3.57771)>]
    [<InlineData(35.0, 10.0, 47, 20, 5.36656)>]
    [<InlineData(-35.0, 10.0, -47, 20, -5.36656)>]
    let ``score Should Work`` (trueMean, trueStd, sampleMean, sampleSize, expected) =
        let opm = {
            PopulationMean = trueMean
            SampleMean = sampleMean
            SampleSize = sampleSize
            StdDev = trueStd
        }
        score opm |> should (equalWithin 1e-5) expected


module ``One Sample Tests`` =
    let getZTestExpectedResults lowerP =
        if lowerP < 0.5 then
            (should lessThan lowerP, should greaterThan (1.0 - lowerP), should lessThan (2.0 * lowerP))
        else
            (should greaterThan lowerP, should lessThan (1.0 - lowerP), should lessThan (2.0 * (1.0 - lowerP)))


    [<Theory>]
    [<InlineData(55.0, 10.0, 47, 20, 0.05)>]
    [<InlineData(35.0, 10.0, 47, 20, 0.95)>]
    [<InlineData(-35.0, 10.0, -47, 20, 0.05)>]
    [<InlineData(-55.0, 10.0, -47, 20, 0.95)>]
    let ``One-sample Z-Test Should Work`` (trueMean, trueStd, sampleMean, sampleSize, lowerP) =
        let (lowerTail, upperTail, twoTail) = getZTestExpectedResults lowerP
        OneSample.zTest trueMean trueStd sampleMean sampleSize LowerTailed |> lowerTail
        OneSample.zTest trueMean trueStd sampleMean sampleSize UpperTailed |> upperTail
        OneSample.zTest trueMean trueStd sampleMean sampleSize TwoTailed |> twoTail


    [<Theory>]
    [<InlineData(55.0, 10.0, "35.0, 45.0, 58.0, 44.0, 48.0", 0.05)>]
    [<InlineData(35.0, 10.0, "35.0, 45.0, 58.0, 44.0, 48.0", 0.95)>]
    [<InlineData(-35.0, 10.0, "-35.0, -45.0, -58.0, -44.0, -48.0", 0.05)>]
    [<InlineData(-55.0, 10.0, "-35.0, -45.0, -58.0, -44.0, -48.0", 0.95)>]
    let ``One-sample Z-Test for Sample Should Work`` (trueMean, trueStd, sampleString: string, lowerP) =
        let sample = sampleString.Split(',') |> Array.map float
        let (lowerTail, upperTail, twoTail) = getZTestExpectedResults lowerP
        let summary = SummaryStatistics.create sample
        let sampleMean = SummaryStatistics.mean summary
        let sampleSize = Array.length sample
        OneSample.zTest trueMean trueStd sampleMean sampleSize LowerTailed |> lowerTail
        OneSample.zTest trueMean trueStd sampleMean sampleSize UpperTailed |> upperTail
        OneSample.zTest trueMean trueStd sampleMean sampleSize TwoTailed |> twoTail


    [<Theory>]
    [<InlineData(55.0, "35.0, 45.0, 58.0, 44.0, 48.0", 0.05)>]
    [<InlineData(35.0, "35.0, 45.0, 58.0, 44.0, 48.0", 0.95)>]
    [<InlineData(-35.0, "-35.0, -45.0, -58.0, -44.0, -48.0", 0.05)>]
    [<InlineData(-55.0, "-35.0, -45.0, -58.0, -44.0, -48.0", 0.95)>]
    let ``One-sample T-Test for Sample Should Work`` (trueMean, sampleString: string, lowerP) =
        let sample = sampleString.Split(',') |> Array.map float
        let (lowerTail, upperTail, twoTail) = getZTestExpectedResults lowerP
        let summary = SummaryStatistics.create sample
        let sampleMean = SummaryStatistics.mean summary
        let sampleStd = SummaryStatistics.stddev summary
        let sampleSize = Array.length sample
        OneSample.tTest trueMean sampleMean sampleStd sampleSize LowerTailed |> lowerTail
        OneSample.tTest trueMean sampleMean sampleStd sampleSize UpperTailed |> upperTail
        OneSample.tTest trueMean sampleMean sampleStd sampleSize TwoTailed |> twoTail


module ``One Population Proportion`` =
    open OnePopulationProportion

    [<Theory>]
    [<InlineData(0.8, 0.75, 200, 0.03855)>]
    [<InlineData(0.8, 0.85, 200, 0.96145)>]
    [<InlineData(0.8, 0.75, 20, 0.28808)>]
    let ``Z-Test should work`` (p0, p, sampleSize, lowerP) =
        let (lowerTail, upperTail, twoTail) = getTestExpectedResults 1e-5 lowerP
        let opp = {
            PopulationProportion = p0
            SampleProportion = p
            SampleSize = sampleSize
        }
        zTest opp LowerTailed |> lowerTail
        zTest opp UpperTailed |> upperTail
        zTest opp TwoTailed |> twoTail


    [<Theory>]
    [<InlineData(0.8, 0.75, 200, 0.03932)>]
    [<InlineData(0.8, 0.85, 200, 0.96068)>]
    [<InlineData(0.8, 0.75, 20, 0.29134)>]
    let ``t-Test should work`` (p0, p, sampleSize, lowerP) =
        let (lowerTail, upperTail, twoTail) = getTestExpectedResults 1e-5 lowerP
        let opp = {
            PopulationProportion = p0
            SampleProportion = p
            SampleSize = sampleSize
        }
        tTest opp LowerTailed |> lowerTail
        tTest opp UpperTailed |> upperTail
        tTest opp TwoTailed |> twoTail

    [<Theory>]
    [<InlineData(0.8, 0.75, 200, -1.76777)>]
    [<InlineData(0.8, 0.85, 200, 1.76777)>]
    [<InlineData(0.8, 0.75, 20, -0.55902)>]
    [<InlineData(0.8, 0.85, 20, 0.55902)>]
    let ``score should work`` (p0, p, sampleSize, expected) =
        let opp = {
            PopulationProportion = p0
            SampleProportion = p
            SampleSize = sampleSize
        }
        score opp |> should (equalWithin 1e-5) expected


module ``Two population averages`` =
    open TwoPopulationAverages

    [<Theory>]
    [<InlineData(7.0, 2.3, 100, 7.1, 2.5, 50, -0.23709)>]
    [<InlineData(7.1, 2.5, 50, 7.0, 2.3, 100,  0.23709)>]
    [<InlineData(7.1, 2.5, 100, 8.0, 2.7, 100, -2.44587)>]
    [<InlineData(8.0, 2.7, 100, 7.1, 2.5, 100,  2.44587)>]
    let ``score should work`` (mean1, std1, size1, mean2, std2, size2, expected) =
        let s1 = {Mean = mean1; StdDev = std1; Size = size1 }
        let s2 = {Mean = mean2; StdDev = std2; Size = size2 }
        score s1 s2 |> should (equalWithin 1e-5) expected


    [<Theory>]
    [<InlineData(7.0, 2.3, 100, 7.1, 2.5, 50, 0.40629)>]
    [<InlineData(7.1, 2.5, 50, 7.0, 2.3, 100, 0.59371)>]
    [<InlineData(7.1, 2.5, 100, 8.0, 2.7, 100, 0.00723)>]
    [<InlineData(8.0, 2.7, 100, 7.1, 2.5, 100, 0.99277)>]
    let ``Z-Test should work`` (mean1, std1, size1, mean2, std2, size2, lowerP) =
        let (lowerTail, upperTail, twoTail) = getTestExpectedResults 1e-5 lowerP
        let s1 = {Mean = mean1; StdDev = std1; Size = size1 }
        let s2 = {Mean = mean2; StdDev = std2; Size = size2 }
        zTest s1 s2 LowerTailed |> lowerTail
        zTest s1 s2 UpperTailed |> upperTail
        zTest s1 s2 TwoTailed |> twoTail


    [<Theory>]
    [<InlineData(7.0, 2.3, 100, 7.1, 2.5, 50, 0.40679)>]
    [<InlineData(7.1, 2.5, 50, 7.0, 2.3, 100, 0.59321)>]
    [<InlineData(7.1, 2.5, 50, 7.0, 2.3, 30, 0.57164)>]
    [<InlineData(7.1, 2.5, 100, 8.0, 2.7, 100, 0.00811)>]
    [<InlineData(8.0, 2.7, 100, 7.1, 2.5, 100, 0.99189)>]
    let ``t-Test should work`` (mean1, std1, size1, mean2, std2, size2, lowerP) =
        let (lowerTail, upperTail, twoTail) = getTestExpectedResults 1e-5 lowerP
        let s1 = {Mean = mean1; StdDev = std1; Size = size1 }
        let s2 = {Mean = mean2; StdDev = std2; Size = size2 }
        tTest s1 s2 LowerTailed |> lowerTail
        tTest s1 s2 UpperTailed |> upperTail
        tTest s1 s2 TwoTailed |> twoTail


module ``Two Population Proportions`` =
    open TwoPopulationProportions

    [<Theory>]
    [<InlineData(0.11, 273, 0.17, 212, 0.03140)>]
    [<InlineData(0.17, 212, 0.11, 273, 0.03140)>]
    let ``Standard error should work`` (p1, n1, p2, n2, se) =
        let pp1 = {Proportion = p1; Size = n1}
        let pp2 = {Proportion = p2; Size = n2}
        standardError pp1 pp2 |> should (equalWithin 1e-5) se

    [<Theory>]
    [<InlineData(0.11, 273, 0.17, 212, -1.91073)>]
    [<InlineData(0.17, 212, 0.11, 273, 1.91073)>]
    let ``Score should work`` (p1, n1, p2, n2, expected) =
        let pp1 = {Proportion = p1; Size = n1}
        let pp2 = {Proportion = p2; Size = n2}
        score pp1 pp2 |> should (equalWithin 1e-5) expected


    [<Theory>]
    [<InlineData(0.11, 273, 0.17, 212, 0.02802)>]
    [<InlineData(0.17, 212, 0.11, 273, 0.97198)>]
    let ``z-Test should work`` (p1, n1, p2, n2, lowerP) =
        let (lowerTail, upperTail, twoTail) = getTestExpectedResults 1e-5 lowerP
        let pp1 = {Proportion = p1; Size = n1}
        let pp2 = {Proportion = p2; Size = n2}
        zTest pp1 pp2 LowerTailed |> lowerTail
        zTest pp1 pp2 UpperTailed |> upperTail
        zTest pp1 pp2 TwoTailed |> twoTail
        

    [<Theory>]
    [<InlineData(0.11, 273, 0.17, 212, 0.02870)>]
    [<InlineData(0.17, 212, 0.11, 273, 0.97130)>]
    let ``t-Test should work`` (p1, n1, p2, n2, lowerP) =
        let (lowerTail, upperTail, twoTail) = getTestExpectedResults 1e-5 lowerP
        let pp1 = {Proportion = p1; Size = n1}
        let pp2 = {Proportion = p2; Size = n2}
        tTest pp1 pp2 LowerTailed |> lowerTail
        tTest pp1 pp2 UpperTailed |> upperTail
        tTest pp1 pp2 TwoTailed |> twoTail

module ``PairedAverageDifference`` =
    open PairedAverageDifference

    [<Theory>]
    // Poisson 50 vs Poisson 50 + Normal (0, 4)
    [<InlineData("48 58 56 50 43 65 49 58 45 40 45", "49 56 58 53 51 65 46 56 43 37 41", 0.17150)>]
    [<InlineData("49 56 58 53 51 65 46 56 43 37 41", "48 58 56 50 43 65 49 58 45 40 45", -0.17150)>]
     // Poisson 50 vs Poisson 57
    [<InlineData("48 58 56 50 43 65 49 58 45 40 45", "55 54 62 68 53 60 65 47 67 53 56", -2.41397)>]
    [<InlineData("55 54 62 68 53 60 65 47 67 53 56", "48 58 56 50 43 65 49 58 45 40 45", 2.41397)>]
    let ``Score should work`` (set1String : string, set2String : string, expected) =
        let set1 = set1String.Split(' ') |> Array.map float
        let set2 = set2String.Split(' ') |> Array.map float
        let pd = PairedAverageDifference.create set1 set2
        PairedAverageDifference.score pd |> should (equalWithin 1e-5) expected
        true |> should equal true

    [<Theory>]
    // strongly speaking, using Z-Test for so small samples is not correct
    // Poisson 50 vs Poisson 50 + Normal (0, 4)
    [<InlineData("48 58 56 50 43 65 49 58 45 40 45", "49 56 58 53 51 65 46 56 43 37 41", 0.56808)>]
    [<InlineData("49 56 58 53 51 65 46 56 43 37 41", "48 58 56 50 43 65 49 58 45 40 45", 0.43192)>]
     // Poisson 50 vs Poisson 57
    [<InlineData("48 58 56 50 43 65 49 58 45 40 45", "55 54 62 68 53 60 65 47 67 53 56",  0.00789)>]
    [<InlineData("55 54 62 68 53 60 65 47 67 53 56",  "48 58 56 50 43 65 49 58 45 40 45", 0.99211)>]
    let ``Z-Test should work`` (set1String : string, set2String : string, lowerP) =
        let (lowerTail, upperTail, twoTail) = getTestExpectedResults 1e-5 lowerP
        let set1 = set1String.Split(' ') |> Array.map float
        let set2 = set2String.Split(' ') |> Array.map float
        let pd = PairedAverageDifference.create set1 set2
        zTest pd LowerTailed |> lowerTail
        zTest pd UpperTailed |> upperTail
        zTest pd TwoTailed |> twoTail


    [<Theory>]
    // Poisson 50 vs Poisson 50 + Normal (0, 4)
    [<InlineData("48 58 56 50 43 65 49 58 45 40 45", "49 56 58 53 51 65 46 56 43 37 41", 0.56637)>]
    [<InlineData("49 56 58 53 51 65 46 56 43 37 41", "48 58 56 50 43 65 49 58 45 40 45", 0.43363)>]
    // Poisson 50 vs Poisson 57
    [<InlineData("48 58 56 50 43 65 49 58 45 40 45", "55 54 62 68 53 60 65 47 67 53 56",  0.01822)>]
    [<InlineData("55 54 62 68 53 60 65 47 67 53 56", "48 58 56 50 43 65 49 58 45 40 45",  0.98178)>]
    let ``t-Test should work`` (set1String : string, set2String : string, lowerP) =
        let (lowerTail, upperTail, twoTail) = getTestExpectedResults 1e-5 lowerP
        let set1 = set1String.Split(' ') |> Array.map float
        let set2 = set2String.Split(' ') |> Array.map float
        let pd = PairedAverageDifference.create set1 set2
        tTest pd LowerTailed |> lowerTail
        tTest pd UpperTailed |> upperTail
        tTest pd TwoTailed |> twoTail


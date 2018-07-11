namespace FsStats.Tests

open Xunit
open FsUnit.Xunit

open FsStats.Hypothesis
open Utils

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


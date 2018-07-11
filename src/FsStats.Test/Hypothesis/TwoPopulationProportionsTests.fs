namespace FsStats.Tests

open Xunit
open FsUnit.Xunit

open FsStats.Hypothesis
open Utils


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

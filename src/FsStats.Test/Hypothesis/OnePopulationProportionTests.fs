namespace FsStats.Tests

open Xunit
open FsUnit.Xunit

open FsStats.Hypothesis
open Utils


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
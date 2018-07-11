namespace FsStats.Tests

open Xunit
open FsUnit.Xunit

open FsStats.Hypothesis
open Utils

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


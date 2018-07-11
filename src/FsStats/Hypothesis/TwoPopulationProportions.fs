namespace FsStats.Hypothesis

open LanguagePrimitives
open FsStats


/// Compare Two Population Proportions
module TwoPopulationProportions =
    type T = {
        Proportion : float
        Size : int
    }

    let standardError {Proportion = p1; Size = n1} {Proportion = p2; Size = n2} =
        let overallProportion = DivideByInt (p1 * float n1 + p2 * float n2) (n1 + n2)
        let variance = overallProportion * (1.0 - overallProportion)
        let n = (DivideByInt 1.0 n1) + (DivideByInt 1.0 n2)
        sqrt (variance * n)

    let score pp1 pp2 =
        (pp1.Proportion - pp2.Proportion) / standardError pp1 pp2

    let zTest pp1 pp2 testType =
        let z = score pp1 pp2
        Test.perform StandardDistribution.cdf testType z

    let tTest pp1 pp2 testType =
        let t = score pp1 pp2
        let d = StudentsDistribution.create (min pp1.Size pp2.Size)
        Test.perform (StudentsDistribution.cdf d) testType t

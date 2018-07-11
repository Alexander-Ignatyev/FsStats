namespace FsStats.Hypothesis

open LanguagePrimitives
open FsStats


/// One Population Proportion Tests
module OnePopulationProportion =
    type T = { 
        PopulationProportion : float
        SampleProportion : float
        SampleSize : int
    }

    let score { PopulationProportion = p0; SampleProportion = p; SampleSize = n } = 
        let standardError = DivideByInt (p0 * (1.0 - p0)) n |> sqrt
        (p - p0) / standardError

    let zTest opp testType =
        let z = score opp
        Test.perform StandardDistribution.cdf testType z

    let tTest opp testType =
        let t = score opp
        let d = StudentsDistribution.create opp.SampleSize
        Test.perform (StudentsDistribution.cdf d) testType t

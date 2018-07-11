namespace FsStats.Hypothesis

open LanguagePrimitives
open FsStats
    
    
/// Compare Two Population Averages
module TwoPopulationAverages =
    type T = {
        Mean : float     // Sample mean
        StdDev : float   // Sample standard deviation
        Size : int       // Sample size
    }

    let standardError s1 s2 =
        let se2 {StdDev = std; Size = n} = DivideByInt (std ** 2.0) n
        sqrt (se2 s1 + se2 s2)

    let score s1 s2 =
        (s1.Mean - s2.Mean) / standardError s1 s2


    let zTest s1 s2 testType =
        let z = score s1 s2
        Test.perform StandardDistribution.cdf testType z

    let tTest s1 s2 testType =
        let t = score s1 s2
        let d = StudentsDistribution.create (min s1.Size s2.Size)
        Test.perform (StudentsDistribution.cdf d) testType t

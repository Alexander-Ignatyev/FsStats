namespace FsStats.AWSLambda

module Processor =
    type Request = {
        Bernoulli : BernoulliDistribution.Request option
        Binomial : BinomialDistribution.Request option
        Poisson : PoissonDistribution.Request option
        Normal : NormalDistribution.Request option
        Students : StudentsDistribution.Request option
        Summary : SummaryStatistics.Request option
        OnePopulationMeanTest : OnePopulationMeanTest.Request option
    }

    type Response = {
        Bernoulli : BernoulliDistribution.Response option
        Binomial : BinomialDistribution.Response option
        Poisson : PoissonDistribution.Response option
        Normal : NormalDistribution.Response option
        Students : StudentsDistribution.Response option
        Summary : SummaryStatistics.Response option
        OnePopulationMeanTest : OnePopulationMeanTest.Response option
    }
    
    let handle (r: Request) = {
        Bernoulli = Option.map BernoulliDistribution.handle r.Bernoulli
        Binomial = Option.map BinomialDistribution.handle r.Binomial
        Poisson = Option.map PoissonDistribution.handle r.Poisson
        Normal = Option.map NormalDistribution.handle r.Normal
        Students = Option.map StudentsDistribution.handle r.Students
        Summary = Option.map SummaryStatistics.handle r.Summary
        OnePopulationMeanTest = Option.map OnePopulationMeanTest.handle r.OnePopulationMeanTest
    }

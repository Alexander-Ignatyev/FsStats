namespace FsStats.AWSLambda

module Processor =
    type Request = {
        Bernoulli : BernoulliDistribution.Request option
        Binomial : BinomialDistribution.Request option
        Normal : NormalDistribution.Request option
    }

    type Response = {
        Bernoulli : BernoulliDistribution.Response option
        Binomial : BinomialDistribution.Response option
        Normal : NormalDistribution.Response option
    }
    
    let handle (r: Request) = {
        Bernoulli = Option.map BernoulliDistribution.handle r.Bernoulli
        Binomial = Option.map BinomialDistribution.handle r.Binomial
        Normal = Option.map NormalDistribution.handle r.Normal
    }

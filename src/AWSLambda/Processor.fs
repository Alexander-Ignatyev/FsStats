namespace FsStats.AWSLambda

module Processor =
    type Request = {
        Bernoulli : BernoulliDistribution.Request option
        Normal : NormalDistribution.Request option
    }

    type Response = {
        Bernoulli : BernoulliDistribution.Response option
        Normal : NormalDistribution.Response option
    }
    
    let handle (r: Request) = {
        Bernoulli = Option.map BernoulliDistribution.handle r.Bernoulli
        Normal = Option.map NormalDistribution.handle r.Normal
    }

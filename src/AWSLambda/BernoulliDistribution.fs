namespace FsStats.AWSLambda

open FsStats

module BernoulliDistribution =
    type Request = {
        Params : BernoulliDistribution.T
        Pmf : int option
        Cdf : int option
        Sample : int option
    }

    type Response = {
        Params : BernoulliDistribution.T
        Pmf : float option
        Cdf : float option
        Sample : int[] option
    }

    let rnd = System.Random()

    let handle (r: Request) = {
            Params = r.Params
            Pmf = Option.map (BernoulliDistribution.pmf r.Params) r.Pmf
            Cdf = Option.map (BernoulliDistribution.cdf r.Params) r.Cdf
            Sample = Option.map (BernoulliDistribution.sample r.Params rnd) r.Sample
        }

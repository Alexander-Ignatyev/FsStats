namespace FsStats.AWSLambda

module Processor =
    type Request = {
        Normal : NormalDistribution.Request
    }

    type Response = {
        Normal : NormalDistribution.Response
    }
    
    let handle (r: Request) = {
        Normal = NormalDistribution.handle r.Normal
    }

namespace FsStats.AWSLambda

open Amazon.Lambda.Core

[<assembly: LambdaSerializer(typeof<JsonSerializer>)>]
()


type Function() =
    member _this.FunctionHandler (input: Processor.Request) (_: ILambdaContext) =
        Processor.handle input

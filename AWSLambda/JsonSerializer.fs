namespace FsStats.AWSLambda

open Newtonsoft.Json

type JsonSerializer() =
     inherit Amazon.Lambda.Serialization.Json.JsonSerializer([Fable.JsonConverter() :> JsonConverter])


# AWS Lambda Stats Function

This project consists of:
* Function.fs - Code file containing the function handler method
* aws-lambda-tools-defaults.json - default argument settings for use with Visual Studio and command line deployment tools for AWS

## Here are some steps to follow from Visual Studio:

To deploy the function to AWS Lambda, right click the project in Solution Explorer and select *Publish to AWS Lambda*.

To view the deployed function open its Function View window by double-clicking the function name shown beneath the AWS Lambda node in the AWS Explorer tree.

To perform testing against the deployed function use the Test Invoke tab in the opened Function View window.

To configure event sources for the deployed function, for example to have the function invoked when an object is created in an Amazon S3 bucket, use the Event Sources tab in the opened Function View window.

To update the runtime configuration of the deployed function use the Configuration tab in the opened Function View window.

To view execution logs of invocations of the function use the Logs tab in the opened Function View window.

## Here are some steps to follow to get started from the command line:

Once you have edited the function you can use the following command lines to build, test and deploy the function to AWS Lambda from the command line:

Restore dependencies
```
    cd src/AWSLambda
    dotnet restore
```

Deploy function to AWS Lambda
```
    cd src/AWSLambda
    dotnet lambda deploy-function
```

## Here is an example of JSON to test the function

```(json)
{
  "Normal": {
    "Params": {
      "Mu": 10,
      "Sigma": 3
    },
    "Curve": 20,
    "Pdf": 10,
    "Cdf": 11,
    "Quantile": 0.63055859,
    "Sample": 10
  }
}
```
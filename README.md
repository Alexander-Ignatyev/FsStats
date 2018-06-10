# FsStats

[![Build Status](https://travis-ci.org/aligusnet/FsStats.svg?branch=master)](https://travis-ci.org/aligusnet/FsStats)

Basic statistics with some probability library.

Includes common distributions (Bernoulli, Binomial, Poisson, Student's and Normal), random number generators from some of the distributions, summary statistics for a sample, Z-Test, Student's T-Test, special functions (Error, Gamma, Beta and Regularized Incomplete Beta).

## Getting started with .NET Core

### Install to use in your project

```(bash)
dotnet add package FsStats
```

## Developing

### Build and run unit tests

```(bash)
dotnet test src/Test/Test.fsproj
```

### Build and publishing the package

```(bash)
dotnet pack  # prints path to nupkg required by the next command
dotnet nuget push <path/to/nupkg> -k <API-KEY> -s https://api.nuget.org/v3/index.json
```

## Licensing

The code in this project is licensed under BSD 3-Clause license.

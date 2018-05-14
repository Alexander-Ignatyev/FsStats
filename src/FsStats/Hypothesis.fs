module FsStats.Hypothesis

open FsStats.Distribution

/// Test Hypothesis
/// Returns True if we cannot reject the hypothesis
/// for the given distribution, threshould value alpha and random value x
let testHypothesis  (distribution : Distribution<'T>) (alpha : float) (x : 'T) =
    let p = distribution.CumulativeProbability x
    if (System.Convert.ToDouble x) < distribution.Mean
    then p >= alpha
    else (1.0 - p) >= alpha

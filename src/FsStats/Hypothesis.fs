namespace FsStats

module Hypothesis = 
    /// Test Hypothesis
    /// Returns True if we cannot reject the hypothesis
    /// for the given distribution, threshould value alpha and random value x
    let testHypothesis  (distribution : Distribution<'T>) (alpha : float) (x : 'T) =
        let p = distribution.CumulativeProbability x
        if (System.Convert.ToDouble x) < distribution.Mean
        then p >= alpha
        else (1.0 - p) >= alpha

    /// z-score is the number of standard deviations from the mean a data point is.
    /// It takes mean mu, standard deviation sigma and the data point x. 
    let zScore mu sigma x = (x - mu) / sigma

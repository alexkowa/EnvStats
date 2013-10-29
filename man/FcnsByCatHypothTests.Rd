\name{FcnsByCatHypothTests}
\alias{FcnsByCatHypothTests}
\alias{Hypothesis Tests}
\title{
  EnvStats Functions for Hypothesis Tests
}
\description{
  The \pkg{EnvStats} functions listed below are useful for performing hypothesis tests not 
  already built into \R.  See \link{Power and Sample Size Calculations} for a list of 
  functions you can use to perform power and sample size calculations based on various 
  hypothesis tests.
}
\details{
  For goodness-of-fit tests, see \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests}.

  \tabular{ll}{
  \emph{Function Name}                             \tab \emph{Description} \cr
  \code{\link{chenTTest}}                          \tab Chen's modified one-sided t-test for skewed \cr
                                                   \tab distributions. \cr
  \code{\link{kendallTrendTest}}                   \tab Nonparametric test for monotonic trend \cr
                                                   \tab based on Kendall's tau statistic (and \cr
                                                   \tab optional confidence interval for slope). \cr
  \code{\link{kendallSeasonalTrendTest}}           \tab Nonparametric test for monotonic trend \cr
                                                   \tab within each season based on Kendall's tau \cr
                                                   \tab statistic (and optional confidence interval \cr
                                                   \tab for slope). \cr
  \code{\link{oneSamplePermutationTest}}           \tab Fisher's one-sample randomization \cr
                                                   \tab (permutation) test for location. \cr
  \code{\link{quantileTest}}                       \tab Two-sample rank test to detect a shift in \cr
                                                   \tab a proportion of the \dQuote{treated} population. \cr
  \code{\link{quantileTestPValue}}                 \tab Compute p-value associated with a specified \cr
                                                   \tab combination of \eqn{m}, \eqn{n}, \eqn{r} and \eqn{k} \cr
                                                   \tab for the quantile test. \cr
                                                   \tab Useful for determining \eqn{r} and \eqn{k} for a \cr
                                                   \tab given significance level \eqn{\alpha}. \cr
  \code{\link{serialCorrelationTest}}              \tab Test for the presence of serial correlation. \cr
  \code{\link{signTest}}                           \tab One- or paired-sample sign test on the \cr
                                                   \tab median. \cr
  \code{\link{twoSampleLinearRankTest}}            \tab Two-sample linear rank test to detect a \cr
                                                   \tab shift in the \dQuote{treated} population. \cr
  \code{\link{twoSamplePermutationTestLocation}}   \tab Two-sample or paired-sample randomization \cr
                                                   \tab (permutation) test for location. \cr
  \code{\link{twoSamplePermutationTestProportion}} \tab Randomization (permutation) test to compare \cr
                                                   \tab two proportions (Fisher's exact test). \cr
  \code{\link{varTest}}                            \tab One-sample test on variance or two-sample \cr
                                                   \tab test to compare variances. \cr
  \code{\link{varGroupTest}}                       \tab Test for homogeneity of variance among two \cr
                                                   \tab or more groups. \cr
  \code{\link{zTestGevdShape}}                     \tab Estimate the shape parameter of a \cr
                                                   \tab Generalized Extreme Value distribution and \cr
                                                   \tab test the null hypothesis that the true \cr
                                                   \tab value is equal to 0. \cr
  }
}
\keyword{ package }

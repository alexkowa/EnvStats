\name{oneSamplePermutationTest}
\alias{oneSamplePermutationTest}
\title{
  Fisher's One-Sample Randomization (Permutation) Test for Location
}
\description{
  Perform Fisher's one-sample randomization (permutation) test for location.
}
\usage{
  oneSamplePermutationTest(x, alternative = "two.sided", mu = 0, exact = FALSE, 
    n.permutations = 5000, seed = NULL, ...)
}
\arguments{
  \item{x}{
  numeric vector of observations.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{alternative}{
  character string indicating the kind of alternative hypothesis.  The possible values 
  are \code{"two.sided"} (the default), \code{"less"}, and \code{"greater"}. 
}
  \item{mu}{
  numeric scalar indicating the hypothesized value of the mean.  
  The default value is \code{mu=0}.
}
  \item{exact}{
  logical scalar indicating whether to perform the exact permutation test 
  (i.e., enumerate all possible permutations) or simply sample from the permutation 
  distribution.  The default value is \code{exact=FALSE}.
}
  \item{n.permutations}{
  integer indicating how many times to sample from the permutation distribution when 
  \code{exact=FALSE}.  The default value is \code{n.permutations=5000}.  
  This argument is ignored when \code{exact=TRUE}.
}
  \item{seed}{
  positive integer to pass to the \R function \code{\link{set.seed}}.  The 
  default is \code{seed=NULL}, in which case the current value of 
  \code{\link{.Random.seed}} is used.  
  Using the \code{seed} argument lets you reproduce the exact same result if all 
  other arguments stay the same.
}
  \item{\dots}{
  arguments that can be supplied to the \code{\link[base]{format}} function.  This 
  argument is used when creating the \code{names} attribute for the \code{statistic} 
  component of the returned list (see \code{\link{permutationTest.object}}).
}
}
\details{
  \emph{Randomization Tests} \cr
  In 1935, R.A. Fisher introduced the idea of a \bold{\emph{randomization test}} 
  (Manly, 2007, p. 107; Efron and Tibshirani, 1993, Chapter 15), which is based on 
  trying to answer the question:  \dQuote{Did the observed pattern happen by chance, 
  or does the pattern indicate the null hypothesis is not true?}  A randomization 
  test works by simply enumerating all of the possible outcomes under the null 
  hypothesis, then seeing where the observed outcome fits in.  A randomization test 
  is also called a \bold{\emph{permutation test}}, because it involves permuting the 
  observations during the enumeration procedure (Manly, 2007, p. 3).

  In the past, randomization tests have not been used as extensively as they are now 
  because of the \dQuote{large} computing resources needed to enumerate all of the 
  possible outcomes, especially for large sample sizes.  The advent of more powerful 
  personal computers and software has allowed randomization tests to become much 
  easier to perform.  Depending on the sample size, however, it may still be too 
  time consuming to enumerate all possible outcomes.  In this case, the randomization 
  test can still be performed by sampling from the randomization distribution, and 
  comparing the observed outcome to this sampled permutation distribution.
  \cr

  \emph{Fisher's One-Sample Randomization Test for Location} \cr
  Let \eqn{\underline{x} = x_1, x_2, \ldots, x_n} be a vector of \eqn{n} independent 
  and identically distributed (i.i.d.) observations from some symmetric distribution 
  with mean \eqn{\mu}.  Consider the test of the null hypothesis that the mean 
  \eqn{\mu} is equal to some specified value \eqn{\mu_0}:
  \deqn{H_0: \mu = \mu_0 \;\;\;\;\;\; (1)}
  The three possible alternative hypotheses are the upper one-sided alternative 
  (\code{alternative="greater"})
  \deqn{H_a: \mu > \mu_0 \;\;\;\;\;\; (2)}
  the lower one-sided alternative (\code{alternative="less"})
  \deqn{H_a: \mu < \mu_0 \;\;\;\;\;\; (3)}
  and the two-sided alternative
  \deqn{H_a: \mu \ne \mu_0 \;\;\;\;\;\; (4)}
  To perform the test of the null hypothesis (1) versus any of the three 
  alternatives (2)-(4), Fisher proposed using the test statistic
  \deqn{T = \sum_{i=1}^n y_i \;\;\;\;\;\; (5)}
  where
  \deqn{y_i = x_i - \mu_0 \;\;\;\;\;\; (6)}
  (Manly, 2007, p. 112).  The test assumes all of the observations come from the 
  same distribution that is symmetric about the true population mean 
  (hence the mean is the same as the median for this distribution).  
  Under the null hypothesis, the \eqn{y_i}'s are equally likely to be positive or 
  negative.  Therefore, the permutation distribution of the test statistic \eqn{T} 
  consists of enumerating all possible ways of permuting the signs of the 
  \eqn{y_i}'s and computing the resulting sums.  For \eqn{n} observations, there are 
  \eqn{2^n} possible permutations of the signs, because each observation can either 
  be positive or negative.

  For a one-sided upper alternative hypothesis (Equation (2)), the p-value is computed 
  as the proportion of sums in the permutation distribution that are greater than or 
  equal to the observed sum \eqn{T}.  For a one-sided lower alternative hypothesis 
  (Equation (3)), the p-value is computed as the proportion of sums in the permutation 
  distribution that are less than or equal to the observed sum \eqn{T}.  For a 
  two-sided alternative hypothesis (Equation (4)), the p-value is computed by using 
  the permutation distribution of the absolute value of \eqn{T} (i.e., \eqn{|T|}) 
  and computing the proportion of values in this permutation distribution that are 
  greater than or equal to the observed value of \eqn{|T|}.
  \cr
 
  \emph{Confidence Intervals Based on Permutation Tests} \cr
  Based on the relationship between hypothesis tests and confidence intervals, it is 
  possible to construct a two-sided or one-sided \eqn{(1-\alpha)100\%} confidence 
  interval for the mean \eqn{\mu} based on the one-sample permutation test by finding 
  the values of \eqn{\mu_0} that correspond to obtaining a p-value of \eqn{\alpha}  
  (Manly, 2007, pp. 18--20, 113).  A confidence interval based on the bootstrap 
  however, will yield a similar type of confidence interval 
  (Efron and Tibshirani, 1993, p. 214); see the help file for 
  \code{\link[boot]{boot}} in the \R package \pkg{boot}.
}
\value{
  A list of class \code{"permutationTest"} containing the results of the hypothesis 
  test.  See the help file for \code{\link{permutationTest.object}} for details.
}
\references{
  Efron, B., and R.J. Tibshirani. (1993).  \emph{An Introduction to the Bootstrap}.  
  Chapman and Hall, New York, pp.224--227.

  Manly, B.F.J. (2007).  \emph{Randomization, Bootstrap and Monte Carlo Methods in 
  Biology}.  Third Edition. Chapman & Hall, New York, pp.112-113.

  Millard, S.P., and N.K. Neerchal. (2001).  
  \emph{Environmental Statistics with S-PLUS}.  CRC Press, Boca Raton, FL, pp.404--406.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  A frequent question in environmental statistics is \dQuote{Is the concentration of 
  chemical X greater than Y units?}.  For example, in groundwater assessment 
  (compliance) monitoring at hazardous and solid waste sites, the concentration of a 
  chemical in the groundwater at a downgradient well must be compared to a 
  groundwater protection standard (GWPS).  If the concentration is \dQuote{above} the 
  GWPS, then the site enters corrective action monitoring.  As another example, soil 
  screening at a Superfund site involves comparing the concentration of a chemical in 
  the soil with a pre-determined soil screening level (SSL).  If the concentration is 
  \dQuote{above} the SSL, then further investigation and possible remedial action is 
  required.  Determining what it means for the chemical concentration to be 
  \dQuote{above} a GWPS or an SSL is a policy decision:  the average of the 
  distribution of the chemical concentration must be above the GWPS or SSL, or the 
  median must be above the GWPS or SSL, or the 95'th percentile must be above the 
  GWPS or SSL, or something else.  Often, the first interpretation is used.

  Hypothesis tests you can use to perform tests of location include:  
  \link[stats:t.test]{Student's t-test}, Fisher's randomization test, the 
  \link[stats:wilcox.test]{Wilcoxon signed rank test}, 
  \link[=chenTTest]{Chen's modified t-test}, the 
  \link[=signTest]{sign test}, and a test based on a bootstrap confidence interval.  
  For a discussion comparing the performance of these tests, see 
  Millard and Neerchal (2001, pp.408-409).
}
\seealso{
  \code{\link{permutationTest.object}}, \link{Hypothesis Tests}, 
  \code{\link[boot]{boot}}.
}
\examples{
  # Generate 10 observations from a logistic distribution with parameters 
  # location=7 and scale=2, and test the null hypothesis that the true mean 
  # is equal to 5 against the alternative that the true mean is greater than 5. 
  # Use the exact permutation distribution. 
  # (Note: the call to set.seed() allows you to reproduce this example).

  set.seed(23) 

  dat <- rlogis(10, location = 7, scale = 2) 

  test.list <- oneSamplePermutationTest(dat, mu = 5, 
    alternative = "greater", exact = TRUE) 

  # Print the results of the test 
  #------------------------------
  test.list 

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 Mean (Median) = 5
  #
  #Alternative Hypothesis:          True Mean (Median) is greater than 5
  #
  #Test Name:                       One-Sample Permutation Test
  #                                 (Exact)
  #
  #Estimated Parameter(s):          Mean = 9.977294
  #
  #Data:                            dat
  #
  #Sample Size:                     10
  #
  #Test Statistic:                  Sum(x - 5) = 49.77294
  #
  #P-value:                         0.001953125


  # Plot the results of the test 
  #-----------------------------
  dev.new()
  plot(test.list)

  #==========

  # The guidance document "Supplemental Guidance to RAGS: Calculating the 
  # Concentration Term" (USEPA, 1992d) contains an example of 15 observations 
  # of chromium concentrations (mg/kg) which are assumed to come from a 
  # lognormal distribution.  These data are stored in the vector 
  # EPA.92d.chromium.vec.  Here, we will use the permutation test to test 
  # the null hypothesis that the mean (median) of the log-transformed chromium 
  # concentrations is less than or equal to log(100 mg/kg) vs. the alternative 
  # that it is greater than log(100 mg/kg).  Note that we *cannot* use the 
  # permutation test to test a hypothesis about the mean on the original scale 
  # because the data are not assumed to be symmetric about some mean, they are 
  # assumed to come from a lognormal distribution.
  #
  # We will sample from the permutation distribution.
  # (Note: setting the argument seed=542 allows you to reproduce this example).

  test.list <- oneSamplePermutationTest(log(EPA.92d.chromium.vec), 
    mu = log(100), alternative = "greater", seed = 542) 

  test.list

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 Mean (Median) = 4.60517
  #
  #Alternative Hypothesis:          True Mean (Median) is greater than 4.60517
  #
  #Test Name:                       One-Sample Permutation Test
  #                                 (Based on Sampling
  #                                 Permutation Distribution
  #                                 5000 Times)
  #
  #Estimated Parameter(s):          Mean = 4.378636
  #
  #Data:                            log(EPA.92d.chromium.vec)
  #
  #Sample Size:                     15
  #
  #Test Statistic:                  Sum(x - 4.60517) = -3.398017
  #
  #P-value:                         0.7598


  # Plot the results of the test 
  #-----------------------------
  dev.new()
  plot(test.list)

  #----------

  # Clean up
  #---------
  rm(test.list)
  graphics.off()
}
\keyword{htest}
\keyword{models}

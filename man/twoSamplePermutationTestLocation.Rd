\name{twoSamplePermutationTestLocation}
\alias{twoSamplePermutationTestLocation}
\title{
  Two-Sample or Paired-Sample Randomization (Permutation) Test for Location
}
\description{
  Perform a two-sample or paired-sample randomization (permutation) test for 
  location based on either means or medians.
}
\usage{
  twoSamplePermutationTestLocation(x, y, fcn = "mean", alternative = "two.sided", 
    mu1.minus.mu2 = 0, paired = FALSE, exact = FALSE, n.permutations = 5000, 
    seed = NULL, tol = sqrt(.Machine$double.eps))
}
\arguments{
  \item{x}{
  numeric vector of observations from population 1.    
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{y}{
  numeric vector of observations from population 2.    
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.

  In the case when \code{paired=TRUE}, the argument \code{y} must have the same number 
  of elements as the argument \code{x}.
}
  \item{fcn}{
  character string indicating which location parameter to compare between the two 
  groups.  The possible values are \code{fcn="mean"} (the default) and 
  \code{fcn="median"}.  This argument is ignored when \code{paired=TRUE}.
}
  \item{alternative}{
  character string indicating the kind of alternative hypothesis.  The possible values 
  are \code{"two.sided"} (the default), \code{"less"}, and \code{"greater"}. 
}
  \item{mu1.minus.mu2}{
  numeric scalar indicating the hypothesized value of the difference between the 
  means or medians.  The default value is \code{mu1.minus.mu2=0}.
}
  \item{paired}{
  logical scalar indicating whether to perform a paired or two-sample permutation 
  test.  The possible values are \code{paired=FALSE} (the default; indicates a 
  two-sample permutation test) and \code{paired=TRUE} (indicates take differences of 
  pairs and perform a one-sample permutation test).
}
  \item{exact}{
  logical scalar indicating whether to perform the exact permutation test (i.e., 
  enumerate all possible permutations) or simply sample from the permutation 
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
  \item{tol}{
  numeric scalar indicating the tolerance to use for computing the p-value for the 
  two-sample permutation test.  The default value is \cr
  \code{tol=sqrt(.Machine$double.eps)}.  See the DETAILS section below for more 
  information.
}
}
\details{
  \bold{Randomization Tests} \cr
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

  \emph{Two-Sample Randomization Test for Location} (\code{paired=FALSE}) \cr
  Let \eqn{\underline{x} = x_1, x_2, \ldots, x_{n1}} be a vector of \eqn{n1} 
  independent and identically distributed (i.i.d.) observations
  from some distribution with location parameter (e.g., mean or median) \eqn{\theta_1}, 
  and let \eqn{\underline{y} = y_1, y_2, \ldots, y_{n2}} be a vector of \eqn{n2} 
  i.i.d. observations from the same distribution with possibly different location 
  parameter \eqn{\theta_2}.

  Consider the test of the null hypothesis that the difference in the location 
  parameters is equal to some specified value: 
  \deqn{H_0: \delta = \delta_0 \;\;\;\;\;\; (1)}
  where
  \deqn{\delta = \theta_1 - \theta_2 \;\;\;\;\;\; (2)}
  and \eqn{\delta_0} denotes the hypothesized difference in the meansures of 
  location (usually \eqn{\delta_0 = 0}).

  The three possible alternative hypotheses are the upper one-sided alternative 
  (\code{alternative="greater"})
  \deqn{H_a: \delta > \delta_0 \;\;\;\;\;\; (3)}
  the lower one-sided alternative (\code{alternative="less"})
  \deqn{H_a: \delta < \delta_0 \;\;\;\;\;\; (4)}
  and the two-sided alternative
  \deqn{H_a: \delta \ne \delta_0 \;\;\;\;\;\; (5)}
  To perform the test of the null hypothesis (1) versus any of the three 
  alternatives (3)-(5), you can use the two-sample permutation test.  The two 
  sample permutation test is based on trying to answer the question, 
  \dQuote{Did the observed difference in means or medians happen by chance, or 
  does the observed difference indicate that the null hypothesis is not true?}  
  Under the null hypothesis, the underlying distributions for each group are the 
  same, therefore it should make no difference which group an observation gets 
  assigned to.  The two-sample permutation test works by simply enumerating all 
  possible permutations of group assignments, and for each permutation computing 
  the difference between the measures of location for each group 
  (Manly, 2007, p. 113; Efron and Tibshirani, 1993, p. 202).  The measure of 
  location for a group could be the mean, median, or any other measure you want to 
  use.  For example, if the observations from Group 1 are 3 and 5, and the 
  observations from Group 2 are 4, 6, and 7, then there are 10 different ways of 
  splitting these five observations into one group of size 2 and another group of 
  size 3.  The table below lists all of the possible group assignments, along with 
  the differences in the group means.
  \tabular{lll}{
  \bold{Group 1} \tab \bold{Group 2} \tab \bold{Mean 1 - Mean 2} \cr
  3, 4           \tab 5, 6, 7        \tab -2.5  \cr
  3, 5           \tab 4, 6, 7        \tab -1.67 \cr
  3, 6           \tab 4, 5, 7        \tab -0.83 \cr
  3, 7           \tab 4, 5, 6        \tab  0    \cr
  4, 5           \tab 3, 6, 7        \tab -0.83 \cr
  4, 6           \tab 3, 5, 7        \tab  0    \cr
  4, 7           \tab 3, 5, 6        \tab  0.83 \cr
  5, 6           \tab 3, 4, 7        \tab  0.83 \cr
  5, 7           \tab 3, 4, 6        \tab  1.67 \cr
  6, 7           \tab 3, 4, 5        \tab  2.5
  } 
  In this example, the \emph{observed} group assignments and difference in means 
  are shown in the second row of the table.

  For a one-sided upper alternative (Equation (3)), the p-value is computed as the 
  proportion of times that the differences of the means (or medians) in the 
  permutation distribution are greater than or equal to the observed difference in 
  means (or medians).  For a one-sided lower alternative hypothesis (Equation (4)), 
  the p-value is computed as the proportion of times that the differences in the means 
  (or medians) in the permutation distribution are less than or equal to the observed 
  difference in the means (or medians).  For a two-sided alternative hypothesis 
  (Equation (5)), the p-value is computed as the proportion of times the absolute 
  values of the differences in the means (or medians) in the permutation distribution 
  are greater than or equal to the absolute value of the observed difference in the 
  means (or medians).

  For this simple example, the one-sided upper, one-sided lower, and two-sided 
  p-values are 0.9, 0.2 and 0.4, respectively.

  \bold{Note:} Because of the nature of machine arithmetic and how the permutation 
  distribution is computed, a one-sided upper p-value is computed as the proportion 
  of times that the differences of the means (or medians) in the permutation 
  distribution are greater than or equal to 
  [the observed difference in means (or medians) - a small tolerance value], where the 
  tolerance value is determined by the argument \code{tol}.  Similarly, a one-sided 
  lower p-value is computed as the proportion of times that the differences in the 
  means (or medians) in the permutation distribution are less than or equal to 
  [the observed difference in the means (or medians) + a small tolerance value].  
  Finally, a two-sided p-value is computed as the proportion of times the absolute 
  values of the differences in the means (or medians) in the permutation distribution 
  are greater than or equal to 
  [the absolute value of the observed difference in the means (or medians) - a small tolerance value].

  In this simple example, we assumed the hypothesized differences in the means under 
  the null hypothesis was \eqn{\delta_0 = 0}.  If we had hypothesized a different 
  value for \eqn{\delta_0}, then we would have had to subtract this value from each of 
  the observations in Group 1 before permuting the group assignments to compute the 
  permutation distribution of the differences of the means.  As in the case of the 
  \link[=oneSamplePermutationTest]{one-sample permutation test}, if the sample sizes 
  for the groups become too large to compute all possible permutations of the group 
  assignments, the permutation test can still be performed by sampling from the 
  permutation distribution and comparing the observed difference in locations to the 
  sampled permutation distribution of the difference in locations.

  Unlike the two-sample \link[stats:t.test]{Student's t-test}, we do not have to worry 
  about the normality assumption when we use a permutation test.  The permutation test 
  still assumes, however, that under the null hypothesis, the distributions of the 
  observations from each group are exactly the same, and under the alternative 
  hypothesis there is simply a shift in location (that is, the whole distribution of 
  group 1 is shifted by some constant relative to the distribution of group 2).  
  Mathematically, this can be written as follows:
  \deqn{F_1(t) = F_2(t - \delta), \;\; -\infty < t < \infty \;\;\;\;\; (6)}
  where \eqn{F_1} and \eqn{F_2} denote the cumulative distribution functions for 
  group 1 and group 2, respectively.  If \eqn{\delta > 0}, this implies that the 
  observations in group 1 tend to be larger than the observations in group 2, and 
  if \eqn{\delta < 0}, this implies that the observations in group 1 tend to be 
  smaller than the observations in group 2.  Thus, the shape and spread (variance) 
  of the two distributions should be the same whether the null hypothesis is true or 
  not.  Therefore, the Type I error rate for a permutation test can be affected by 
  differences in variances between the two groups.
  \cr
 
  \emph{Confidence Intervals for the Difference in Means or Medians} \cr
  Based on the relationship between hypothesis tests and confidence intervals, it is 
  possible to construct a two-sided or one-sided \eqn{(1-\alpha)100\%} confidence 
  interval for the difference in means or medians based on the two-sample permutation 
  test by finding the values of \eqn{\delta_0} that correspond to obtaining a 
  p-value of \eqn{\alpha} (Manly, 2007, pp. 18--20, 114).  A confidence interval 
  based on the bootstrap however, will yield a similar type of confidence interval 
  (Efron and Tibshirani, 1993, p. 214); see the help file for 
  \code{\link[boot]{boot}} in the \R package \pkg{boot}.
  \cr
 
  \bold{Paired-Sample Randomization Test for Location} (\code{paired=TRUE}) \cr
  When the argument \code{paired=TRUE}, the arguments \code{x} and \code{y} are 
  assumed to have the same length, and the \eqn{n1 = n2 = n} differences
  \eqn{y_i = x_i - y_i}, \eqn{i = 1, 2, \ldots, n} are assumed to be independent 
  observations from some symmetric distribution with mean \eqn{\mu}.  The 
  \link[=oneSamplePermutationTest]{one-sample permutation test} can then be applied 
  to the differences.
}
\value{
  A list of class \code{"permutationTest"} containing the results of the hypothesis 
  test.  See the help file for \code{\link{permutationTest.object}} for details.
}
\references{
  Efron, B., and R.J. Tibshirani. (1993).  \emph{An Introduction to the Bootstrap}.  
  Chapman and Hall, New York, Chapter 15.

  Manly, B.F.J. (2007).  \emph{Randomization, Bootstrap and Monte Carlo Methods in 
  Biology}.  Third Edition. Chapman & Hall, New York, Chapter 6.

  Millard, S.P., and N.K. Neerchal. (2001).  
  \emph{Environmental Statistics with S-PLUS}.  CRC Press, Boca Raton, FL, 
  pp.426--431.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  A frequent question in environmental statistics is \dQuote{Is the concentration of 
  chemical X in Area A greater than the concentration of chemical X in Area B?}.  
  For example, in groundwater detection monitoring at hazardous and solid waste sites, 
  the concentration of a chemical in the groundwater at a downgradient well must be 
  compared to \dQuote{background}.  If the concentration is \dQuote{above} the 
  background then the site enters assessment monitoring.  As another example, soil 
  cleanup at a Superfund site may involve comparing the concentration of a chemical 
  in the soil at a \dQuote{cleaned up} site with the concentration at a 
  \dQuote{background} site.  If the concentration at the \dQuote{cleaned up} site is 
  \dQuote{greater} than the background concentration, then further investigation and 
  remedial action may be required.  Determining what it means for the chemical 
  concentration to be \dQuote{greater} than background is a policy decision: you may 
  want to compare averages, medians, 95'th percentiles, etc.

  Hypothesis tests you can use to compare \dQuote{location} between two groups include:  
  \link[stats:t.test]{Student's t-test}, Fisher's randomization test 
  (described in this help file), the \link[stats:wilcox.test]{Wilcoxon rank sum test}, 
  other \link[=twoSampleLinearRankTest]{two-sample linear rank tests}, 
  the \link[=quantileTest]{quantile test}, and a test based on a bootstrap confidence 
  interval.
}
\seealso{
  \code{\link{permutationTest.object}}, \code{\link{plot.permutationTest}}, 
  \code{\link{oneSamplePermutationTest}}, \cr
  \code{\link{twoSamplePermutationTestProportion}}, 
  \link{Hypothesis Tests}, \code{\link[boot]{boot}}.
}
\examples{
  # Generate 10 observations from a lognormal distribution with parameters 
  # mean=5 and cv=2, and and 20 observations from a lognormal distribution with 
  # parameters mean=10 and cv=2.  Test the null hypothesis that the means of the 
  # two distributions are the same against the alternative that the mean for 
  # group 1 is less than the mean for group 2. 
  # (Note: the call to set.seed allows you to reproduce the same data 
  # (dat1 and dat2), and setting the argument seed=732 in the call to 
  # twoSamplePermutationTestLocation() lets you reproduce this example by 
  # getting the same sample from the permutation distribution).

  set.seed(256) 
  dat1 <- rlnormAlt(10, mean = 5, cv = 2) 
  dat2 <- rlnormAlt(20, mean = 10, cv = 2) 

  test.list <- twoSamplePermutationTestLocation(dat1, dat2, 
    alternative = "less", seed = 732) 

  # Print the results of the test 
  #------------------------------
  test.list 

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 mu.x-mu.y = 0
  #
  #Alternative Hypothesis:          True mu.x-mu.y is less than 0
  #
  #Test Name:                       Two-Sample Permutation Test
  #                                 Based on Differences in Means
  #                                 (Based on Sampling
  #                                 Permutation Distribution
  #                                 5000 Times)
  #
  #Estimated Parameter(s):          mean of x =  2.253439
  #                                 mean of y = 11.825430
  #
  #Data:                            x = dat1
  #                                 y = dat2
  #
  #Sample Sizes:                    nx = 10
  #                                 ny = 20
  #
  #Test Statistic:                  mean.x - mean.y = -9.571991
  #
  #P-value:                         0.001


  # Plot the results of the test 
  #-----------------------------
  dev.new()
  plot(test.list)

  #==========

  # The guidance document "Statistical Methods for Evaluating the Attainment of 
  # Cleanup Standards, Volume 3: Reference-Based Standards for Soils and Solid 
  # Media" (USEPA, 1994b, pp. 6.22-6.25) contains observations of 
  # 1,2,3,4-Tetrachlorobenzene (TcCB) in ppb at a Reference Area and a Cleanup Area.  
  # These data are stored in the data frame EPA.94b.tccb.df.  Use the 
  # two-sample permutation test to test for a difference in means between the 
  # two areas vs. the alternative that the mean in the Cleanup Area is greater.  
  # Do the same thing for the medians.
  #
  # The permutation test based on comparing means shows a significant differnce, 
  # while the one based on comparing medians does not.


  # First test for a difference in the means.
  #------------------------------------------

  mean.list <- with(EPA.94b.tccb.df, 
    twoSamplePermutationTestLocation(
      TcCB[Area=="Cleanup"], TcCB[Area=="Reference"], 
      alternative = "greater", seed = 47)) 

  mean.list

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 mu.x-mu.y = 0
  #
  #Alternative Hypothesis:          True mu.x-mu.y is greater than 0
  #
  #Test Name:                       Two-Sample Permutation Test
  #                                 Based on Differences in Means
  #                                 (Based on Sampling
  #                                 Permutation Distribution
  #                                 5000 Times)
  #
  #Estimated Parameter(s):          mean of x = 3.9151948
  #                                 mean of y = 0.5985106
  #
  #Data:                            x = TcCB[Area == "Cleanup"]  
  #                                 y = TcCB[Area == "Reference"]
  #
  #Sample Sizes:                    nx = 77
  #                                 ny = 47
  #
  #Test Statistic:                  mean.x - mean.y = 3.316684
  #
  #P-value:                         0.0206

  dev.new()
  plot(mean.list)


  #----------

  # Now test for a difference in the medians.
  #------------------------------------------

  median.list <- with(EPA.94b.tccb.df, 
    twoSamplePermutationTestLocation(
      TcCB[Area=="Cleanup"], TcCB[Area=="Reference"], 
      fcn = "median", alternative = "greater", seed = 47)) 

  median.list

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 mu.x-mu.y = 0
  #
  #Alternative Hypothesis:          True mu.x-mu.y is greater than 0
  #
  #Test Name:                       Two-Sample Permutation Test
  #                                 Based on Differences in Medians
  #                                 (Based on Sampling
  #                                 Permutation Distribution
  #                                 5000 Times)
  #
  #Estimated Parameter(s):          median of x = 0.43
  #                                 median of y = 0.54
  #
  #Data:                            x = TcCB[Area == "Cleanup"]  
  #                                 y = TcCB[Area == "Reference"]
  #
  #Sample Sizes:                    nx = 77
  #                                 ny = 47
  #
  #Test Statistic:                  median.x - median.y = -0.11
  #
  #P-value:                         0.936

  dev.new()
  plot(median.list)

  #==========

  # Clean up
  #---------
  rm(test.list, mean.list, median.list)
  graphics.off()
}
\keyword{htest}
\keyword{models}

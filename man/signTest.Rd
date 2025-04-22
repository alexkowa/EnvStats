\name{signTest}
\alias{signTest}
\alias{sign test}
\title{
  One-Sample or Paired-Sample Sign Test on a Median
}
\description{
  Estimate the median, test the null hypothesis that the median is equal to a 
  user-specified value based on the sign test, and create a confidence interval 
  for the median.
}
\usage{
  signTest(x, y = NULL, alternative = "two.sided", mu = 0, paired = FALSE, 
    ci.method = "interpolate", approx.conf.level = 0.95, 
    min.coverage = TRUE, lb = -Inf, ub = Inf)
}
\arguments{
  \item{x}{
  numeric vector of observations.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{y}{
  optional numeric vector of observations that are paired with the observations in 
  \code{x}.  The length of \code{y} must be the same as the length of \code{x}.  
  This argument is ignored if \code{paired=FALSE}, and must be supplied if 
  \code{paired=TRUE}.  The default value is \code{y=NULL}.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{alternative}{
  character string indicating the kind of alternative hypothesis.  The possible values 
  are \code{"two.sided"} (the default), \code{"greater"}, and \code{"less"}.
}
  \item{mu}{
  numeric scalar indicating the hypothesized value of the median.  The default value is 
  \code{mu=0}.
}
  \item{paired}{
  logical scalar indicating whether to perform a paired or one-sample sign test.  
  The possible values are \code{paired=FALSE} (the default; indicates a one-sample 
  sign test) and \code{paired=TRUE}.
}
  \item{ci.method}{
  character string indicating the method to use to construct the confidence interval.
  The possible values are \code{"interpolate"} (the default), \code{"exact"}, and
  \code{"normal.approx"}.  See the help file for \code{\link{eqnpar}} 
    for more information on these methods.
}
  \item{approx.conf.level}{
  a scalar between 0 and 1 indicating the desired confidence level of the 
  confidence interval for the population median.  The default value is 
  \code{appox.conf.level=0.95}.  The true confidence level usually will 
  not be exactly equal to \code{approx.conf.level}.  See the help file for 
  \code{\link{eqnpar}} for more information
}
  \item{min.coverage}{
  for the case when \code{ci.method="exact"}, a logical scalar indicating whether the
  confidence interval for the median should have a minimum coverage at least as great as the value
  of the argument \code{approx.conf.level}. The default value is 
  \code{min.coverage=TRUE}.  This argument is ignored if 
  \code{ci.method} is not equal to \code{"exact"}.
}
  \item{lb, ub}{
  scalars indicating lower and upper bounds on the distribution.  
  By default, \cr
  \code{lb=-Inf} and \code{ub=Inf}.  If you are constructing a confidence 
  interval for the median from a distribution that you know has a lower 
  bound other than \code{-Inf} (e.g., \code{0}), set \code{lb} to this value.  
  Similarly, if you know the distribution has an upper bound other than 
  \code{Inf}, set \code{ub} to this value.
}
}
\details{
  \bold{One-Sample Case} (\code{paired=FALSE}) \cr
  Let \eqn{\underline{x} = x_1, x_2, \ldots, x_n} be a vector of \eqn{n} 
  independent observations from one or more distributions that all have the 
  same median \eqn{\mu}.

  Consider the test of the null hypothesis:
  \deqn{H_0: \mu = \mu_0 \;\;\;\;\;\; (1)}
  The three possible alternative hypotheses are the upper one-sided alternative 
  (\code{alternative="greater"})
  \deqn{H_a: \mu > \mu_0 \;\;\;\;\;\; (2)}
  the lower one-sided alternative (\code{alternative="less"})
  \deqn{H_a: \mu < \mu_0 \;\;\;\;\;\; (3)}
  and the two-sided alternative (\code{alternative="two.sided"})
  \deqn{H_a: \mu \ne \mu_0 \;\;\;\;\;\; (4)}

  To perform the test of the null hypothesis (1) versus any of the three alternatives 
  (2)-(4), the sign test uses the test statistic \eqn{T} which is simply the number of 
  observations that are greater than \eqn{\mu_0} (Conover, 1980, p. 122; 
  van Belle et al., 2004, p. 256; Hollander and Wolfe, 1999, p. 60; 
  Lehmann, 1975, p. 120; Sheskin, 2011; Zar, 2010, p. 537).  Under the null 
  hypothesis, the distribution of \eqn{T} is a 
  \link[stats:Binomial]{binomial random variable} with 
  parameters \code{size=}\eqn{n} and \code{prob=0.5}.  Usually, however, cases for 
  which the observations are equal to \eqn{\mu_0} are discarded, so the distribution 
  of \eqn{T} is taken to be binomial with parameters \code{size=}\eqn{r} and 
  \code{prob=0.5}, where \eqn{r} denotes the number of observations not equal to 
  \eqn{\mu_0}.  The sign test only requires that the observations are independent 
  and that they all come from one or more distributions (not necessarily the same 
  ones) that all have the same population median.

  For a two-sided alternative hypothesis (Equation (4)), the p-value is computed as:
  \deqn{p = Pr(X_{r,0.5} \le r-m) + Pr(X_{r,0.5} > m) \;\;\;\;\;\; (5)}
  where \eqn{X_{r,p}} denotes a \link[stats:Binomial]{binomial} random variable 
  with parameters \code{size=}\eqn{r} and \code{prob=}\eqn{p}, and \eqn{m} 
  is defined by:
  \deqn{m = max(T, r-T) \;\;\;\;\;\; (6)}
 
  For a one-sided lower alternative hypothesis (Equation (3)), the p-value is 
  computed as:
  \deqn{p = Pr(X_{m,0.5} \le T) \;\;\;\;\;\; (7)}
  and for a one-sided upper alternative hypothesis (Equation (2)), the p-value is 
  computed as:
  \deqn{p = Pr(X_{m,0.5} \ge T) \;\;\;\;\;\; (8)}

  It is obvious that the sign test is simply a special case of the 
  \link[stats:binom.test]{binomial test} with \code{p=0.5}.

  \emph{Computing Confidence Intervals} \cr
  Based on the relationship between hypothesis tests and confidence intervals, 
  we can construct a confidence interval for the population median based on the 
  sign test (e.g., Hollander and Wolfe, 1999, p. 72; Lehmann, 1975, p. 182).  
  It turns out that this is equivalent to using the formulas for a nonparametric 
  confidence intervals for the 0.5 quantile (see \code{\link{eqnpar}}).
  \cr

  \bold{Paired-Sample Case} (\code{paired=TRUE}) \cr
  When the argument \code{paired=TRUE}, the arguments \code{x} and \code{y} are 
  assumed to have the same length, and the \eqn{n} differences
  \eqn{d_i = x_i - y_i, \;\; i = 1, 2, \ldots, n} are assumed to be independent 
  observations from distributions with the same median \eqn{\mu}.  The sign test 
  can then be applied to the differences.
}
\value{
  A list of class \code{"htestEnvStats"} containing the results of the hypothesis test.  
  See the help file for \code{\link{htestEnvStats.object}} for details.
}
\references{
  Conover, W.J. (1980). \emph{Practical Nonparametric Statistics}.  
  Second Edition. John Wiley and Sons, New York, p.122

  Hollander, M., and D.A. Wolfe. (1999). \emph{Nonparametric Statistical Methods}.  
  Second Edition.  John Wiley and Sons, New York, p.60.

  Lehmann, E.L. (1975).  \emph{Nonparametrics: Statistical Methods Based on Ranks}.  
  Holden-Day, Oakland, CA, p.120.

  Millard, S.P., and N.K. Neerchal. (2001).  
  \emph{Environmental Statistics with S-PLUS}.  CRC Press, Boca Raton, FL, pp.404--406.

  Sheskin, D.J. (2011).  \emph{Handbook of Parametric and Nonparametric 
  Statistical Procedures}  Fifth Edition. CRC Press, Boca Raton, FL.

  van Belle, G., L.D. Fisher, Heagerty, P.J., and Lumley, T. (2004).  
  \emph{Biostatistics: A Methodology for the Health Sciences}  2nd Edition.  
  John Wiley & Sons, New York.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}.  Fifth Edition.  
  Prentice-Hall, Upper Saddle River, NJ,
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
  median must be above the GWPS or SSL, or the 95th percentile must be above the 
  GWPS or SSL, or something else.  Often, the first interpretation is used.

  Hypothesis tests you can use to perform tests of location include:  
  \link[stats:t.test]{Student's t-test}, 
  \link[=oneSamplePermutationTest]{Fisher's randomization test}, the 
  \link[stats:wilcox.test]{Wilcoxon signed rank test}, 
  \link[=chenTTest]{Chen's modified t-test}, the 
  sign test, and a test based on a bootstrap confidence interval.  For a discussion 
  comparing the performance of these tests, see Millard and Neerchal (2001, pp.408-409).
}
\seealso{
  \code{\link{wilcox.test}}, \link{Hypothesis Tests}, \code{\link{eqnpar}}, 
  \code{\link{htestEnvStats.object}}.
}
\examples{
  # Generate 10 observations from a lognormal distribution with parameters 
  # meanlog=2 and sdlog=1.  The median of this distribution is e^2 (about 7.4). 
  # Test the null hypothesis that the true median is equal to 5 against the 
  # alternative that the true mean is greater than 5. 
  # (Note: the call to set.seed allows you to reproduce this example).

  set.seed(23) 
  dat <- rlnorm(10, meanlog = 2, sdlog = 1) 
  signTest(dat, mu = 5, lb = 0) 

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 median = 5
  #
  #Alternative Hypothesis:          True median is not equal to 5
  #
  #Test Name:                       Sign test
  #
  #Estimated Parameter(s):          median = 19.21717
  #
  #Data:                            dat
  #
  #Test Statistic:                  # Obs > median = 9
  #
  #P-value:                         0.02148438
  #
  #Confidence Interval for:         median
  #
  #Confidence Interval Method:      interpolate (Nyblom, 1992)
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Limit Rank(s):        2 3 9 8 
  #
  #Confidence Interval:             LCL =  7.000846
  #                                 UCL = 26.937725

  #----------

  # Redo the above example using an exact confidence interval
  # and specifying min.coverage=FALSE
  #----------------------------------------------------------

  signTest(dat, mu = 5, ci.method = "exact", min.coverage = FALSE, lb = 0) 

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 median = 5
  #
  #Alternative Hypothesis:          True median is not equal to 5
  #
  #Test Name:                       Sign test
  #
  #Estimated Parameter(s):          median = 19.21717
  #
  #Data:                            dat
  #
  #Test Statistic:                  # Obs > median = 9
  #
  #P-value:                         0.02148438
  #
  #Confidence Interval for:         median
  #
  #Confidence Interval Method:      exact
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                94.43359%
  #
  #Confidence Limit Rank(s):        1 8 
  #
  #Confidence Interval:             LCL =  4.784196
  #                                 UCL = 22.364849

  #----------

  # Clean up
  #---------
  rm(dat)

  #==========

  # The guidance document "Supplemental Guidance to RAGS: Calculating the 
  # Concentration Term" (USEPA, 1992d) contains an example of 15 observations 
  # of chromium concentrations (mg/kg) which are assumed to come from a 
  # lognormal distribution.  These data are stored in the vector 
  # EPA.92d.chromium.vec.  Here, we will use the sign test to test the null 
  # hypothesis that the median chromium concentration is less than or equal to 
  # 100 mg/kg vs. the alternative that it is greater than 100 mg/kg.  The 
  # estimated median is 110 mg/kg.  There are 8 out of 15 observations greater 
  # than 100 mg/kg, the p-value is equal to 0.5, and the lower 95% confidence 
  # limit is 40.53 mg/kg.

  summaryStats(EPA.92d.chromium.vec) 

  #                      N     Mean      SD Median Min  Max
  #EPA.92d.chromium.vec 15 175.4667 318.544    110  10 1300

  #----------

  signTest(EPA.92d.chromium.vec, mu = 100, alternative = "greater") 

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 median = 100
  #
  #Alternative Hypothesis:          True median is greater than 100
  #
  #Test Name:                       Sign test
  #
  #Estimated Parameter(s):          median = 110
  #
  #Data:                            EPA.92d.chromium.vec
  #
  #Test Statistic:                  # Obs > median = 8
  #
  #P-value:                         0.5
  #
  #Confidence Interval for:         median
  #
  #Confidence Interval Method:      interpolate (Nyblom, 1992)
  #
  #Confidence Interval Type:        lower
  #
  #Confidence Level:                95%
  #
  #Confidence Limit Rank(s):        4 5 NA NA
  #
  #Confidence Interval:             LCL =  40.53074
  #                                 UCL = Inf

  #----------

  # Redo the above example using the exact confidence interval and 
  # setting min.coverage=FALSE
  #---------------------------------------------------------------

  signTest(EPA.92d.chromium.vec, mu = 100, alternative = "greater", 
    ci.method = "exact", min.coverage = FALSE) 

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 median = 100
  #
  #Alternative Hypothesis:          True median is greater than 100
  #
  #Test Name:                       Sign test
  #
  #Estimated Parameter(s):          median = 110
  #
  #Data:                            EPA.92d.chromium.vec
  #
  #Test Statistic:                  # Obs > median = 8
  #
  #P-value:                         0.5
  #
  #Confidence Interval for:         median
  #
  #Confidence Interval Method:      exact
  #
  #Confidence Interval Type:        lower
  #
  #Confidence Level:                94.07654%
  #
  #Confidence Limit Rank(s):        5 
  #
  #Confidence Interval:             LCL =  41
  #                                 UCL = Inf

  #----------

  # Redo the above example using the exact confidence interval and 
  # setting min.coverage=TRUE
  #---------------------------------------------------------------

  signTest(EPA.92d.chromium.vec, mu = 100, alternative = "greater", 
    ci.method = "exact") 

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 median = 100
  #
  #Alternative Hypothesis:          True median is greater than 100
  #
  #Test Name:                       Sign test
  #
  #Estimated Parameter(s):          median = 110
  #
  #Data:                            EPA.92d.chromium.vec
  #
  #Test Statistic:                  # Obs > median = 8
  #
  #P-value:                         0.5
  #
  #Confidence Interval for:         median
  #
  #Confidence Interval Method:      exact
  #
  #Confidence Interval Type:        lower
  #
  #Confidence Level:                98.24219%
  #
  #Confidence Limit Rank(s):        4 
  #
  #Confidence Interval:             LCL =  36
  #                                 UCL = Inf
}
\keyword{htestEnvStats}
\keyword{models}

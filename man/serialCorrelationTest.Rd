\name{serialCorrelationTest}
\alias{serialCorrelationTest}
\alias{serialCorrelationTest.default}
\alias{serialCorrelationTest.lm}
\alias{serial correlation test}
\alias{serial correlation}
\title{
  Test for the Presence of Serial Correlation
}
\description{
  \code{serialCorrelationTest} is a generic function used to test for the 
  presence of lag-one serial correlation using either the rank 
  von Neumann ratio test, the normal approximation based on the Yule-Walker 
  estimate of lag-one correlation, or the normal approximation based on the 
  MLE of lag-one correlation.  The function invokes particular 
  \code{\link{methods}} which depend on the \code{\link{class}} of the first 
  argument. 

  Currently, there is a default method and a method for objects of class \code{"lm"}.
}
\usage{
  serialCorrelationTest(x, ...)

  \method{serialCorrelationTest}{default}(x, test = "rank.von.Neumann", 
    alternative = "two.sided", conf.level = 0.95, ...) 

  \method{serialCorrelationTest}{lm}(x, test = "rank.von.Neumann", 
    alternative = "two.sided", conf.level = 0.95, ...)
}
\arguments{
  \item{x}{
  numeric vector of observations, a numeric univariate time series of 
  class \code{"ts"}, or an object of class \code{"lm"}.  Undefined (\code{NaN}) and 
  infinite (\code{Inf}, \code{-Inf}) values are not allowed for \code{x} 
  when \code{x} is a numeric vector or time series, nor for the residuals 
  associated with \code{x} when \code{x} is an object of class \code{"lm"}.

  When \code{test="AR1.mle"}, missing (\code{NA}) values are allowed, otherwise 
  they are not allowed.  When \code{x} is a numeric vector of observations 
  or a numeric univariate time series of class \code{"ts"}, it must contain at least 
  3 non-missing values.  When \code{x} is an object of class \code{"lm"}, the 
  residuals must contain at least 3 non-missing values.

  Note:  when \code{x} is an object of class \code{"lm"}, the linear model 
  should have been fit using the argument \code{na.action=na.exclude} in the 
  call to \code{\link{lm}} in order to correctly deal with missing values.
}
  \item{test}{
  character string indicating which test to use.  The possible values are: \cr
  \code{"rank.von.Neumann"} (rank von Neumann ratio test; the default), \cr
  \code{"AR1.yw"} (z-test based on Yule-Walker lag-one estimate of correlation), and \cr
  \code{"AR1.mle"} (z-test based on MLE of lag-one correlation).  
}
  \item{alternative}{
  character string indicating the kind of alternative hypothesis.  The possible 
  values are \code{"two.sided"} (the default), \code{"greater"}, and \code{"less"}.
}
  \item{conf.level}{
  numeric scalar between 0 and 1 indicating the confidence level associated with 
  the confidence interval for the population lag-one autocorrelation.  The default 
  value is \code{conf.level=0.95}.
}
  \item{\dots}{
  optional arguments for possible future methods.  Currently not used.
}
}
\details{
  Let \eqn{\underline{x} = x_1, x_2, \ldots, x_n} denote \eqn{n} observations from a 
  stationary time series sampled at equispaced points in time with normal (Gaussian) 
  errors.  The function \code{serialCorrelationTest} tests the null hypothesis:
  \deqn{H_0: \rho_1 = 0 \;\;\;\;\;\; (1)}
  where \eqn{\rho_1} denotes the true lag-1 autocorrelation (also called the lag-1 
  serial correlation coefficient).  Actually, the null hypothesis is that the 
  lag-\eqn{k} autocorrelation is 0 for all values of \eqn{k} greater than 0 (i.e., 
  the time series is purely random).

  In the case when the argument \code{x} is a linear model, the function 
  \code{serialCorrelationTest} tests the null hypothesis (1) for the 
  residuals.

  The three possible alternative hypotheses are the upper one-sided alternative 
  (\code{alternative="greater"}):
  \deqn{H_a: \rho_1 > 0 \;\;\;\;\;\; (2)}
  the lower one-sided alternative (\code{alternative="less"}):
  \deqn{H_a: \rho_1 < 0 \;\;\;\;\;\; (3)}
  and the two-sided alternative: 
  \deqn{H_a: \rho_1 \ne 0 \;\;\;\;\;\; (4)}

  \bold{Testing the Null Hypothesis of No Lag-1 Autocorrelation} \cr
  There are several possible methods for testing the null hypothesis (1) versus any 
  of the three alternatives (2)-(4). The function \code{serialCorrelationTest} allows 
  you to use one of three possible tests:
  \itemize{
  \item The rank von Neuman ratio test.
  \item The test based on the normal approximation for the distribution of the 
    Yule-Walker estimate of lag-one correlation.
  \item The test based on the normal approximation for the distribution of the 
    maximum likelihood estimate (MLE) of lag-one correlation.
  }
  Each of these tests is described below.
  \cr

  \emph{Test Based on Yule-Walker Estimate} (\code{test="AR1.yw"}) \cr
  The Yule-Walker estimate of the lag-1 autocorrelation is given by:
  \deqn{\hat{\rho}_1 = \frac{\hat{\gamma}_1}{\hat{\gamma}_0} \;\;\;\;\;\; (5)}
  where
  \deqn{\hat{\gamma}_k = \frac{1}{n} \sum_{t=1}^{n-k} (x_t - \bar{x})(x_{t+k} - \bar{x}) \;\;\;\;\;\; (6)}
  is the estimate of the lag-\eqn{k} autocovariance.  
  (This estimator does not allow for missing values.)

  Under the null hypothesis (1), the estimator of lag-1 correlation in Equation (5) is 
  approximately distributed as a normal (Gaussian) random variable with mean 0 and 
  variance given by:
  \deqn{Var(\hat{\rho}_1) \approx \frac{1}{n} \;\;\;\;\;\; (7)}
  (Box and Jenkins, 1976, pp.34-35). Thus, the null hypothesis (1) can be tested 
  with the statistic
  \deqn{z = \sqrt{n} \hat{\rho_1} \;\;\;\;\;\; (8)}
  which is distributed approximately as a standard normal random variable under the 
  null hypothesis that the lag-1 autocorrelation is 0.
  \cr

  \emph{Test Based on the MLE} (\code{test="AR1.mle"}) \cr
  The function \code{serialCorrelationTest} the \R function \code{\link{arima}} to 
  compute the MLE of the lag-one autocorrelation and the estimated variance of this 
  estimator.  As for the test based on the Yule-Walker estimate, the z-statistic is 
  computed as the estimated lag-one autocorrelation divided by the square root of the 
  estimated variance.
  \cr
 
  \emph{Test Based on Rank von Neumann Ratio} (\code{test="rank.von.Neumann"}) \cr
  The null distribution of the serial correlation coefficient may be badly affected 
  by departures from normality in the underlying process (Cox, 1966; Bartels, 1977).  
  It is therefore a good idea to consider using a nonparametric test for randomness if 
  the normality of the underlying process is in doubt (Bartels, 1982). 

  Wald and Wolfowitz (1943) introduced the rank serial correlation coefficient, which 
  for lag-1 autocorrelation is simply the Yule-Walker estimate (Equation (5) above) 
  with the actual observations replaced with their ranks.
  
  von Neumann et al. (1941) introduced a test for randomness in the context of 
  testing for trend in the mean of a process. Their statistic is given by:
  \deqn{V = \frac{\sum_{i=1}^{n-1}(x_i - x_{i+1})^2}{\sum_{i=1}^n (x_i - \bar{x})^2} \;\;\;\;\;\; (9)}
  which is the ratio of the square of successive differences to the usual sums of 
  squared deviations from the mean.  This statistic is bounded between 0 and 4, and 
  for a purely random process is symmetric about 2.  Small values of this statistic 
  indicate possible positive autocorrelation, and large values of this statistics 
  indicate possible negative autocorrelation.  Durbin and Watson (1950, 1951, 1971) 
  proposed using this statistic in the context of checking the independence of 
  residuals from a linear regression model and provided tables for the distribution 
  of this statistic.  This statistic is therefore often called the 
  \dQuote{Durbin-Watson statistic} (Draper and Smith, 1998, p.181).

  The rank version of the von Neumann ratio statistic is given by:
  \deqn{V_{rank} = \frac{\sum_{i=1}^{n-1}(R_i - R_{i+1})^2}{\sum_{i=1}^n (R_i - \bar{R})^2} \;\;\;\;\;\; (10)}
  where \eqn{R_i} denotes the rank of the \eqn{i}'th observation (Bartels, 1982).  
  (This test statistic does not allow for missing values.)  In the absence of ties, 
  the denominator of this test statistic is equal to
  \deqn{\sum_{i=1}^n (R_i - \bar{R})^2 = \frac{n(n^2 - 1)}{12} \;\;\;\;\;\; (11)}
  The range of the \eqn{V_{rank}} test statistic is given by:
  \deqn{[\frac{12}{(n)(n+1)} , 4 - \frac{12}{(n)(n+1)}] \;\;\;\;\;\; (12)}
  if n is even, with a negligible adjustment if n is odd (Bartels, 1982), so 
  asymptotically the range is from 0 to 4, just as for the \eqn{V} test statistic in 
  Equation (9) above.

  Bartels (1982) shows that asymptotically, the rank von Neumann ratio statistic is a 
  linear transformation of the rank serial correlation coefficient, so any asymptotic 
  results apply to both statistics.

  For any fixed sample size \eqn{n}, the exact distribution of the \eqn{V_{rank}} 
  statistic in Equation (10) above can be computed by simply computing the value of 
  \eqn{V_{rank}} for all possible permutations of the serial order of the ranks.  
  Based on this exact distribution, Bartels (1982) presents a table of critical 
  values for the numerator of the RVN statistic for sample sizes between 4 and 10.

  Determining the exact distribution of \eqn{V_{rank}} becomes impractical as the 
  sample size increases.  For values of n between 10 and 100, Bartels (1982) 
  approximated the distribution of \eqn{V_{rank}} by a 
  \link[stats:Beta]{beta distribution} over the range 0 to 4 with shape parameters 
  \code{shape1=}\eqn{\nu} and \code{shape2=}\eqn{\omega} and:
  \deqn{\nu = \omega = \frac{5n(n+1)(n-1)^2}{2(n-2)(5n^2 - 2n - 9)} - \frac{1}{2} \;\;\;\;\;\; (13)}
  Bartels (1982) checked this approximation by simulating the distribution of 
  \eqn{V_{rank}} for \eqn{n=25} and \eqn{n=50} and comparing the empirical quantiles 
  at \eqn{0.005}, \eqn{0.01}, \eqn{0.025}, \eqn{0.05}, and \eqn{0.1} with the 
  approximated quantiles based on the beta distribution.  He found that the quantiles 
  agreed to 2 decimal places for eight of the 10 values, and differed by \eqn{0.01} 
  for the other two values.

  \bold{Note}: The definition of the \link[stats:Beta]{beta distribution} assumes the 
  random variable ranges from 0 to 1.  This definition can be generalized as follows.  
  Suppose the random variable \eqn{Y} has a beta distribution over the range 
  \eqn{a \le y \le b}, with shape parameters \eqn{\nu} and \eqn{\omega}.  Then the 
  random variable \eqn{X} defined as:
  \deqn{X = \frac{Y-a}{b-a} \;\;\;\;\;\; (14)}
  has the \dQuote{standard beta distribution} as described in the help file for Beta 
  (Johnson et al., 1995, p.210).

  Bartels (1982) shows that asymptotically, \eqn{V_{rank}} has normal distribution 
  with mean 2 and variance \eqn{4/n}, but notes that a slightly better approximation 
  is given by using a variance of \eqn{20/(5n + 7)}.

  To test the null hypothesis (1) when \code{test="rank.von.Neumann"}, the function \cr
  \code{serialCorrelationTest} does the following:
  \itemize{
  \item When the sample size is between 3 and 10, the exact distribution of \eqn{V_{rank}} 
  is used to compute the p-value.

  \item When the sample size is between 11 and 100, the beta approximation to the 
  distribution of \eqn{V_{rank}} is used to compute the p-value.

  \item When the sample size is larger than 100, the normal approximation to the 
  distribution of \eqn{V_{rank}} is used to compute the p-value.  
  (This uses the variance \eqn{20/(5n + 7)}.)
  }
 
  When ties are present in the observations and midranks are used for the tied 
  observations, the distribution of the \eqn{V_{rank}} statistic based on the 
  assumption of no ties is not applicable.  If the number of ties is small, however, 
  they may not grossly affect the assumed p-value.

  When ties are present, the function \code{serialCorrelationTest} issues a warning.  
  When the sample size is between 3 and 10, the p-value is computed based on 
  rounding up the computed value of \eqn{V_{rank}} to the nearest possible value 
  that could be observed in the case of no ties.
  \cr
 
  \bold{Computing a Confidence Interval for the Lag-1 Autocorrelation} \cr
  The function \code{serialCorrelationTest} computes an approximate 
  \eqn{100(1-\alpha)\%} confidence interval for the lag-1 autocorrelation as follows:
  \deqn{[\hat{\rho}_1 - z_{1-\alpha/2}\hat{\sigma}_{\hat{\rho}_1},  \hat{\rho}_1 + z_{1-\alpha/2}\hat{\sigma}_{\hat{\rho}_1}] \;\;\;\;\;\; (15)}
  where \eqn{\hat{\sigma}_{\hat{\rho}_1}} denotes the estimated standard deviation of 
  the estimated of lag-1 autocorrelation and \eqn{z_p} denotes the \eqn{p}'th quantile 
  of the standard \link[stats:Normal]{normal distribution}.

  When \code{test="AR1.yw"} or \code{test="rank.von.Neumann"}, the Yule-Walker 
  estimate of lag-1 autocorrelation is used and the variance of the estimated 
  lag-1 autocorrelation is approximately:
  \deqn{Var(\hat{\rho}_1) \approx \frac{1}{n} (1 - \rho_1^2) \;\;\;\;\;\; (16)}
  (Box and Jenkins, 1976, p.34), so
  \deqn{\hat{\sigma}_{\hat{\rho}_1} = \sqrt{\frac{1 - \hat{\rho}_1^2}{n}} \;\;\;\;\;\; (17)}
  When \code{test="AR1.mle"}, the MLE of the lag-1 autocorrelation is used, and its 
  standard deviation is estimated with the square root of the estimated variance 
  returned by \code{\link{arima}}.
}
\value{
  A list of class \code{"htestEnvStats"} containing the results of the hypothesis test.  
  See the help file for \code{\link{htestEnvStats.object}} for details.
}
\references{
  Bartels, R. (1982).  The Rank Version of von Neumann's Ratio Test for Randomness.  
  \emph{Journal of the American Statistical Association} \bold{77}(377), 40--46.

  Berthouex, P.M., and L.C. Brown. (2002).  
  \emph{Statistics for Environmental Engineers}.  Second Edition.  
  Lewis Publishers, Boca Raton, FL.
 
  Box, G.E.P., and G.M. Jenkins. (1976).  
  \emph{Time Series Analysis: Forecasting and Control}.  Prentice Hall, 
  Englewood Cliffs, NJ, Chapter 2.

  Cox, D.R. (1966).  The Null Distribution of the First Serial Correlation Coefficient.  
  \emph{Biometrika} \bold{53}, 623--626.

  Draper, N., and H. Smith. (1998).  \emph{Applied Regression Analysis}.  
  Third Edition. John Wiley and Sons, New York, pp.69-70;181-192.

  Durbin, J., and G.S. Watson. (1950).  Testing for Serial Correlation in Least 
  Squares Regression I.  \emph{Biometrika} \bold{37}, 409--428.

  Durbin, J., and G.S. Watson. (1951).  Testing for Serial Correlation in Least 
  Squares Regression II.  \emph{Biometrika} \bold{38}, 159--178.

  Durbin, J., and G.S. Watson. (1971). Testing for Serial Correlation in Least Squares 
  Regression III.  \emph{Biometrika} \bold{58}, 1--19.

  Helsel, D.R., and R.M. Hirsch. (1992).  \emph{Statistical Methods in Water 
  Resources Research}.  Elsevier, New York, NY, pp.250--253.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995).  \emph{Continuous Univariate 
  Distributions, Volume 2}.  Second Edition.  John Wiley and Sons, New York, 
  Chapter 25.

  Knoke, J.D. (1975).  Testing for Randomness Against Autocorrelation Alternatives:  
  The Parametric Case.  \emph{Biometrika} \bold{62}, 571--575.

  Knoke, J.D. (1977).  Testing for Randomness Against Autocorrelation Alternatives:  
  Alternative Tests.  \emph{Biometrika} \bold{64}, 523--529.

  Lehmann, E.L. (1975).  \emph{Nonparametrics:  Statistical Methods Based on Ranks}.  
  Holden-Day, Oakland, CA, 457pp.

  von Neumann, J., R.H. Kent, H.R. Bellinson, and B.I. Hart. (1941).  The Mean Square 
  Successive Difference.  \emph{Annals of Mathematical Statistics} \bold{12}(2), 
  153--162.

  Wald, A., and J. Wolfowitz. (1943).  An Exact Test for Randomness in the 
  Non-Parametric Case Based on Serial Correlation.  \emph{Annals of Mathematical 
  Statistics} \bold{14}, 378--388.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Data collected over time on the same phenomenon are called a time series.  
  A time series is usually modeled as a single realization of a stochastic process; 
  that is, if we could go back in time and repeat the experiment, we would get 
  different results that would vary according to some probabilistic law.  
  The simplest kind of time series is a stationary time series, in which the mean 
  value is constant over time, the variability of the observations is constant over 
  time, etc.  That is, the probability distribution associated with each future 
  observation is the same.

  A common concern in applying standard statistical tests to time series data is 
  the assumption of independence.  Most conventional statistical hypothesis tests 
  assume the observations are independent, but data collected sequentially in time 
  may not satisfy this assumption.  For example, high observations may tend to 
  follow high observations (positive serial correlation), or low observations may 
  tend to follow high observations (negative serial correlation).  One way to 
  investigate the assumption of independence is to estimate the lag-one serial 
  correlation and test whether it is significantly different from 0.

  The null distribution of the serial correlation coefficient may be badly affected 
  by departures from normality in the underlying process (Cox, 1966; Bartels, 1977).  
  It is therefore a good idea to consider using a nonparametric test for randomness 
  if the normality of the underlying process is in doubt (Bartels, 1982).  
  Knoke (1977) showed that under normality, the test based on the rank serial 
  correlation coefficient (and hence the test based on the rank von Neumann ratio 
  statistic) has asymptotic relative efficiency of 0.91 with respect to using the 
  test based on the ordinary serial correlation coefficient against the alternative 
  of first-order autocorrelation.

  Bartels (1982) performed an extensive simulation study of the power of the 
  rank von Neumann ratio test relative to the standard von Neumann ratio test 
  (based on the statistic in Equation (9) above) and the runs test 
  (Lehmann, 1975, 313-315).  He generated a first-order autoregressive process for 
  sample sizes of 10, 25, and 50, using 6 different parent distributions: normal, 
  Cauchy, contaminated normal, Johnson, Stable, and exponential.  Values of 
  lag-1 autocorrelation ranged from -0.8 to 0.8.  Bartels (1982) found three 
  important results:
  \itemize{
  \item The rank von Neumann ratio test is far more powerful than the runs test.

  \item For the normal process, the power of the rank von Neumann ratio test was 
  never less than 89\% of the power of the standard von Neumann ratio test.

  \item For non-normal processes, the rank von Neumann ratio test was often much 
  more powerful than of the standard von Neumann ratio test.
}
}
\seealso{
  \code{\link{htestEnvStats.object}}, \code{\link{acf}}, \code{\link{ar}}, 
  \code{\link{arima}}, \code{\link{arima.sim}}, 
  \code{\link{ts.plot}}, \code{\link{plot.ts}},  
  \code{\link{lag.plot}}, \link{Hypothesis Tests}.
}
\examples{
  # Generate a purely random normal process, then use serialCorrelationTest 
  # to test for the presence of correlation. 
  # (Note: the call to set.seed allows you to reproduce this example.) 

  set.seed(345) 
  x <- rnorm(100) 

  # Look at the data
  #-----------------
  dev.new()
  ts.plot(x)

  dev.new()
  acf(x)

  # Test for serial correlation
  #----------------------------
  serialCorrelationTest(x) 

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 rho = 0
  #
  #Alternative Hypothesis:          True rho is not equal to 0
  #
  #Test Name:                       Rank von Neumann Test for
  #                                 Lag-1 Autocorrelation
  #                                 (Beta Approximation)
  #
  #Estimated Parameter(s):          rho = 0.02773737
  #
  #Estimation Method:               Yule-Walker
  #
  #Data:                            x
  #
  #Sample Size:                     100
  #
  #Test Statistic:                  RVN = 1.929733
  #
  #P-value:                         0.7253405
  #
  #Confidence Interval for:         rho
  #
  #Confidence Interval Method:      Normal Approximation
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = -0.1681836
  #                                 UCL =  0.2236584

  # Clean up
  #---------
  rm(x)
  graphics.off()

  #==========

  # Now use the R function arima.sim to generate an AR(1) process with a 
  # lag-1 autocorrelation of 0.8, then test for autocorrelation.

  set.seed(432) 
  y <- arima.sim(model = list(ar = 0.8), n = 100) 

  # Look at the data
  #-----------------
  dev.new()
  ts.plot(y)

  dev.new()
  acf(y)

  # Test for serial correlation
  #----------------------------
  serialCorrelationTest(y)

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 rho = 0
  #
  #Alternative Hypothesis:          True rho is not equal to 0
  #
  #Test Name:                       Rank von Neumann Test for
  #                                 Lag-1 Autocorrelation
  #                                 (Beta Approximation)
  #
  #Estimated Parameter(s):          rho = 0.835214
  #
  #Estimation Method:               Yule-Walker
  #
  #Data:                            y
  #
  #Sample Size:                     100
  #
  #Test Statistic:                  RVN = 0.3743174
  #
  #P-value:                         0
  #
  #Confidence Interval for:         rho
  #
  #Confidence Interval Method:      Normal Approximation
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 0.7274307
  #                                 UCL = 0.9429973

  #----------

  # Clean up
  #---------
  rm(y)
  graphics.off()

  #==========

  # The data frame Air.df contains information on ozone (ppb^1/3), 
  # radiation (langleys), temperature (degrees F), and wind speed (mph) 
  # for 153 consecutive days between May 1 and September 30, 1973.  
  # First test for serial correlation in (the cube root of) ozone.  
  # Note that we must use the test based on the MLE because the time series 
  # contains missing values.  Serial correlation appears to be present.  
  # Next fit a linear model that includes the predictor variables temperature, 
  # radiation, and wind speed, and test for the presence of serial correlation 
  # in the residuals.  There is no evidence of serial correlation.

  # Look at the data
  #-----------------

  Air.df
  #              ozone radiation temperature wind
  #05/01/1973 3.448217       190          67  7.4
  #05/02/1973 3.301927       118          72  8.0
  #05/03/1973 2.289428       149          74 12.6
  #05/04/1973 2.620741       313          62 11.5
  #05/05/1973       NA        NA          56 14.3
  #...
  #09/27/1973       NA       145          77 13.2
  #09/28/1973 2.410142       191          75 14.3
  #09/29/1973 2.620741       131          76  8.0
  #09/30/1973 2.714418       223          68 11.5

  #----------

  # Test for serial correlation
  #----------------------------

  with(Air.df, 
    serialCorrelationTest(ozone, test = "AR1.mle"))

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 rho = 0
  #
  #Alternative Hypothesis:          True rho is not equal to 0
  #
  #Test Name:                       z-Test for
  #                                 Lag-1 Autocorrelation
  #                                 (Wald Test Based on MLE)
  #
  #Estimated Parameter(s):          rho = 0.5641616
  #
  #Estimation Method:               Maximum Likelihood
  #
  #Data:                            ozone
  #
  #Sample Size:                     153
  #
  #Number NA/NaN/Inf's:             37
  #
  #Test Statistic:                  z = 7.586952
  #
  #P-value:                         3.28626e-14
  #
  #Confidence Interval for:         rho
  #
  #Confidence Interval Method:      Normal Approximation
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 0.4184197
  #                                 UCL = 0.7099034

  #----------

  # Next fit a linear model that includes the predictor variables temperature, 
  # radiation, and wind speed, and test for the presence of serial correlation 
  # in the residuals.  Note setting the argument na.action = na.exclude in the 
  # call to lm to correctly deal with missing values.
  #----------------------------------------------------------------------------

  lm.ozone <- lm(ozone ~ radiation + temperature + wind + 
    I(temperature^2) + I(wind^2), 
    data = Air.df, na.action = na.exclude) 


  # Now test for serial correlation in the residuals.
  #--------------------------------------------------

  serialCorrelationTest(lm.ozone, test = "AR1.mle") 

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 rho = 0
  #
  #Alternative Hypothesis:          True rho is not equal to 0
  #
  #Test Name:                       z-Test for
  #                                 Lag-1 Autocorrelation
  #                                 (Wald Test Based on MLE)
  #
  #Estimated Parameter(s):          rho = 0.1298024
  #
  #Estimation Method:               Maximum Likelihood
  #
  #Data:                            Residuals
  #
  #Data Source:                     lm.ozone
  #
  #Sample Size:                     153
  #
  #Number NA/NaN/Inf's:             42
  #
  #Test Statistic:                  z = 1.285963
  #
  #P-value:                         0.1984559
  #
  #Confidence Interval for:         rho
  #
  #Confidence Interval Method:      Normal Approximation
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = -0.06803223
  #                                 UCL =  0.32763704

  # Clean up
  #---------
  rm(lm.ozone)

}
\keyword{htestEnvStats}
\keyword{univar}
\keyword{models}

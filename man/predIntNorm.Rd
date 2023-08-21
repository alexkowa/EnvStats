\name{predIntNorm}
\alias{predIntNorm}
\title{
  Prediction Interval for a Normal Distribution
}
\description{
  Estimate the mean and standard deviation of a
  \link[stats:Normal]{normal distribution}, and
  construct a prediction interval for the next \eqn{k} observations or
  next set of \eqn{k} means.
}
\usage{
  predIntNorm(x, n.mean = 1, k = 1, method = "Bonferroni",
    pi.type = "two-sided", conf.level = 0.95)
}
\arguments{
  \item{x}{
  a numeric vector of observations, or an object resulting from a call to an estimating
  function that assumes a normal (Gaussian) distribution (e.g., \code{\link{enorm}},
  \code{\link{eqnorm}}, \code{\link{enormCensored}}, etc.).  If \code{x} is a
  numeric vector, missing (\code{NA}), undefined (\code{NaN}), and
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{n.mean}{
  positive integer specifying the sample size associated with the \eqn{k} future averages.
  The default value is \code{n.mean=1} (i.e., individual observations).  Note that all
  future averages must be based on the same sample size.
}
  \item{k}{
  positive integer specifying the number of future observations or averages the
  prediction interval should contain with confidence level \code{conf.level}.
  The default value is \code{k=1}.
}
  \item{method}{
  character string specifying the method to use if the number of future observations
  (\code{k}) is greater than 1.  The possible values are \code{method="Bonferroni"}
  (approximate method based on Bonferonni inequality; the default), and \cr
  \code{method="exact"} (exact method due to Dunnett, 1955).  See the DETAILS section
  of \code{\link{predIntNormK}} for more information.
  This argument is ignored if \code{k=1}.
}
  \item{pi.type}{
  character string indicating what kind of prediction interval to compute.
  The possible values are \code{pi.type="two-sided"} (the default), \code{pi.type="lower"},
  and \code{pi.type="upper"}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level of the prediction interval.
  The default value is \code{conf.level=0.95}.
}
}
\details{
  \emph{What is a Prediction Interval?} \cr
  A prediction interval for some population is an interval on the real line constructed
  so that it will contain \eqn{k} future observations or averages from that population
  with some specified probability \eqn{(1-\alpha)100\%}, where
  \eqn{0 < \alpha < 1} and \eqn{k} is some pre-specified positive integer.
  The quantity \eqn{(1-\alpha)100\%} is called
  the confidence coefficient or confidence level associated with the prediction
  interval.
  \cr

  \emph{The Form of a Prediction Interval} \cr
  Let \eqn{\underline{x} = x_1, x_2, \ldots, x_n} denote a vector of \eqn{n}
  observations from a \link[stats:Normal]{normal distribution} with parameters
  \code{mean=}\eqn{\mu} and \code{sd=}\eqn{\sigma}.  Also, let \eqn{m} denote the
  sample size associated with the \eqn{k} future averages (i.e., \code{n.mean=}\eqn{m}).
  When \eqn{m=1}, each average is really just a single observation, so in the rest of
  this help file the term \dQuote{averages} will replace the phrase
  \dQuote{observations or averages}.

  For a normal distribution, the form of a two-sided \eqn{(1-\alpha)100\%} prediction
  interval is:
  \deqn{[\bar{x} - Ks, \bar{x} + Ks] \;\;\;\;\;\; (1)}
  where \eqn{\bar{x}} denotes the sample mean:
  \deqn{\bar{x} = \frac{1}{n} \sum_{i=1}^n x_i \;\;\;\;\;\; (2)}
  \eqn{s} denotes the sample standard deviation:
  \deqn{s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\;\;\; (3)}
  and \eqn{K} denotes a constant that depends on the sample size \eqn{n}, the
  confidence level, the number of future averages \eqn{k}, and the
  sample size associated with the future averages, \eqn{m}.  Do not confuse the
  constant \eqn{K} (uppercase K) with the number of future averages \eqn{k}
  (lowercase k).  The symbol \eqn{K} is used here to be consistent with the
  notation used for tolerance intervals (see \code{\link{tolIntNorm}}).

  Similarly, the form of a one-sided lower prediction interval is:
  \deqn{[\bar{x} - Ks, \infty] \;\;\;\;\;\; (4)}
  and the form of a one-sided upper prediction interval is:
  \deqn{[-\infty, \bar{x} + Ks] \;\;\;\;\;\; (5)}
  but \eqn{K} differs for one-sided versus two-sided prediction intervals.
  The derivation of the constant \eqn{K} is explained in the help file for
  \code{\link{predIntNormK}}.
  \cr

  \emph{A Prediction Interval is a Random Interval} \cr
  A prediction interval is a \emph{random} interval; that is, the lower and/or
  upper bounds are random variables computed based on sample statistics in the
  baseline sample.  Prior to taking one specific baseline sample, the probability
  that the prediction interval will contain the next \eqn{k} averages is
  \eqn{(1-\alpha)100\%}.  Once a specific baseline sample is taken and the
  prediction interval based on that sample is computed, the probability that that
  prediction interval will contain the next \eqn{k} averages is not necessarily
  \eqn{(1-\alpha)100\%}, but it should be close.

  If an experiment is repeated \eqn{N} times, and for each experiment:
  \enumerate{
  \item A sample is taken and a \eqn{(1-\alpha)100\%} prediction interval for \eqn{k=1}
  future observation is computed, and
  \item One future observation is generated and compared to the prediction interval,
  }
  then the number of prediction intervals that actually contain the future observation
  generated in step 2 above is a \link[stats:Binomial]{binomial random variable}
  with parameters \code{size=}\eqn{N} and \code{prob=}\eqn{(1-\alpha)100\%}.

  If, on the other hand, only one baseline sample is taken and only one prediction
  interval for \eqn{k=1} future observation is computed, then the number of
  future observations out of a total of \eqn{N} future observations that will be
  contained in that one prediction interval is a binomial random variable with
  parameters \code{size=}\eqn{N} and \code{prob=}\eqn{(1-\alpha^*)100\%}, where
  \eqn{\alpha^*} depends on the true population parameters and the computed
  bounds of the prediction interval.
}
\value{
  If \code{x} is a numeric vector, \code{predIntNorm} returns a list of class
  \code{"estimate"} containing the estimated parameters, the prediction interval,
  and other information.  See the help file for \cr
  \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function,
  \code{predIntNorm} returns a list whose class is the same as \code{x}.
  The list contains the same components as \code{x}, as well as a component called
  \code{interval} containing the prediction interval information.
  If \code{x} already has a component called \code{interval}, this component is
  replaced with the prediction interval information.
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). \emph{Statistics for Environmental Engineers}.
  Lewis Publishers, Boca Raton.

  Dunnett, C.W. (1955). A Multiple Comparisons Procedure for Comparing Several Treatments
  with a Control. \emph{Journal of the American Statistical Association} \bold{50}, 1096-1121.

  Dunnett, C.W. (1964). New Tables for Multiple Comparisons with a Control.
  \emph{Biometrics} \bold{20}, 482-491.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009).
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.
  John Wiley & Sons, Hoboken.

  Hahn, G.J. (1969). Factors for Calculating Two-Sided Prediction Intervals for
  Samples from a Normal Distribution.
  \emph{Journal of the American Statistical Association} \bold{64}(327), 878-898.

  Hahn, G.J. (1970a). Additional Factors for Calculating Prediction Intervals for
  Samples from a Normal Distribution.
  \emph{Journal of the American Statistical Association} \bold{65}(332), 1668-1676.

  Hahn, G.J. (1970b). Statistical Intervals for a Normal Population, Part I: Tables,
  Examples and Applications. \emph{Journal of Quality Technology} \bold{2}(3), 115-125.

  Hahn, G.J. (1970c). Statistical Intervals for a Normal Population, Part II:
  Formulas, Assumptions, Some Derivations. \emph{Journal of Quality Technology}
  \bold{2}(4), 195-206.

  Hahn, G.J., and W.Q. Meeker. (1991). \emph{Statistical Intervals: A Guide for Practitioners}.
  John Wiley and Sons, New York.

  Hahn, G., and W. Nelson. (1973). A Survey of Prediction Intervals and Their Applications.
  \emph{Journal of Quality Technology} \bold{5}, 178-188.

  Helsel, D.R., and R.M. Hirsch. (1992). \emph{Statistical Methods in Water Resources Research}.
  Elsevier, New York.

  Helsel, D.R., and R.M. Hirsch. (2002). \emph{Statistical Methods in Water Resources}.
  Techniques of Water Resources Investigations, Book 4, chapter A3. U.S. Geological Survey.
  (available on-line at:  \url{https://pubs.usgs.gov/tm/04/a03/tm4a3.pdf}).

  Krishnamoorthy K., and T. Mathew. (2009).
  \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}.
  John Wiley and Sons, Hoboken.

  Millard, S.P., and Neerchal, N.K. (2001). \emph{Environmental Statistics with S-PLUS}.
  CRC Press, Boca Raton, Florida.

  Miller, R.G. (1981a). \emph{Simultaneous Statistical Inference}. McGraw-Hill, New York.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
  U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Prediction and tolerance intervals have long been applied to quality control and
  life testing problems (Hahn, 1970b,c; Hahn and Nelson, 1973; Krishnamoorthy and Mathew, 2009).
  In the context of environmental statistics, prediction intervals are useful for
  analyzing data from groundwater detection monitoring programs at hazardous and
  solid waste facilities (e.g., Gibbons et al., 2009; Millard and Neerchal, 2001;
  USEPA, 2009).
}
\seealso{
  \code{\link{predIntNormK}}, \code{\link{predIntNormSimultaneous}},
  \code{\link{predIntLnorm}}, \code{\link{tolIntNorm}},
  \link{Normal}, \cr
  \code{\link{estimate.object}}, \code{\link{enorm}},
  \code{\link{eqnorm}}.
}
\examples{
  # Generate 20 observations from a normal distribution with parameters
  # mean=10 and sd=2, then create a two-sided 95% prediction interval for
  # the next observation.
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(47)
  dat <- rnorm(20, mean = 10, sd = 2)
  predIntNorm(dat)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Normal
  #
  #Estimated Parameter(s):          mean = 9.792856
  #                                 sd   = 1.821286
  #
  #Estimation Method:               mvue
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Prediction Interval Method:      exact
  #
  #Prediction Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Number of Future Observations:   1
  #
  #Prediction Interval:             LPL =  5.886723
  #                                 UPL = 13.698988

  #----------

  # Using the same data from the last example, create a one-sided
  # upper 99% prediction limit for the next 3 averages of order 2
  # (i.e., each of the 3 future averages is based on a sample size
  # of 2 future observations).

  predIntNorm(dat, n.mean = 2, k = 3, conf.level = 0.99,
    pi.type = "upper")

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Normal
  #
  #Estimated Parameter(s):          mean = 9.792856
  #                                 sd   = 1.821286
  #
  #Estimation Method:               mvue
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Prediction Interval Method:      Bonferroni
  #
  #Prediction Interval Type:        upper
  #
  #Confidence Level:                99%
  #
  #Number of Future Averages:       3
  #
  #Sample Size for Averages:        2
  #
  #Prediction Interval:             LPL =     -Inf
  #                                 UPL = 13.90537

  #----------

  # Compare the result above that is based on the Bonferroni method
  # with the exact method

  predIntNorm(dat, n.mean = 2, k = 3, conf.level = 0.99,
    pi.type = "upper", method = "exact")$interval$limits["UPL"]

  #     UPL
  #13.89272

  #----------

  # Clean up
  rm(dat)

  #--------------------------------------------------------------------

  # Example 18-1 of USEPA (2009, p.18-9) shows how to construct a 95%
  # prediction interval for 4 future observations assuming a
  # normal distribution based on arsenic concentrations (ppb) in
  # groundwater at a solid waste landfill.  There were 4 years of
  # quarterly monitoring, and years 1-3 are considered background.
  # The question to be answered is whether there is evidence of
  # contamination in year 4.

  # The data for this example is stored in EPA.09.Ex.18.1.arsenic.df.

  EPA.09.Ex.18.1.arsenic.df

  #   Year Sampling.Period Arsenic.ppb
  #1     1      Background        12.6
  #2     1      Background        30.8
  #3     1      Background        52.0
  #4     1      Background        28.1
  #5     2      Background        33.3
  #6     2      Background        44.0
  #7     2      Background         3.0
  #8     2      Background        12.8
  #9     3      Background        58.1
  #10    3      Background        12.6
  #11    3      Background        17.6
  #12    3      Background        25.3
  #13    4      Compliance        48.0
  #14    4      Compliance        30.3
  #15    4      Compliance        42.5
  #16    4      Compliance        15.0

  As.bkgd <- with(EPA.09.Ex.18.1.arsenic.df,
    Arsenic.ppb[Sampling.Period == "Background"])
  As.cmpl <- with(EPA.09.Ex.18.1.arsenic.df,
    Arsenic.ppb[Sampling.Period == "Compliance"])

  # A Shapiro-Wilks goodness-of-fit test for normality indicates
  # there is no evidence to reject the assumption of normality
  # for the background data:

  gofTest(As.bkgd)

  #Results of Goodness-of-Fit Test
  #-------------------------------
  #
  #Test Method:                     Shapiro-Wilk GOF
  #
  #Hypothesized Distribution:       Normal
  #
  #Estimated Parameter(s):          mean = 27.51667
  #                                 sd   = 17.10119
  #
  #Estimation Method:               mvue
  #
  #Data:                            As.bkgd
  #
  #Sample Size:                     12
  #
  #Test Statistic:                  W = 0.94695
  #
  #Test Statistic Parameter:        n = 12
  #
  #P-value:                         0.5929102
  #
  #Alternative Hypothesis:          True cdf does not equal the
  #                                 Normal Distribution.

  # Here is the one-sided 95% upper prediction limit:

  UPL <- predIntNorm(As.bkgd, k = 4,
    pi.type = "upper")$interval$limits["UPL"]
  UPL
  #     UPL
  #73.67237

  # Are any of the compliance observations above the prediction limit?

  any(As.cmpl > UPL)
  #[1] FALSE

  #==========

  # Cleanup
  #--------

  rm(As.bkgd, As.cmpl, UPL)
}
\keyword{ distribution }
\keyword{ htest }

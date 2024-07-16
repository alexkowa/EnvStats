\name{eqpois}
\alias{eqpois}
\title{
  Estimate Quantiles of a Poisson Distribution
}
\description{
  Estimate quantiles of an \link[stats:Poisson]{Poisson distribution}, and
  optionally contruct a confidence interval for a quantile.
}
\usage{
  eqpois(x, p = 0.5, method = "mle/mme/mvue", ci = FALSE, ci.method = "exact",
    ci.type = "two-sided", conf.level = 0.95, digits = 0)
}
\arguments{
  \item{x}{
  a numeric vector of observations, or an object resulting from a call to an
  estimating function that assumes an Poisson distribution
  (e.g., \code{\link{epois}}).  If \code{ci=TRUE} then \code{x} must be a
  numeric vector of observations.  If \code{x} is a numeric vector,
  missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
  values are allowed but will be removed.
}
  \item{p}{
  numeric vector of probabilities for which quantiles will be estimated.
  All values of \code{p} must be between 0 and 1.  When \code{ci=TRUE}, \code{p}
  must be a scalar.  The default value is \code{p=0.5}.
}
  \item{method}{
  character string specifying the method to use to estimate the mean.  Currently the
  only possible value is \code{"mle/mme/mvue"}
  (maximum likelihood/method of moments/minimum variance unbiased;
  the default).  See the DETAILS section of the help file for \code{\link{epois}} for
  more information.
}
  \item{ci}{
  logical scalar indicating whether to compute a confidence interval for the
  specified quantile.  The default value is \code{ci=FALSE}.
}
  \item{ci.method}{
  character string indicating what method to use to construct the confidence
  interval for the quantile.  The only possible value is \code{"exact"}
  (exact method; the default).  See the DETAILS section for more information.
}
  \item{ci.type}{
  character string indicating what kind of confidence interval to compute.  The
  possible values are \code{"two-sided"} (the default), \code{"lower"}, and
  \code{"upper"}.  This argument is ignored if \code{ci=FALSE}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level of the confidence interval.
  The default value is \code{conf.level=0.95}. This argument is ignored if
  \code{ci=FALSE}.
}
  \item{digits}{
  an integer indicating the number of decimal places to round to when printing out
  the value of \code{100*p}. The default value is \code{digits=0}.
}
}
\details{
  The function \code{eqpois} returns estimated quantiles as well as
  the estimate of the mean parameter.

  \bold{Estimation} \cr
  Let \eqn{X} denote a \link[stats:Poisson]{Poisson random variable} with parameter
  \code{lambda=}\eqn{\lambda}.  Let \eqn{x_{p|\lambda}} denote the \eqn{p}'th
  quantile of the distribution.  That is,
  \deqn{Pr(X < x_{p|\lambda}) \le p \le Pr(X \le x_{p|\lambda}) \;\;\;\; (1)}
  Note that due to the discrete nature of the Poisson distribution, there will be
  several values of \eqn{p} associated with one value of \eqn{X}.  For example,
  for \eqn{\lambda=2}, the value \eqn{1} is the \eqn{p}'th quantile for any value of
  \eqn{p} between 0.14 and 0.406.

  Let \eqn{\underline{x}} denote a vector of \eqn{n} observations from a Poisson
  distribution with parameter \code{lambda=}\eqn{\lambda}.  The \eqn{p}'th quantile
  is estimated as the \eqn{p}'th quantile from a Poisson distribution assuming the
  true value of \eqn{\lambda} is equal to the estimated value of \eqn{\lambda}.
  That is:
  \deqn{\hat{x}_{p|\lambda} = x_{p|\lambda=\hat{\lambda}} \;\;\;\; (2)}
  where
  \deqn{\hat{\lambda} = \bar{x} = \frac{1}{n} \sum_{i=1}^n x_i \;\;\;\; (3)}
  Because the estimator in equation (3) is the maximum likelihood estimator of
  \eqn{\lambda} (see the help file for \code{\link{epois}}), the estimated
  quantile is the maximum likelihood estimator.

  Quantiles are estimated by 1) estimating the mean parameter by
  calling \code{\link{epois}}, and then 2) calling the function
  \code{\link[stats:Poisson]{qpois}} and using the estimated value for
  the mean parameter.
  \cr

  \bold{Confidence Intervals} \cr
  It can be shown (e.g., Conover, 1980, pp.119-121) that an upper confidence
  interval for the \eqn{p}'th quantile with confidence level \eqn{100(1-\alpha)\%}
  is equivalent to an upper \eqn{\beta}-content tolerance interval with coverage
  \eqn{100p\%} and confidence level \eqn{100(1-\alpha)\%}.  Also, a lower
  confidence interval for the \eqn{p}'th quantile with confidence level
  \eqn{100(1-\alpha)\%} is equivalent to a lower \eqn{\beta}-content tolerance
  interval with coverage \eqn{100(1-p)\%} and confidence level \eqn{100(1-\alpha)\%}.

  Thus, based on the theory of tolerance intervals for a Poisson distribution
  (see \code{\link{tolIntPois}}), if \code{ci.type="upper"}, a one-sided upper
  \eqn{100(1-\alpha)\%} confidence interval for the \eqn{p}'th quantile is constructed
  as:
  \deqn{[0, x_{p|\lambda=UCL}] \;\;\;\; (4)}
  where \eqn{UCL} denotes the upper \eqn{100(1-\alpha)\%} confidence limit for
  \eqn{\lambda} (see the help file for \code{\link{epois}} for information on how
  \eqn{UCL} is computed).

  Similarly, if \code{ci.type="lower"}, a one-sided lower \eqn{100(1-\alpha)\%}
  confidence interval for the \eqn{p}'th quantile is constructed as:
  \deqn{[x_{p|\lambda=LCL}, \infty] \;\;\;\; (5)}
  where \eqn{LCL} denotes the lower \eqn{100(1-\alpha)\%} confidence limit for
  \eqn{\lambda} (see the help file for \code{\link{epois}} for information on how
  \eqn{LCL} is computed).

  Finally, if \code{ci.type="two-sided"}, a two-sided \eqn{100(1-\alpha)\%}
  confidence interval for the \eqn{p}'th quantile is constructed as:
  \deqn{[x_{p|\lambda=LCL}, x_{p|\lambda=UCL}] \;\;\;\; (6)}
  where \eqn{LCL} and \eqn{UCL} denote the two-sided lower and upper
  \eqn{100(1-\alpha)\%} confidence limits for \eqn{\lambda} (see the help file for
  \code{\link{epois}} for information on how \eqn{LCL} and \eqn{UCL} are computed).
}
\value{
  If \code{x} is a numeric vector, \code{eqpois} returns a
  list of class \code{"estimate"} containing the estimated quantile(s) and other
  information. See \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function, \code{eqpois}
  returns a list whose class is the same as \code{x}.  The list
  contains the same components as \code{x}, as well as components called
  \code{quantiles} and \code{quantile.method}.
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002).  \emph{Statistics for Environmental
  Engineers}.  Second Edition.  Lewis Publishers, Boca Raton, FL.

  Berthouex, P.M., and I. Hau. (1991).  Difficulties Related to Using Extreme
  Percentiles for Water Quality Regulations.  \emph{Research Journal of the Water
  Pollution Control Federation} \bold{63}(6), 873--879.

  Conover, W.J. (1980).  \emph{Practical Nonparametric Statistics}.  Second Edition.
  John Wiley and Sons, New York, Chapter 3.

  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Gibbons, R.D. (1987b).  Statistical Models for the Analysis of Volatile Organic
  Compounds in Waste Disposal Sites.  \emph{Ground Water} \bold{25}, 572-580.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009).
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.
  John Wiley & Sons, Hoboken.

  Johnson, N. L., S. Kotz, and A. Kemp. (1992).  \emph{Univariate Discrete
  Distributions}.  Second Edition.  John Wiley and Sons, New York, Chapter 4.

  Pearson, E.S., and H.O. Hartley, eds. (1970).  \emph{Biometrika Tables for
  Statisticians, Volume 1}.  Cambridge Universtiy Press, New York, p.81.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}.
  Fifth Edition. Prentice-Hall, Upper Saddle River, NJ.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Percentiles are sometimes used in environmental standards and regulations.  For
  example, Berthouex and Brown (2002, p.71) state:

  \emph{The U.S. EPA has specifications for air quality monitoring that are,
  in effect, percentile limitations. ... The U.S. EPA has provided guidance for
  setting aquatic standards on toxic chemicals that require estimating 99th
  percentiles and using this statistic to make important decisions about monitoring
  and compliance.  They have also used the 99th percentile to establish maximum
  daily limits for industrial effluents (e.g., pulp and paper).}

  Given the importance of these quantities, it is essential to characterize the
  amount of uncertainty associated with the estimates of these quantities.  This
  is done with confidence intervals.

  The \link[stats:Poisson]{Poisson distribution} is named after Poisson, who
  derived this distribution as the limiting distribution of the
  \link[stats:Binomial]{binomial distribution} with parameters \code{size=}\eqn{N}
  and \code{prob=}\eqn{p}, where \eqn{N} tends to infinity, \eqn{p} tends to 0, and
  \eqn{Np} stays constant.

  In this context, the Poisson distribution was used by Bortkiewicz (1898) to model
  the number of deaths (per annum) from kicks by horses in Prussian Army Corps.  In
  this case, \eqn{p}, the probability of death from this cause, was small, but the
  number of soldiers exposed to this risk, \eqn{N}, was large.

  The Poisson distribution has been applied in a variety of fields, including quality
  control (modeling number of defects produced in a process), ecology (number of
  organisms per unit area), and queueing theory.  Gibbons (1987b) used the Poisson
  distribution to model the number of detected compounds per scan of the 32 volatile
  organic priority pollutants (VOC), and also to model the distribution of chemical
  concentration (in ppb).
}
\seealso{
  \code{\link{epois}}, \link[stats]{Poisson}, \code{\link{estimate.object}}.
}
\examples{
  # Generate 20 observations from a Poisson distribution with parameter
  # lambda=2.  The true 90'th percentile of this distribution is 4 (actually,
  # 4 is the p'th quantile for any value of p between 0.86 and 0.947).
  # Here we will use eqpois to estimate the 90'th percentile and construct a
  # two-sided 95% confidence interval for this percentile.
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250)
  dat <- rpois(20, lambda = 2)
  eqpois(dat, p = 0.9, ci = TRUE)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Poisson
  #
  #Estimated Parameter(s):          lambda = 1.8
  #
  #Estimation Method:               mle/mme/mvue
  #
  #Estimated Quantile(s):           90'th %ile = 4
  #
  #Quantile Estimation Method:      mle
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Confidence Interval for:         90'th %ile
  #
  #Confidence Interval Method:      Exact
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 3
  #                                 UCL = 5

  # Clean up
  #---------
  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }

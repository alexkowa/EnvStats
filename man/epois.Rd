\name{epois}
\alias{epois}
\title{
  Estimate Parameter of a Poisson Distribution
}
\description{
  Estimate the mean of a \link[stats:Poisson]{Poisson distribution}, and 
  optionally construct a confidence interval for the mean.
}
\usage{
  epois(x, method = "mle/mme/mvue", ci = FALSE, ci.type = "two-sided", 
    ci.method = "exact", conf.level = 0.95)
}
\arguments{
  \item{x}{
  numeric vector of observations.
}
  \item{method}{
  character string specifying the method of estimation.  Currently the only possible 
  value is \code{"mle/mme/mvue"} (maximum likelihood/method of moments/minimum variance unbiased; 
  the default).  See the DETAILS section for more information. 
}
  \item{ci}{
  logical scalar indicating whether to compute a confidence interval for the 
  location or scale parameter.  The default value is \code{FALSE}.
}
  \item{ci.type}{
  character string indicating what kind of confidence interval to compute.  The 
  possible values are \code{"two-sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.  This argument is ignored if \code{ci=FALSE}.
}
  \item{ci.method}{
  character string indicating what method to use to construct the confidence interval 
  for the location or scale parameter.  Possible values are \code{"exact"} 
  (the default), \code{"pearson.hartley.approx"} (Pearson-Hartley approximation), and 
  \code{"normal.approx"} (normal approximation).  See the DETAILS section for more 
  information.  This argument is ignored if \code{ci=FALSE}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level of the confidence interval.  
  The default value is \code{conf.level=0.95}. This argument is ignored if 
  \code{ci=FALSE}.
}
}
\details{
  If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or 
  infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to 
  performing the estimation.

  Let \eqn{\underline{x} = (x_1, x_2, \ldots, x_n)} be a vector of 
  \eqn{n} observations from a \link[stats:Poisson]{Poisson distribution} with 
  parameter \code{lambda=}\eqn{\lambda}.  It can be shown (e.g., Forbes et al., 2009) 
  that if \eqn{y} is defined as:
  \deqn{y = \sum_{i=1}^n x_i \;\;\;\; (1)}
  then \eqn{y} is an observation from a Poisson distribution with parameter 
  \code{lambda=}\eqn{n \lambda}.

  \bold{Estimation} \cr
  The maximum likelihood, method of moments, and minimum variance unbiased estimator 
  (mle/mme/mvue) of \eqn{\lambda} is given by:
  \deqn{\hat{\lambda} = \bar{x} \;\;\;\; (2)}
  where
  \deqn{\bar{x} = \frac{1}{n} \sum_{i=1}^n x_i = \frac{y}{n} \;\;\;\; (3)}
  \cr

  \bold{Confidence Intervals} \cr
  There are three possible ways to construct a confidence interval for 
  \eqn{\lambda}:  based on the exact distribution of the estimator of 
  \eqn{\lambda} (\code{ci.type="exact"}), based on an approximation of 
  Pearson and Hartley (\code{ci.type="pearson.hartley.approx"}), or based on the 
  normal approximation \cr
  (\code{ci.type="normal.approx"}).

  \emph{Exact Confidence Interval} (\code{ci.method="exact"}) \cr
  If \code{ci.type="two-sided"}, an exact \eqn{(1-\alpha)100\%} confidence interval 
  for \eqn{\lambda} can be constructed as \eqn{[LCL, UCL]}, where the confidence 
  limits are computed such that:
  \deqn{Pr[Y \ge y \| \lambda = LCL] = \frac{\alpha}{2} \;\;\;\; (4)}
  \deqn{Pr[Y \le y \| \lambda = UCL] = \frac{\alpha}{2} \;\;\;\; (5)}
  where \eqn{y} is defined in equation (1) and \eqn{Y} denotes a Poisson random 
  variable with parameter \code{lambda=}\eqn{n \lambda}.

  If \code{ci.type="lower"}, \eqn{\alpha/2} is replaced with \eqn{\alpha} in 
  equation (4) and \eqn{UCL} is set to \eqn{\infty}.

  If \code{ci.type="upper"}, \eqn{\alpha/2} is replaced with \eqn{\alpha} in 
  equation (5) and \eqn{LCL} is set to 0.

  Note that an exact upper confidence bound can be computed even when all 
  observations are 0.
  \cr
 
  \emph{Pearson-Hartley Approximation} (\code{ci.method="pearson.hartley.approx"}) \cr
  For a two-sided \eqn{(1-\alpha)100\%} confidence interval for \eqn{\lambda}, the 
  Pearson and Hartley approximation (Zar, 2010, p.587; Pearson and Hartley, 1970, p.81) 
  is given by:
  \deqn{[\frac{\chi^2_{2n\bar{x}, \alpha/2}}{2n}, \frac{\chi^2_{2n\bar{x} + 2, 1 - \alpha/2}}{2n}] \;\;\;\; (6)}
  where \eqn{\chi^2_{\nu, p}} denotes the \eqn{p}'th quantile of the 
  \link[stats:Chisquare]{chi-square distribution} with \eqn{\nu} degrees of freedom.
  One-sided confidence intervals are computed in a similar fashion.
  \cr

  \emph{Normal Approximation} (\code{ci.method="normal.approx"})
  An approximate \eqn{(1-\alpha)100\%} confidence interval for \eqn{\lambda} can be 
  constructed assuming the distribution of the estimator of \eqn{\lambda} is 
  approximately normally distributed.  A two-sided confidence interval is constructed 
  as:
  \deqn{[\hat{\lambda} - z_{1-\alpha/2} \hat{\sigma}_{\hat{\lambda}}, \hat{\lambda} + z_{1-\alpha/2} \hat{\sigma}_{\hat{\lambda}}] \;\;\;\; (7)}
  where \eqn{z_p} is the \eqn{p}'th quantile of the standard normal distribution, and 
  the quantity
  \deqn{\hat{\sigma}_{\hat{\lambda}} = \sqrt{\hat{\lambda} / n} \;\;\;\; (8)}
  denotes the estimated asymptotic standard deviation of the estimator of 
  \eqn{\lambda}.

  One-sided confidence intervals are constructed in a similar manner.
}
\value{
  a list of class \code{"estimate"} containing the estimated parameters and other information. \cr 
  See \code{\link{estimate.object}} for details.
}
\references{
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
  Fifth Edition. Prentice-Hall, Upper Saddle River, NJ, pp. 585--586.  
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
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
  \link[stats:Poisson]{Poisson}.
}
\examples{
  # Generate 20 observations from a Poisson distribution with parameter 
  # lambda=2, then estimate the parameter and construct a 90\% confidence 
  # interval. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rpois(20, lambda = 2) 
  epois(dat, ci = TRUE, conf.level = 0.9) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Poisson
  #
  #Estimated Parameter(s):          lambda = 1.8
  #
  #Estimation Method:               mle/mme/mvue
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Confidence Interval for:         lambda
  #
  #Confidence Interval Method:      exact
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                90%
  #
  #Confidence Interval:             LCL = 1.336558
  #                                 UCL = 2.377037

  #----------

  # Compare the different ways of constructing confidence intervals for 
  # lambda using the same data as in the previous example:

  epois(dat, ci = TRUE, ci.method = "pearson", 
    conf.level = 0.9)$interval$limits 
  #     LCL      UCL 
  #1.336558 2.377037

  epois(dat, ci = TRUE, ci.method = "normal.approx",  
    conf.level = 0.9)$interval$limits 
  #     LCL      UCL 
  #1.306544 2.293456 

  #----------

  # Clean up
  #---------

  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }

\name{elogis}
\alias{elogis}
\title{
  Estimate Parameters of a Logistic Distribution
}
\description{
  Estimate the location and scale parameters of a 
  \link[stats:Logistic]{logistic distribution}, and optionally construct a 
  confidence interval for the location parameter.
}
\usage{
  elogis(x, method = "mle", ci = FALSE, ci.type = "two-sided", 
    ci.method = "normal.approx", conf.level = 0.95)
}
\arguments{
  \item{x}{
  numeric vector of observations.
}
  \item{method}{
  character string specifying the method of estimation.  Possible values are 
  \code{"mle"} (maximum likelihood; the default), \code{"mme"} (methods of moments), 
  and \code{"mmue"} (method of moments based on the unbiased estimator of variance).  
  See the DETAILS section for more information on these estimation methods. 
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
  for the location or scale parameter.  Currently, the only possible value is 
  \code{"normal.approx"} (the default).  See the DETAILS section for more information.  
  This argument is ignored if \code{ci=FALSE}.
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
  \eqn{n} observations from an \link[stats:Logistic]{logistic distribution} with 
  parameters \code{location=}\eqn{\eta} and \code{scale=}\eqn{\theta}.

  \bold{Estimation} \cr

  \emph{Maximum Likelihood Estimation} (\code{method="mle"}) \cr
  The maximum likelihood estimators (mle's) of \eqn{\eta} and \eqn{\theta} are 
  the solutions of the simultaneous equations (Forbes et al., 2011):
  \deqn{\sum_{i=1}^{n} \frac{1}{1 + e^{z_i}} = \frac{n}{2} \;\;\;\; (1)}
  \deqn{\sum_{i=1}^{n} z_i \, [\frac{1 - e^{z_i}}{1 + e^{z_i}} = n \;\;\;\; (2)}
  where 
  \deqn{z_i = \frac{x_i - \hat{eta}_{mle}}{\hat{\theta}_{mle}} \;\;\;\; (3)}

  \emph{Method of Moments Estimation} (\code{method="mme"}) \cr
  The method of moments estimators (mme's) of \eqn{\eta} and \eqn{\theta} are 
  given by:
  \deqn{\hat{\eta}_{mme} = \bar{x} \;\;\;\; (4)}
  \deqn{\hat{\theta}_{mme} = \frac{\sqrt{3}}{\pi} s_m \;\;\;\; (5)}
  where 
  \deqn{\bar{x} = \sum_{i=1}^n x_i \;\;\;\; (6)}
  \deqn{s_m^2 = \frac{1}{n} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\; (7)}
  that is, \eqn{s_m} denotes the square root of the method of moments estimator 
  of variance.

  \emph{Method of Moments Estimators Based on the Unbiased Estimator of Variance} (\code{method="mmue"}) \cr
  These estimators are exactly the same as the method of moments estimators given in 
  equations (4-7) above, except that the method of moments estimator of variance in 
  equation (7) is replaced with the unbiased estimator of variance:
  \deqn{s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\; (8)}
  \cr

  \bold{Confidence Intervals} \cr
  When \code{ci=TRUE}, an approximate \eqn{(1-\alpha)}100\% confidence intervals 
  for \eqn{\eta} can be constructed assuming the distribution of the estimator of 
  \eqn{\eta} is approximately normally distributed.  A two-sided confidence 
  interval is constructed as:
  \deqn{[\hat{\eta} - t(n-1, 1-\alpha/2) \hat{\sigma}_{\hat{\eta}}, \, \hat{\eta} + t(n-1, 1-\alpha/2) \hat{\sigma}_{\hat{\eta}}]}
  where \eqn{t(\nu, p)} is the \eqn{p}'th quantile of 
  \link[stats:TDist]{Student's t-distribution} with 
  \eqn{\nu} degrees of freedom, and the quantity 
  \deqn{\hat{\sigma}_{\hat{\eta}} = \frac{\pi \hat{\theta}}{\sqrt{3n}} \;\;\;\; (9)}
  denotes the estimated asymptotic standard deviation of the estimator of \eqn{\eta}.

  One-sided confidence intervals for \eqn{\eta} and \eqn{\theta} are computed in 
  a similar fashion.
}
\value{
  a list of class \code{"estimate"} containing the estimated parameters and other information. \cr 
  See \code{\link{estimate.object}} for details.
}
\references{
  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995). 
  \emph{Continuous Univariate Distributions, Volume 2}. 
  Second Edition. John Wiley and Sons, New York.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The \link[stats:Logistic]{logistic distribution} is defined on the real line 
  and is unimodal and symmetric about its location parameter (the mean).  It has 
  longer tails than a normal (Gaussian) distribution.  It is used to model growth 
  curves and bioassay data.
}
\seealso{
  \link[stats]{Logistic}.
}
\examples{
  # Generate 20 observations from a logistic distribution with 
  # parameters location=0 and scale=1, then estimate the parameters 
  # and construct a 90% confidence interval for the location parameter. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rlogis(20) 
  elogis(dat, ci = TRUE, conf.level = 0.9) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Logistic
  #
  #Estimated Parameter(s):          location = -0.2181845
  #                                 scale    =  0.8152793
  #
  #Estimation Method:               mle
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Confidence Interval for:         location
  #
  #Confidence Interval Method:      Normal Approximation
  #                                 (t Distribution)
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                90%
  #
  #Confidence Interval:             LCL = -0.7899382
  #                                 UCL =  0.3535693
  
  #----------

  # Clean up
  #---------
  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }


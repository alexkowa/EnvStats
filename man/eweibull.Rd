\name{eweibull}
\alias{eweibull}
\title{
  Estimate Parameters of a Weibull Distribution
}
\description{
  Estimate the shape and scale parameters of a 
  \link[stats:Weibull]{Weibull distribution}.
}
\usage{
  eweibull(x, method = "mle")
}
\arguments{
  \item{x}{
  numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), and 
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{method}{
  character string specifying the method of estimation.  Possible values are 
  \code{"mle"} (maximum likelihood; the default), \code{"mme"} (methods of moments), 
  and \code{"mmue"} (method of moments based on the unbiased estimator of variance).  
  See the DETAILS section for more information on these estimation methods.
}
}
\details{
  If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or 
  infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to 
  performing the estimation.

  Let \eqn{\underline{x} = (x_1, x_2, \ldots, x_n)} be a vector of 
  \eqn{n} observations from an \link[stats:Weibull]{Weibull distribution} with 
  parameters \code{shape=}\eqn{\alpha} and \code{scale=}\eqn{\beta}.

  \bold{Estimation} \cr

  \emph{Maximum Likelihood Estimation} (\code{method="mle"}) \cr
  The maximum likelihood estimators (mle's) of \eqn{\alpha} and \eqn{\beta} are 
  the solutions of the simultaneous equations (Forbes et al., 2011):
  \deqn{\hat{\alpha}_{mle} = \frac{n}{\{(1/\hat{\beta}_{mle})^{\hat{\alpha}_{mle}} \sum_{i=1}^n [x_i^{\hat{\alpha}_{mle}} log(x_i)]\} - \sum_{i=1}^n log(x_i) }  \;\;\;\; (1)}
  \deqn{\hat{\beta}_{mle} = [\frac{1}{n} \sum_{i=1}^n x_i^{\hat{\alpha}_{mle}}]^{1/\hat{\alpha}_{mle}} \;\;\;\; (2)}
  \cr
 
  \emph{Method of Moments Estimation} (\code{method="mme"}) \cr
  The method of moments estimator (mme) of \eqn{\alpha} is computed by solving the 
  equation:
  \deqn{\frac{s}{\bar{x}} = \{\frac{\Gamma[(\hat{\alpha}_{mme} + 2)/\hat{\alpha}_{mme}]}{\{\Gamma[(\hat{\alpha}_{mme} + 1)/\hat{\alpha}_{mme}] \}^2} - 1 \}^{1/2} \;\;\;\; (3)}
  and the method of moments estimator (mme) of \eqn{\beta} is then computed as:
  \deqn{\hat{\beta}_{mme} = \frac{\bar{x}}{\Gamma[(\hat{\alpha}_{mme} + 1)/\hat{\alpha}_{mme}]} \;\;\;\; (4)}
  where
  \deqn{\bar{x} = \frac{1}{n} \sum_{i=1}^n x_i \;\;\;\; (5)}
  \deqn{s^2_m = \frac{1}{n} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\; (6)}
  and \eqn{\Gamma()} denotes the \link[base:Special]{gamma function}.
  \cr
 
  \emph{Method of Moments Estimation Based on the Unbiased Estimator of Variance} (\code{method="mmue"}) \cr
  The method of moments estimators based on the unbiased estimator of variance are 
  exactly the same as the method of moments estimators given in equations (3-6) above, 
  except that the method of moments estimator of variance in equation (6) is replaced 
  with the unbiased estimator of variance:
  \deqn{s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\; (7)}
}
\value{
  a list of class \code{"estimate"} containing the estimated parameters and other 
  information.  See \cr
  \code{\link{estimate.object}} for details.
}
\references{
  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994). 
  \emph{Continuous Univariate Distributions, Volume 1}. 
  Second Edition. John Wiley and Sons, New York.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The \link[stats:Weibull]{Weibull distribution} is named after the Swedish physicist 
  Waloddi Weibull, who used this distribution to model breaking strengths of 
  materials.  The Weibull distribution has been extensively applied in the fields 
  of reliability and quality control.

  The \link[stats:Exponential]{exponential distribution} is a special case of the 
  Weibull distribution: a Weibull random variable with parameters \code{shape=}\eqn{1} 
  and \code{scale=}\eqn{\beta} is equivalent to an exponential random variable with 
  parameter \code{rate=}\eqn{1/\beta}.

  The Weibull distribution is related to the 
  \link[=EVD]{Type I extreme value (Gumbel) distribution} as follows: 
  if \eqn{X} is a random variable from a Weibull distribution with parameters 
  \code{shape=}\eqn{\alpha} and \code{scale=}\eqn{\beta}, then 
  \deqn{Y = -log(X) \;\;\;\; (10)} 
  is a random variable from an extreme value distribution with parameters 
  \code{location=}\eqn{-log(\beta)} and \code{scale=}\eqn{1/\alpha}.
}
\seealso{
  \link[stats]{Weibull}, \link[stats]{Exponential}, \link{EVD}, 
  \code{\link{estimate.object}}.
}
\examples{
  # Generate 20 observations from a Weibull distribution with parameters 
  # shape=2 and scale=3, then estimate the parameters via maximum likelihood. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rweibull(20, shape = 2, scale = 3) 
  eweibull(dat) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Weibull
  #
  #Estimated Parameter(s):          shape = 2.673098
  #                                 scale = 3.047762
  #
  #Estimation Method:               mle
  #
  #Data:                            dat
  #
  #Sample Size:                     20

  #----------

  # Use the same data as in previous example, and compute the method of 
  # moments estimators based on the unbiased estimator of variance:

  eweibull(dat, method = "mmue") 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Weibull
  #
  #Estimated Parameter(s):          shape = 2.528377
  #                                 scale = 3.052507
  #
  #Estimation Method:               mmue
  #
  #Data:                            dat
  #
  #Sample Size:                     20

  #----------

  # Clean up
  #---------
  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }


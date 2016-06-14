\name{kurtosis}
\alias{kurtosis}
\alias{Kurtosis}
\alias{sample kurtosis}
\title{
  Coefficient of (Excess) Kurtosis
}
\description{
  Compute the sample coefficient of kurtosis or excess kurtosis.
}
\usage{
  kurtosis(x, na.rm = FALSE, method = "fisher", l.moment.method = "unbiased", 
    plot.pos.cons = c(a = 0.35, b = 0), excess = TRUE)
}
\arguments{
  \item{x}{
  numeric vector of observations.
}
  \item{na.rm}{
  logical scalar indicating whether to remove missing values from \code{x}.  
  If \code{na.rm=FALSE} (the default) and \code{x} contains missing values, 
  then a missing value (\code{NA}) is returned.  If \code{na.rm=TRUE}, 
  missing values are removed from \code{x} prior to computing the coefficient 
  of variation.
}
  \item{method}{
  character string specifying what method to use to compute the sample coefficient 
  of kurtosis.  The possible values are 
  \code{"fisher"} (ratio of unbiased moment estimators; the default), 
  \code{"moments"} (ratio of product moment estimators), or 
  \code{"l.moments"} (ratio of \eqn{L}-moment estimators).
}
  \item{l.moment.method}{
  character string specifying what method to use to compute the 
  \eqn{L}-moments when \code{method="l.moments"}.  The possible values are 
  \code{"ubiased"} (method based on the \eqn{U}-statistic; the default), or 
  \code{"plotting.position"} (method based on the plotting position formula). 
}
  \item{plot.pos.cons}{
  numeric vector of length 2 specifying the constants used in the formula for 
  the plotting positions when \code{method="l.moments"} and \cr
  \code{l.moment.method="plotting.position"}.  The default value is \cr
  \code{plot.pos.cons=c(a=0.35, b=0)}.  If this vector has a names attribute 
  with the value \code{c("a","b")} or \code{c("b","a")}, then the elements will 
  be matched by name in the formula for computing the plotting positions.  
  Otherwise, the first element is mapped to the name \code{"a"} and the second 
  element to the name \code{"b"}. 
}
  \item{excess}{
  logical scalar indicating whether to compute the kurtosis (\code{excess=FALSE}) or 
  excess kurtosis (\code{excess=TRUE}; the default).
}
}
\details{
  Let \eqn{\underline{x}} denote a random sample of \eqn{n} observations from 
  some distribution with mean \eqn{\mu} and standard deviation \eqn{\sigma}.

  \emph{Product Moment Coefficient of Kurtosis} \cr
  (\code{method="moment"} or \code{method="fisher"}) \cr
  The \bold{\emph{coefficient of kurtosis}} of a distribution is the fourth 
  standardized moment about the mean:
  \deqn{\eta_4 = \beta_2 = \frac{\mu_4}{\sigma^4} \;\;\;\;\;\; (1)}
  where
  \deqn{\eta_r = E[(\frac{X-\mu}{\sigma})^r] = \frac{1}{\sigma^r} E[(X-\mu)^r] = \frac{\mu_r}{\sigma^r} \;\;\;\;\;\; (2)}
  and
  \deqn{\mu_r = E[(X-\mu)^r] \;\;\;\;\;\; (3)}
  denotes the \eqn{r}'th moment about the mean (central moment).

  The \bold{\emph{coefficient of excess kurtosis}} is defined as: 
  \deqn{\beta_2 - 3 \;\;\;\;\;\; (4)}
  For a normal distribution, the coefficient of kurtosis is 3 and the coefficient of 
  excess kurtosis is 0.  Distributions with kurtosis less than 3 (excess kurtosis 
  less than 0) are called \bold{\emph{platykurtic}}:  they have shorter tails than 
  a normal distribution.  Distributions with kurtosis greater than 3 
  (excess kurtosis greater than 0) are called \bold{\emph{leptokurtic}}:  they have 
  heavier tails than a normal distribution.

  When \code{method="moment"}, the coefficient of kurtosis is estimated using the 
  method of moments estimator for the fourth central moment and and the method of 
  moments estimator for the variance:
  \deqn{\hat{\eta}_4 = \frac{\hat{\mu}_4}{\sigma^4} = \frac{\frac{1}{n} \sum_{i=1}^n (x_i - \bar{x})^4}{[\frac{1}{n} \sum_{i=1}^n (x_i - \bar{x})^2]^2} \;\;\;\;\; (5)}
  where
  \deqn{\hat{\sigma}^2_m = s^2_m = \frac{1}{n} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\;\;\; (6)}

  This form of estimation should be used when resampling (bootstrap or jackknife).

  When \code{method="fisher"}, the coefficient of kurtosis is estimated using the 
  unbiased estimator for the fourth central moment (Serfling, 1980, p.73) and the 
  unbiased estimator for the variance.
  \deqn{\hat{\sigma}^2 = s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\;\;\; (7)}
  \cr

  \emph{L-Moment Coefficient of Kurtosis} (\code{method="l.moments"}) \cr
  Hosking (1990) defines the \eqn{L}-moment analog of the coefficient of kurtosis as:
  \deqn{\tau_4 = \frac{\lambda_4}{\lambda_2} \;\;\;\;\;\; (8)}
  that is, the fourth \eqn{L}-moment divided by the second \eqn{L}-moment.  He shows 
  that this quantity lies in the interval (-1, 1).
 
  When \code{l.moment.method="unbiased"}, the \eqn{L}-kurtosis is estimated by:
  \deqn{t_4 = \frac{l_4}{l_2} \;\;\;\;\;\; (9)}
  that is, the unbiased estimator of the fourth \eqn{L}-moment divided by the 
  unbiased estimator of the second \eqn{L}-moment.

  When \code{l.moment.method="plotting.position"}, the \eqn{L}-kurtosis is estimated by:
  \deqn{\tilde{\tau}_4 = \frac{\tilde{\lambda}_4}{\tilde{\lambda}_2} \;\;\;\;\;\; (10)}
  that is, the plotting-position estimator of the fourth \eqn{L}-moment divided by the 
  plotting-position estimator of the second \eqn{L}-moment.

  See the help file for \code{\link{lMoment}} for more information on 
  estimating \eqn{L}-moments.
}
\value{
  A numeric scalar -- the sample coefficient of kurtosis or excess kurtosis.
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). 
  \emph{Statistics for Environmental Engineers, Second Edition}. 
  Lewis Publishers, Boca Raton, FL.

  Ott, W.R. (1995). \emph{Environmental Statistics and Data Analysis}. 
  Lewis Publishers, Boca Raton, FL.

  Taylor, J.K. (1990). \emph{Statistical Techniques for Data Analysis}.  
  Lewis Publishers, Boca Raton, FL.

  Vogel, R.M., and N.M. Fennessey. (1993).  \eqn{L} Moment Diagrams Should Replace 
  Product Moment Diagrams.  \emph{Water Resources Research} \bold{29}(6), 1745--1752.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition. 
  Prentice-Hall, Upper Saddle River, NJ.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Traditionally, the coefficient of kurtosis has been estimated using product 
  moment estimators.  Sometimes an estimate of kurtosis is used in a 
  goodness-of-fit test for normality (D'Agostino and Stephens, 1986).  
  Hosking (1990) introduced the idea of \eqn{L}-moments and \eqn{L}-kurtosis.  

  Vogel and Fennessey (1993) argue that \eqn{L}-moment ratios should replace 
  product moment ratios because of their superior performance (they are nearly 
  unbiased and better for discriminating between distributions).  
  They compare product moment diagrams with \eqn{L}-moment diagrams.

  Hosking and Wallis (1995) recommend using unbiased estimators of \eqn{L}-moments 
  (vs. plotting-position estimators) for almost all applications.
}
\seealso{
  \code{\link{var}}, \code{\link{sd}}, \code{\link{cv}}, 
  \code{\link{skewness}}, \code{\link{summaryFull}}, 
  \link{Summary Statistics}.
}
\examples{
  # Generate 20 observations from a lognormal distribution with parameters 
  # mean=10 and cv=1, and estimate the coefficient of kurtosis and 
  # coefficient of excess kurtosis. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 

  dat <- rlnormAlt(20, mean = 10, cv = 1) 

  # Compute standard kurtosis first 
  #--------------------------------
  kurtosis(dat, excess = FALSE) 
  #[1] 2.964612
  
  kurtosis(dat, method = "moment", excess = FALSE) 
  #[1] 2.687146
  
  kurtosis(dat, method = "l.moment", excess = FALSE) 
  #[1] 0.1444779


  # Now compute excess kurtosis 
  #----------------------------
  kurtosis(dat) 
  #[1] -0.0353876
 
  kurtosis(dat, method = "moment") 
  #[1] -0.3128536
  
  kurtosis(dat, method = "l.moment") 
  #[1] -2.855522

  #----------
  # Clean up
  rm(dat)
}
\keyword{ univar }


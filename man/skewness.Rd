\name{skewness}
\alias{skewness}
\alias{Skewness}
\alias{skew}
\alias{Skew}
\title{
  Coefficient of Skewness
}
\description{
  Compute the sample coefficient of skewness.
}
\usage{
  skewness(x, na.rm = FALSE, method = "fisher", l.moment.method = "unbiased", 
    plot.pos.cons = c(a = 0.35, b = 0))
}
\arguments{
  \item{x}{
  numeric vector of observations.
}
  \item{na.rm}{
  logical scalar indicating whether to remove missing values from \code{x}.  If \cr
  \code{na.rm=FALSE} (the default) and \code{x} contains missing values, 
  then a missing value (\code{NA}) is returned.  If \code{na.rm=TRUE}, 
  missing values are removed from \code{x} prior to computing the coefficient 
  of variation.
}
  \item{method}{
  character string specifying what method to use to compute the sample coefficient 
  of skewness.  The possible values are 
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
}
\details{
  Let \eqn{\underline{x}} denote a random sample of \eqn{n} observations from 
  some distribution with mean \eqn{\mu} and standard deviation \eqn{\sigma}.

  \emph{Product Moment Coefficient of Skewness} (\code{method="moment"} or \code{method="fisher"}) \cr
  The \bold{\emph{coefficient of skewness}} of a distribution is the third 
  standardized moment about the mean:
  \deqn{\eta_3 = \sqrt{\beta_1} = \frac{\mu_3}{\sigma^3} \;\;\;\;\;\; (1)}
  where
  \deqn{\eta_r = E[(\frac{X-\mu}{\sigma})^r] = \frac{1}{\sigma^r} E[(X-\mu)^r] = \frac{\mu_r}{\sigma^r} \;\;\;\;\;\; (2)}
  and
  \deqn{\mu_r = E[(X-\mu)^r] \;\;\;\;\;\; (3)}
  denotes the \eqn{r}'th moment about the mean (central moment).
  That is, the coefficient of skewness is the third central moment divided by the 
  cube of the standard deviation.  The coefficient of skewness is 0 for a symmetric 
  distribution.  Distributions with positive skew have heavy right-hand tails, and 
  distributions with negative skew have heavy left-hand tails.

  When \code{method="moment"}, the coefficient of skewness is estimated using the 
  method of moments estimator for the third central moment and and the method of 
  moments estimator for the variance:
  \deqn{\hat{\eta}_3 = \frac{\hat{\mu}_3}{\sigma^3} = \frac{\frac{1}{n} \sum_{i=1}^n (x_i - \bar{x})^3}{[\frac{1}{n} \sum_{i=1}^n (x_i - \bar{x})^2]^{3/2}} \;\;\;\;\; (5)}
  where
  \deqn{\hat{\sigma}^2_m = s^2_m = \frac{1}{n} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\;\;\; (6)}

  This form of estimation should be used when resampling (bootstrap or jackknife).

  When \code{method="fisher"}, the coefficient of skewness is estimated using the 
  unbiased estimator for the third central moment 
  (Serfling, 1980, p.73; Chen, 1995, p.769) and the unbiased estimator for the 
  variance.
  \deqn{\hat{\eta}_3 = \frac{\frac{n}{(n-1)(n-2)} \sum_{i=1}^n (x_i - \bar{x})^3}{s^3} \;\;\;\;\;\; (7)}
  where
  \deqn{\hat{\sigma}^2 = s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\;\;\; (8)}
  (Note that Serfling, 1980, p.73 contains a typographical error in the numerator for 
  the unbiased estimator of the third central moment.) 
  \cr

  \emph{L-Moment Coefficient of skewness} (\code{method="l.moments"}) \cr
  Hosking (1990) defines the \eqn{L}-moment analog of the coefficient of skewness as:
  \deqn{\tau_3 = \frac{\lambda_3}{\lambda_2} \;\;\;\;\;\; (9)}
  that is, the third \eqn{L}-moment divided by the second \eqn{L}-moment.  He shows 
  that this quantity lies in the interval (-1, 1).
 
  When \code{l.moment.method="unbiased"}, the \eqn{L}-skewness is estimated by:
  \deqn{t_3 = \frac{l_3}{l_2} \;\;\;\;\;\; (10)}
  that is, the unbiased estimator of the third \eqn{L}-moment divided by the 
  unbiased estimator of the second \eqn{L}-moment.

  When \code{l.moment.method="plotting.position"}, the \eqn{L}-skewness is estimated by:
  \deqn{\tilde{\tau}_3 = \frac{\tilde{\lambda}_3}{\tilde{\lambda}_2} \;\;\;\;\;\; (11)}
  that is, the plotting-position estimator of the third \eqn{L}-moment divided by the 
  plotting-position estimator of the second \eqn{L}-moment.

  See the help file for \code{\link{lMoment}} for more information on 
  estimating \eqn{L}-moments.
}
\value{
  A numeric scalar -- the sample coefficient of skewness.
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). 
  \emph{Statistics for Environmental Engineers, Second Edition}. 
  Lewis Publishers, Boca Raton, FL.

  Chen, L. (1995).  Testing the Mean of Skewed Distributions.  
  \emph{Journal of the American Statistical Association} \bold{90}(430), 
  767--772.

  Helsel, D.R., and R.M. Hirsch. (1992). 
  \emph{Statistical Methods in Water Resources Research}. 
  Elsevier, New York, NY.

  Ott, W.R. (1995). \emph{Environmental Statistics and Data Analysis}. 
  Lewis Publishers, Boca Raton, FL.

  Serfling, R.J. (1980).  \emph{Approximation Theorems of Mathematical Statistics}.  
  John Wiley and Sons, New York, p.73.

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
  Traditionally, the coefficient of skewness has been estimated using product 
  moment estimators.  Sometimes an estimate of skewness is used in a 
  goodness-of-fit test for normality (e.g., set \cr
  \code{test="skew"} in the call to \code{\link{gofTest}}).

  Hosking (1990) introduced the idea of \eqn{L}-moments and \eqn{L}-skewness.  

  Vogel and Fennessey (1993) argue that \eqn{L}-moment ratios should replace 
  product moment ratios because of their superior performance (they are nearly 
  unbiased and better for discriminating between distributions).  
  They compare product moment diagrams with \eqn{L}-moment diagrams.

  Hosking and Wallis (1995) recommend using unbiased estimators of \eqn{L}-moments 
  (vs. plotting-position estimators) for almost all applications.
}
\seealso{
  \code{\link{var}}, \code{\link{sd}}, \code{\link{cv}}, 
  \code{\link{kurtosis}}, \code{\link{summaryFull}}, 
  \link{Summary Statistics}.
}
\examples{
  # Generate 20 observations from a lognormal distribution with parameters 
  # mean=10 and cv=1, and estimate the coefficient of skewness. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 

  dat <- rlnormAlt(20, mean = 10, cv = 1) 

  skewness(dat) 
  #[1] 0.9876632
 
  skewness(dat, method = "moment") 
  #[1] 0.9119889
 
  skewness(dat, meth = "l.moment") 
  #[1] 0.2656674

  #----------
  # Clean up
  rm(dat)
}
\keyword{ univar }


\name{eqlnorm3}
\alias{eqlnorm3}
\title{
  Estimate Quantiles of a Three-Parameter Lognormal Distribution
}
\description{
  Estimate quantiles of a \link[=Lognormal3]{three-parameter lognormal distribution}.
}
\usage{
  eqlnorm3(x, p = 0.5, method = "lmle", digits = 0)
}
\arguments{
  \item{x}{
  a numeric vector of observations, or an object resulting from a call to an 
  estimating function that assumes a three-parameter lognormal distribution 
  (e.g., \code{\link{elnorm3}}). If \code{x} is a numeric vector, 
  missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{p}{
  numeric vector of probabilities for which quantiles will be estimated.  
  All values of \code{p} must be between 0 and 1.  When \code{ci=TRUE}, \code{p} 
  must be a scalar. The default value is \code{p=0.5}.
}
  \item{method}{
  character string specifying the method of estimating the distribution parameters.  
  Possible values are 
  \code{"lmle"} (local maximum likelihood; the default), \code{"mme"} 
  (method of moments), \code{"mmue"} (method of moments using an unbaised 
  estimate of variance), \code{"mmme"} (modified method of moments due to Cohen 
  and Whitten (1980)), \code{"zero.skew"} (zero-skewness estimator due to 
  Griffiths (1980)), and \code{"royston.skew"} (estimator based on 
  Royston's (1992b) index of skewness).  See the DETAILS section of the help file 
  for \code{\link{elnorm3}} for more information on these estimation methods.
}
  \item{digits}{
  an integer indicating the number of decimal places to round to when printing out 
  the value of \code{100*p}. The default value is \code{digits=0}.
}
}
\details{
  If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or 
  infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to 
  performing the estimation.

  Quantiles are estimated by 1) estimating the distribution parameters by 
  calling \code{\link{elnorm3}}, and then 2) calling the function 
  \code{\link[=Lognormal3]{qlnorm3}} and using the estimated distribution  
  parameters.
}
\value{
  If \code{x} is a numeric vector, \code{eqlnorm3} returns a 
  list of class \code{"estimate"} containing the estimated quantile(s) and other 
  information. See \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function, \code{eqlnorm3} 
  returns a list whose class is the same as \code{x}.  The list 
  contains the same components as \code{x}, as well as components called 
  \code{quantiles} and \code{quantile.method}.
}
\references{
  Aitchison, J., and J.A.C. Brown (1957).  \emph{The Lognormal Distribution 
  (with special references to its uses in economics)}.  Cambridge University Press, 
  London, Chapter 5.

  Calitz, F. (1973).  Maximum Likelihood Estimation of the Parameters of the 
  Three-Parameter Lognormal Distribution--a Reconsideration.  \emph{Australian 
  Journal of Statistics} \bold{15}(3), 185--190.

  Cohen, A.C. (1951).  Estimating Parameters of Logarithmic-Normal Distributions by 
  Maximum Likelihood.  \emph{Journal of the American Statistical Association} 
  \bold{46}, 206--212.

  Cohen, A.C. (1988).  Three-Parameter Estimation.  In Crow, E.L., and K. Shimizu, eds. 
  \emph{Lognormal Distributions: Theory and Applications}.  Marcel Dekker, New York, 
  Chapter 4.

  Cohen, A.C., and B.J. Whitten. (1980).  Estimation in the Three-Parameter Lognormal 
  Distribution.  \emph{Journal of the American Statistical Association} \bold{75}, 
  399--404.

  Cohen, A.C., B.J. Whitten, and Y. Ding. (1985).  Modified Moment Estimation for the 
  Three-Parameter Lognormal Distribution.  \emph{Journal of Quality Technology} 
  \bold{17}, 92--99.

  Crow, E.L., and K. Shimizu. (1988).  \emph{Lognormal Distributions: Theory and 
  Applications}.  Marcel Dekker, New York, Chapter 2.

  Griffiths, D.A. (1980).  Interval Estimation for the Three-Parameter Lognormal 
  Distribution via the Likelihood Function.  \emph{Applied Statistics} \bold{29}, 
  58--68.

  Harter, H.L., and A.H. Moore. (1966).  Local-Maximum-Likelihood Estimation of the 
  Parameters of Three-Parameter Lognormal Populations from Complete and Censored 
  Samples.  \emph{Journal of the American Statistical Association} \bold{61}, 842--851.

  Heyde, C.C. (1963).  On a Property of the Lognormal Distribution.  \emph{Journal of 
  the Royal Statistical Society, Series B} \bold{25}, 392--393.

  Hill, .B.M. (1963).  The Three-Parameter Lognormal Distribution and Bayesian 
  Analysis of a Point-Source Epidemic.  \emph{Journal of the American Statistical 
  Association} \bold{58}, 72--84.

  Hoshi, K., J.R. Stedinger, and J. Burges. (1984).  Estimation of Log-Normal 
  Quantiles: Monte Carlo Results and First-Order Approximations.  \emph{Journal of 
  Hydrology} \bold{71}, 1--30.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994). 
  \emph{Continuous Univariate Distributions, Volume 1}. 
  Second Edition. John Wiley and Sons, New York.

  Royston, J.P. (1992b).  Estimation, Reference Ranges and Goodness of Fit for the 
  Three-Parameter Log-Normal Distribution.  \emph{Statistics in Medicine} \bold{11}, 
  897--912.

  Stedinger, J.R. (1980).  Fitting Lognormal Distributions to Hydrologic Data. 
  \emph{Water Resources Research} \bold{16}(3), 481--490.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The problem of estimating the parameters of a three-parameter lognormal distribution 
  has been extensively discussed by Aitchison and Brown (1957, Chapter 6), 
  Calitz (1973), Cohen (1951), Cohen (1988), Cohen and Whitten (1980), 
  Cohen et al. (1985), Griffiths (1980), Harter and Moore (1966), Hill (1963), and 
  Royston (1992b).  Stedinger (1980) and Hoshi et al. (1984) discuss fitting the 
  three-parameter lognormal distribution to hydrologic data.

  The global maximum likelihood estimates are inadmissible.  In the past, several 
  researchers have found that the local maximum likelihood estimates (lmle's) 
  occasionally fail because of convergence problems, but they were not using the 
  likelihood profile and reparameterization of Griffiths (1980).  Cohen (1988) 
  recommends the modified methods of moments estimators over lmle's because they are 
  easy to compute, they are unbiased with respect to \eqn{\mu} and \eqn{\sigma^2} (the 
  mean and standard deviation on the log-scale), their variances are minimal or near 
  minimal, and they do not suffer from regularity problems.

  Because the distribution of the lmle of the threshold parameter \eqn{\gamma} is far 
  from normal for moderate sample sizes (Griffiths, 1980), it is questionable whether 
  confidence intervals for \eqn{\gamma} or the median based on asymptotic variances 
  and covariances will perform well.  Cohen and Whitten (1980) and Cohen et al. (1985), 
  however, found that the asymptotic variances and covariances are reasonably close to 
  corresponding simulated variances and covariances for the modified method of moments 
  estimators (\code{method="mmme"}).  In a simulation study (5000 monte carlo trials), 
  Royston (1992b) found that the coverage of confidence intervals for \eqn{\gamma} 
  based on the likelihood profile (\code{ci.method="likelihood.profile"}) was very 
  close the nominal level (94.1\% for a nominal level of 95\%), although not 
  symmetric.  Royston (1992b) also found that the coverage of confidence intervals 
  for \eqn{\gamma} based on the skewness method (\code{ci.method="skewness"}) was also 
  very close (95.4\%) and symmetric.
}
\seealso{
  \code{\link{elnorm3}}, \link{Lognormal3}, \link[stats]{Lognormal}, 
  \link{LognormalAlt}, \link[stats]{Normal}.
}
\examples{
  # Generate 20 observations from a 3-parameter lognormal distribution 
  # with parameters meanlog=1.5, sdlog=1, and threshold=10, then use 
  # Cohen and Whitten's (1980) modified moments estimators to estimate 
  # the parameters, and estimate the 90th percentile. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rlnorm3(20, meanlog = 1.5, sdlog = 1, threshold = 10) 
  eqlnorm3(dat, method = "mmme", p = 0.9)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            3-Parameter Lognormal
  #
  #Estimated Parameter(s):          meanlog   = 1.5206664
  #                                 sdlog     = 0.5330974
  #                                 threshold = 9.6620403
  #
  #Estimation Method:               mmme
  #
  #Estimated Quantile(s):           90'th %ile = 18.72194
  #
  #Quantile Estimation Method:      Quantile(s) Based on
  #                                 mmme Estimators
  #
  #Data:                            dat
  #
  #Sample Size:                     20

  # Clean up
  #---------
  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }

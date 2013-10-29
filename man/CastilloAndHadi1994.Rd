\name{CastilloAndHadi1994}
\alias{CastilloAndHadi1994}
\alias{Castillo and Hadi 1994}
\concept{Castillo}
\concept{Abstract}
\title{
  Abstract:  Castillo and Hadi (1994)
}
\description{
  Detailed abstract of the manuscript: \cr\cr

  Castillo, E., and A. Hadi. (1994).  Parameter and Quantile Estimation for the 
  Generalized Extreme-Value Distribution.  \emph{Environmetrics} \bold{5}, 417--432.
}
\details{
  \bold{Abstract} \cr
  Castillo and Hadi (1994) introduce a new way to estimate the parameters and 
  quantiles of the \link[=GEVD]{generalized extreme value distribution} (GEVD) 
  with parameters \code{location=}\eqn{\eta}, \code{scale=}\eqn{\theta}, and 
  \code{shape=}\eqn{\kappa}.  The estimator is based on a two-stage procedure using 
  order statistics,  denoted here by \dQuote{TSOE}, which stands for 
  two-stage order-statistics estimator.  Castillo and Hadi (1994) compare the TSOE 
  to the maximum likelihood estimator (MLE; Jenkinson, 1969; Prescott and Walden, 1983) 
  and probability-weighted moments estimator (PWME; 
  \link[=HoskingEtAl1985]{Hosking et al., 1985)}.

  Castillo and Hadi (1994) note that for some samples the likelihood may not have 
  a local maximum, and also when \eqn{\kappa > 1} the likelihood can be made 
  infinite so the MLE does not exist.  They also note, as do 
  \link[=HoskingEtAl1985]{Hosking et al., 1985)}, that when \eqn{\kappa \le -1}, 
  the moments and probability-weighed moments of the GEVD do not exist, hence 
  neither does the PWME.  (Hosking et al., however, claim that in practice the 
  shape parameter usually lies between -1/2 and 1/2.)  On the other hand, the 
  TSOE exists for all values of \eqn{\kappa}.

  Based on computer simulations, Castillo and Hadi (1994) found that the 
  performance (bias and root mean squared error) of the TSOE is comparable to the 
  PWME for values of \eqn{\kappa} in the range \eqn{-1/2 \le \kappa \le 1/2}.  
  They also found that the TSOE is superior to the PWME for large values of 
  \eqn{\kappa}.  Their results, however, are based on using the PWME computed 
  using the approximation given in equation (14) of Hosking et al. (1985, p.253).  
  The true PWME is computed using equation (12) of Hosking et al. (1985, p.253).  
  Hosking et al. (1985) introduced the approximation as a matter of computational 
  convenience, and noted that it is valid in the range \eqn{-1/2 \le \kappa \le 1/2}.  
  If Castillo and Hadi (1994) had used the true PWME for values of \eqn{\kappa} 
  larger than 1/2, they probably would have gotten very different results for the 
  PWME.  (Note: the function \code{\link{egevd}} with \code{method="pwme"} uses 
  the exact equation (12) of Hosking et al. (1985), not the approximation (14)).

  Castillo and Hadi (1994) suggest using the bootstrap or jackknife to obtain 
  variance estimates and confidence intervals for the distribution parameters 
  based on the TSOE.
  \cr

  \bold{More Details}
  Let \eqn{\underline{x} = (x_1, x_2, \ldots, x_n)} be a vector of 
  \eqn{n} observations from a \link[=GEVD]{generalized extreme value distribution} with 
  parameters \code{location=}\eqn{\eta}, \code{scale=}\eqn{\theta}, and 
  \code{shape=}\eqn{\kappa} with cumulative distribution function \eqn{F}.  
  Also, let \eqn{x(1), x(2), \ldots, x(n)} denote the ordered values of 
  \eqn{\underline{x}}.


  \emph{First Stage} \cr
  Castillo and Hadi (1994) propose as initial estimates of the distribution 
  parameters the solutions to the following set of simultaneous equations based 
  on just three observations from the total sample of size \eqn{n}:
  \deqn{F[x(1); \eta, \theta, \kappa] = p_{1,n}}
  \deqn{F[x(j); \eta, \theta, \kappa] = p_{j,n}}
  \deqn{F[x(n); \eta, \theta, \kappa] = p_{n,n} \;\;\;\; (1)}
  where \eqn{2 \le j \le n-1}, and 
  \deqn{p_{i,n} = \hat{F}[x(i); \eta, \theta, \kappa]}
  denotes the \eqn{i}'th plotting position for a sample of size \eqn{n}; that is, a 
  nonparametric estimate of the value of \eqn{F} at \eqn{x(i)}.  Typically, 
  plotting positions have the form:
  \deqn{p_{i,n} = \frac{i-a}{n+b} \;\;\;\; (2)}
  where \eqn{b > -a > -1}.  In their simulation studies, Castillo and Hadi (1994) 
  used a=0.35, b=0.

  Since \eqn{j} is arbitrary in the above set of equations (1), denote the solutions 
  to these equations by: 
  \deqn{\hat{\eta}_j, \hat{\theta}_j, \hat{\kappa}_j}
  There are thus \eqn{n-2} sets of estimates.

  Castillo and Hadi (1994) show that the estimate of the shape parameter, \eqn{\kappa}, 
  is the solution to the equation:
  \deqn{\frac{x(j) - x(n)}{x(1) - x(n)} = \frac{1 - A_{jn}^\kappa}{1 - A_{1n}^\kappa} \;\;\;\; (3)}
  where
  \deqn{A_{ik} = C_i / C_k \;\;\;\; (4)}
  \deqn{C_i = -log(p_{i,n}) \;\;\;\; (5)}
  Castillo and Hadi (1994) show how to easily solve equation (3) using the method of 
  bisection.

  Once the estimate of the shape parameter is obtained, the other estimates are given 
  by:
  \deqn{\hat{\theta}_j = \frac{\hat{\kappa}_j [x(1) - x(n)]}{(C_n)^{\hat{\kappa}_j} - (C_1)^{\hat{\kappa}_j}} \;\;\;\; (6)}
  \deqn{\hat{\eta}_j = x(1) - \frac{\hat{\theta}_j [1 - (C_1)^{\hat{\kappa}_j}]}{\hat{\kappa}_j} \;\;\;\; (7)}
  \cr

  \emph{Second Stage} \cr
  Apply a robust function to the \eqn{n-2} sets of estimates obtained in the 
  first stage.  Castillo and Hadi (1994) suggest using either the median or the 
  least median of squares (using a column of 1's as the predictor variable; 
  see the help file for \link[MASS]{lmsreg} in the package \pkg{MASS}).  Using 
  the median, for example, the final distribution parameter estimates are 
  given by: 
  \deqn{\hat{\eta} = Median(\hat{\eta}_2, \hat{\eta}_3, \ldots, \hat{\eta}_{n-1})}
  \deqn{\hat{\theta} = Median(\hat{\theta}_2, \hat{\theta}_3, \ldots, \hat{\theta}_{n-1})}
  \deqn{\hat{\kappa} = Median(\hat{\kappa}_2, \hat{\kappa}_3, \ldots, \hat{\kappa}_{n-1})}
}
\references{
  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Hosking, J.R.M. (1985).  Algorithm AS 215: Maximum-Likelihood Estimation of the 
  Parameters of the Generalized Extreme-Value Distribution.  
  \emph{Applied Statistics} \bold{34}(3), 301--310.

  Jenkinson, A.F. (1969).  Statistics of Extremes. \emph{Technical Note 98}, 
  World Meteorological Office, Geneva.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995). 
  \emph{Continuous Univariate Distributions, Volume 2}. 
  Second Edition. John Wiley and Sons, New York.

  Prescott, P., and A.T. Walden. (1983).  Maximum Likelihood Estimation of the 
  Three-Parameter Generalized Extreme-Value Distribution from Censored Samples.  
  \emph{Journal of Statistical Computing and Simulation} \bold{16}, 241--250.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \link[=GEVD]{Generalized Extreme Value Distribution}, \code{\link{egevd}}, 
  \link[=HoskingEtAl1985]{Hosking et al., 1985)}.
}
\keyword{ distribution }

\name{ZeroModifiedLognormalAlt}
\alias{ZeroModifiedLognormalAlt}
\alias{DeltaDistAlt}
\alias{Delta Distribution (Alternative Parameterization)}
\alias{Zero-Modified Lognormal (Alternative Parameterization)}
\alias{Zero Modified Lognormal (Alternative Parameterization)}
\alias{dzmlnormAlt}
\alias{pzmlnormAlt}
\alias{qzmlnormAlt}
\alias{rzmlnormAlt}
\title{
  The Zero-Modified Lognormal (Delta) Distribution (Alternative Parameterization)
}
\description{
  Density, distribution function, quantile function, and random generation 
  for the zero-modified lognormal distribution with parameters \code{mean}, 
  \code{cv}, and \code{p.zero}.

  The zero-modified lognormal (delta) distribution is the mixture of a 
  lognormal distribution with a positive probability mass at 0.
}
\usage{
  dzmlnormAlt(x, mean = exp(1/2), cv = sqrt(exp(1) - 1), p.zero = 0.5)
  pzmlnormAlt(q, mean = exp(1/2), cv = sqrt(exp(1) - 1), p.zero = 0.5)
  qzmlnormAlt(p, mean = exp(1/2), cv = sqrt(exp(1) - 1), p.zero = 0.5)
  rzmlnormAlt(n, mean = exp(1/2), cv = sqrt(exp(1) - 1), p.zero = 0.5)
}
\arguments{
  \item{x}{
  vector of quantiles.
}
  \item{q}{
  vector of quantiles.
}
  \item{p}{
  vector of probabilities between 0 and 1.
}
  \item{n}{
  sample size.  If \code{length(n)} is larger than 1, then \code{length(n)} 
  random values are returned.
}
  \item{mean}{
  vector of means of the lognormal part of the distribution on the.  
  The default is \code{mean=exp(1/2)}.
}
  \item{cv}{
  vector of (positive) coefficients of variation of the lognormal 
  part of the distribution.  The default is \code{cv=sqrt(exp(1) - 1)}.
}
  \item{p.zero}{
  vector of probabilities between 0 and 1 indicating the probability the random 
  variable equals 0.  For \code{rzmlnormAlt} this must be a single, non-missing number.
}
}
\details{
  The zero-modified lognormal (delta) distribution is the mixture of a 
  lognormal distribution with a positive probability mass at 0.  This distribution 
  was introduced without a name by Aitchison (1955), and the name 
  \eqn{\Delta}-distribution was coined by Aitchison and Brown (1957, p.95).  
  It is a special case of a \dQuote{zero-modified} distribution 
  (see Johnson et al., 1992, p. 312).

  Let \eqn{f(x; \theta, \tau)} denote the density of a 
  \link[=LognormalAlt]{lognormal random variable} \eqn{X} with parameters 
  \code{mean=}\eqn{\theta} and \code{cv=}\eqn{\tau}.  The density function of a 
  zero-modified lognormal (delta) random variable \eqn{Y} with parameters 
  \code{mean=}\eqn{\theta}, \code{cv=}\eqn{\tau}, and \code{p.zero=}\eqn{p}, 
  denoted \eqn{h(y; \theta, \tau, p)}, is given by:
  \tabular{lll}{
    \eqn{h(y; \theta, \tau, p) =} \tab  \eqn{p}  \tab for \eqn{y = 0} \cr
                                  \tab  \eqn{(1 - p) f(y; \theta, \tau)} \tab for \eqn{y > 0}
  }
  Note that \eqn{\theta} is \emph{not} the mean of the zero-modified lognormal 
  distribution; it is the mean of the lognormal part of the distribution.  
  Similarly, \eqn{\tau} is \emph{not} the coefficient of variation of the 
  zero-modified lognormal distribution; it is the coefficient of variation of the 
  lognormal part of the distribution.

  Let \eqn{\gamma}, \eqn{\delta}, and \eqn{\omega} denote the mean, 
  standard deviation, and coefficient of variation of the overall zero-modified 
  lognormal distribution.  Let \eqn{\eta} denote the standard deviation of the 
  lognormal part of the distribution, so that \eqn{\eta = \theta \tau}.  
  Aitchison (1955) shows that:
  \deqn{E(Y) = \gamma = (1 - p) \theta}
  \deqn{Var(Y) = \delta^2 = (1 - p) \eta^2 + p (1-p) \theta^2}
  so that
  \deqn{\omega = \sqrt{(\tau^2 + p) / (1 - p)}}
  Note that when \code{p.zero=}\eqn{p}\code{=0}, the zero-modified lognormal 
  distribution simplifies to the lognormal distribution.
}
\value{
  \code{dzmlnormAlt} gives the density, \code{pzmlnormAlt} gives the distribution function, 
  \code{qzmlnormAlt} gives the quantile function, and \code{rzmlnormAlt} generates random 
  deviates. 
}
\references{
  Aitchison, J. (1955).  On the Distribution of a Positive Random Variable Having 
  a Discrete Probability Mass at the Origin.  \emph{Journal of the American 
  Statistical Association} \bold{50}, 901-908.

  Aitchison, J., and J.A.C. Brown (1957).  \emph{The Lognormal Distribution 
  (with special reference to its uses in economics)}.  Cambridge University Press, 
  London. pp.94-99.

  Crow, E.L., and K. Shimizu. (1988).  \emph{Lognormal Distributions: 
  Theory and Applications}.  Marcel Dekker, New York, pp.47-51.

  Gibbons, RD., D.K. Bhaumik, and S. Aryal. (2009).  \emph{Statistical Methods 
  for Groundwater Monitoring}.  Second Edition.  John Wiley and Sons, Hoboken, NJ.

  Gilliom, R.J., and D.R. Helsel. (1986).  Estimation of Distributional Parameters 
  for Censored Trace Level Water Quality Data: 1. Estimation Techniques.  
  \emph{Water Resources Research} \bold{22}, 135-146.

  Helsel, D.R. (2012).  \emph{Statistics for Censored Environmental Data Using 
  Minitab and R}.  Second Edition.  John Wiley and Sons, Hoboken, NJ, Chapter 1.

  Johnson, N. L., S. Kotz, and A.W. Kemp. (1992).  \emph{Univariate Discrete Distributions}. 
  Second Edition. John Wiley and Sons, New York, p.312.

  Owen, W., and T. DeRouen. (1980).  Estimation of the Mean for Lognormal Data 
  Containing Zeros and Left-Censored Values, with Applications to the Measurement 
  of Worker Exposure to Air Contaminants.  \emph{Biometrics} \bold{36}, 707-719.

  USEPA (1992c).  \emph{Statistical Analysis of Ground-Water Monitoring Data at 
  RCRA Facilities: Addendum to Interim Final Guidance}.  Office of Solid Waste, 
  Permits and State Programs Division, US Environmental Protection Agency, 
  Washington, D.C.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The zero-modified lognormal (delta) distribution is sometimes used to 
  model chemical concentrations for which some observations are reported as 
  \dQuote{Below Detection Limit} (the nondetects are assumed equal to 0).  
  See, for example, Gilliom and Helsel (1986), Owen and DeRouen (1980), and 
  Gibbons et al. (2009, Chapter 12).  USEPA (2009, Chapter 15) recommends this 
  strategy only in specific situations, and Helsel (2012, Chapter 1) strongly 
  discourages this approach to dealing with non-detects.

  A variation of the zero-modified lognormal (delta) distribution is the 
  \link[=ZeroModifiedNormal]{zero-modified normal distribution}, in which a 
  normal distribution is mixed with a positive probability mass at 0. 

  One way to try to assess whether a zero-modified lognormal (delta), 
  zero-modified normal, censored normal, or censored lognormal is the best 
  model for the data is to construct both censored and detects-only probability 
  plots (see \code{\link{qqPlotCensored}}).
}
\seealso{
  \link[=ZeroModifiedLognormal]{Zero-Modified Lognormal}, \link{LognormalAlt}, 
  \code{\link{ezmlnormAlt}}, \link{Probability Distributions and Random Numbers}.
}
\examples{
  # Density of the zero-modified lognormal (delta) distribution with 
  # parameters mean=10, cv=1, and p.zero=0.5, evaluated at 
  # 9, 10, and 11:

  dzmlnormAlt(9:11, mean = 10, cv = 1, p.zero = 0.5) 
  #[1] 0.02552685 0.02197043 0.01891924

  #----------

  # The cdf of the zero-modified lognormal (delta) distribution with 
  # parameters mean=10, cv=2, and p.zero=0.1, evaluated at 8:

  pzmlnormAlt(8, 10, 2, .1) 
  #[1] 0.709009

  #----------

  # The median of the zero-modified lognormal (delta) distribution with 
  # parameters mean=10, cv=2, and p.zero=0.1:

  qzmlnormAlt(0.5, 10, 2, 0.1) 
  #[1] 3.74576

  #----------

  # Random sample of 3 observations from the zero-modified lognormal 
  # (delta) distribution with parameters mean=10, cv=2, and p.zero=0.4. 
  # (Note: The call to set.seed simply allows you to reproduce this example.)

  set.seed(20) 
  rzmlnormAlt(3, 10, 2, 0.4)
  #[1] 0.000000 0.000000 4.907131
}
\keyword{ distribution }
\keyword{ datagen }

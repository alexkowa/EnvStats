\name{ZeroModifiedLognormal}
\alias{ZeroModifiedLognormal}
\alias{Zero-Modified Lognormal}
\alias{Zero-Modified Lognormal (Delta)}
\alias{Zero Modified Lognormal}
\alias{Zero Modified Lognormal (Delta)}
\alias{DeltaDist}
\alias{Delta Distribution}
\alias{dzmlnorm}
\alias{pzmlnorm}
\alias{qzmlnorm}
\alias{rzmlnorm}
\title{
  The Zero-Modified Lognormal (Delta) Distribution
}
\description{
  Density, distribution function, quantile function, and random generation 
  for the zero-modified lognormal distribution with parameters \code{meanlog}, 
  \code{sdlog}, and \code{p.zero}.

  The zero-modified lognormal (delta) distribution is the mixture of a 
  lognormal distribution with a positive probability mass at 0.
}
\usage{
  dzmlnorm(x, meanlog = 0, sdlog = 1, p.zero = 0.5)
  pzmlnorm(q, meanlog = 0, sdlog = 1, p.zero = 0.5)
  qzmlnorm(p, meanlog = 0, sdlog = 1, p.zero = 0.5)
  rzmlnorm(n, meanlog = 0, sdlog = 1, p.zero = 0.5)
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
  \item{meanlog}{
  vector of means of the normal (Gaussian) part of the distribution on the 
  log scale.  The default is \code{meanlog=0}.
}
  \item{sdlog}{
  vector of (positive) standard deviations of the normal (Gaussian) 
  part of the distribution on the log scale.  The default is \code{sdlog=1}.
}
  \item{p.zero}{
  vector of probabilities between 0 and 1 indicating the probability the random 
  variable equals 0.  For \code{rzmlnorm} this must be a single, non-missing number.
}
}
\details{
  The zero-modified lognormal (delta) distribution is the mixture of a 
  lognormal distribution with a positive probability mass at 0.  This distribution 
  was introduced without a name by Aitchison (1955), and the name 
  \eqn{\Delta}-distribution was coined by Aitchison and Brown (1957, p.95).  
  It is a special case of a \dQuote{zero-modified} distribution 
  (see Johnson et al., 1992, p. 312).

  Let \eqn{f(x; \mu, \sigma)} denote the density of a 
  \link[stats:Lognormal]{lognormal random variable} \eqn{X} with parameters 
  \code{meanlog=}\eqn{\mu} and \code{sdlog=}\eqn{\sigma}.  The density function of a 
  zero-modified lognormal (delta) random variable \eqn{Y} with parameters 
  \code{meanlog=}\eqn{\mu}, \code{sdlog=}\eqn{\sigma}, and \code{p.zero=}\eqn{p}, 
  denoted \eqn{h(y; \mu, \sigma, p)}, is given by:
  \tabular{lll}{
    \eqn{h(y; \mu, \sigma, p) =}  \tab  \eqn{p}  \tab for \eqn{y = 0} \cr
                                  \tab  \eqn{(1 - p) f(y; \mu, \sigma)} \tab for \eqn{y > 0}
  }
  Note that \eqn{\mu} is \emph{not} the mean of the zero-modified lognormal 
  distribution on the log scale; it is the mean of the lognormal part of the 
  distribution on the log scale.  Similarly, \eqn{\sigma} is 
  \emph{not} the standard deviation of the zero-modified lognormal distribution
  on the log scale; it is the standard deviation of the lognormal part of the 
  distribution on the log scale.

  Let \eqn{\gamma} and \eqn{\delta} denote the mean and standard deviation of the 
  overall zero-modified lognormal distribution on the log scale.  Aitchison (1955) 
  shows that:
  \deqn{E[log(Y)] = \gamma = (1 - p) \mu}
  \deqn{Var[log(Y)] = \delta^2 = (1 - p) \sigma^2 + p (1-p) \mu^2}
  Note that when \code{p.zero=}\eqn{p}\code{=0}, the zero-modified lognormal 
  distribution simplifies to the lognormal distribution.
}
\value{
  \code{dzmlnorm} gives the density, \code{pzmlnorm} gives the distribution function, 
  \code{qzmlnorm} gives the quantile function, and \code{rzmlnorm} generates random 
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
  \link[=ZeroModifiedLognormalAlt]{Zero-Modified Lognormal (Alternative Parameterization)}, 
  \link[stats:Lognormal]{Lognormal}, \link{LognormalAlt}, 
  \link[=ZeroModifiedNormal]{Zero-Modified Normal}, 
  \code{\link{ezmlnorm}}, \link{Probability Distributions and Random Numbers}.
}
\examples{
  # Density of the zero-modified lognormal (delta) distribution with 
  # parameters meanlog=0, sdlog=1, and p.zero=0.5, evaluated at 
  # 0, 0.5, 1, 1.5, and 2:

  dzmlnorm(seq(0, 2, by = 0.5)) 
  #[1] 0.50000000 0.31374804 0.19947114 0.12248683 
  #[5] 0.07843701

  #----------

  # The cdf of the zero-modified lognormal (delta) distribution with 
  # parameters meanlog=1, sdlog=2, and p.zero=0.1, evaluated at 4:

  pzmlnorm(4, 1, 2, .1) 
  #[1] 0.6189203

  #----------

  # The median of the zero-modified lognormal (delta) distribution with 
  # parameters meanlog=2, sdlog=3, and p.zero=0.1:

  qzmlnorm(0.5, 2, 3, 0.1) 
  #[1] 4.859177

  #----------

  # Random sample of 3 observations from the zero-modified lognormal 
  # (delta) distribution with parameters meanlog=1, sdlog=2, and p.zero=0.4. 
  # (Note: The call to set.seed simply allows you to reproduce this example.)

  set.seed(20) 
  rzmlnorm(3, 1, 2, 0.4)
  #[1] 0.000000 0.000000 3.146641
}
\keyword{ distribution }
\keyword{ datagen }

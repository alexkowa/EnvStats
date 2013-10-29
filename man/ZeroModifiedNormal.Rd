\name{ZeroModifiedNormal}
\alias{ZeroModifiedNormal}
\alias{Zero-Modified Normal}
\alias{Zero Modified Normal}
\alias{dzmnorm}
\alias{pzmnorm}
\alias{qzmnorm}
\alias{rzmnorm}
\title{
  The Zero-Modified Normal Distribution
}
\description{
  Density, distribution function, quantile function, and random generation 
  for the zero-modified normal distribution with parameters \code{mean}, 
  \code{sd}, and \code{p.zero}.

  The zero-modified normal distribution is the mixture of a normal distribution 
  with a positive probability mass at 0. 
}
\usage{
  dzmnorm(x, mean = 0, sd = 1, p.zero = 0.5)
  pzmnorm(q, mean = 0, sd = 1, p.zero = 0.5)
  qzmnorm(p, mean = 0, sd = 1, p.zero = 0.5)
  rzmnorm(n, mean = 0, sd = 1, p.zero = 0.5)
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
  vector of means of the normal (Gaussian) part of the distribution.  
  The default is \code{mean=0}.
}
  \item{sd}{
  vector of (positive) standard deviations of the normal (Gaussian) 
  part of the distribution.  The default is \code{sd=1}.
}
  \item{p.zero}{
  vector of probabilities between 0 and 1 indicating the probability the random 
  variable equals 0.  For \code{rzmnorm} this must be a single, non-missing number.
}
}
\details{
  The zero-modified normal distribution is the mixture of a normal distribution 
  with a positive probability mass at 0. 

  Let \eqn{f(x; \mu, \sigma)} denote the density of a 
  \link[stats:Normal]{normal (Gaussian) random variable} \eqn{X} with parameters 
  \code{mean=}\eqn{\mu} and \code{sd=}\eqn{\sigma}.  The density function of a 
  zero-modified normal random variable \eqn{Y} with parameters \code{mean=}\eqn{\mu}, 
  \code{sd=}\eqn{\sigma}, and \code{p.zero=}\eqn{p}, denoted \eqn{h(y; \mu, \sigma, p)}, 
  is given by:
  \tabular{lll}{
    \eqn{h(y; \mu, \sigma, p) =}  \tab  \eqn{p}  \tab for \eqn{y = 0} \cr
                                  \tab  \eqn{(1 - p) f(y; \mu, \sigma)} \tab for \eqn{y \ne 0}
  }
  Note that \eqn{\mu} is \emph{not} the mean of the zero-modified normal distribution; 
  it is the mean of the normal part of the distribution.  Similarly, \eqn{\sigma} is 
  \emph{not} the standard deviation of the zero-modified normal distribution; it is 
  the standard deviation of the normal part of the distribution.

  Let \eqn{\gamma} and \eqn{\delta} denote the mean and standard deviation of the 
  overall zero-modified normal distribution.  Aitchison (1955) shows that:
  \deqn{E(Y) = \gamma = (1 - p) \mu}
  \deqn{Var(Y) = \delta^2 = (1 - p) \sigma^2 + p (1-p) \mu^2}
  Note that when \code{p.zero=}\eqn{p}\code{=0}, the zero-modified normal 
  distribution simplifies to the normal distribution.
}
\value{
  \code{dzmnorm} gives the density, \code{pzmnorm} gives the distribution function, 
  \code{qzmnorm} gives the quantile function, and \code{rzmnorm} generates random 
  deviates. 
}
\references{
  Aitchison, J. (1955).  On the Distribution of a Positive Random Variable Having 
  a Discrete Probability Mass at the Origin.  \emph{Journal of the American 
  Statistical Association} \bold{50}, 901-908.

  Gilliom, R.J., and D.R. Helsel. (1986).  Estimation of Distributional Parameters 
  for Censored Trace Level Water Quality Data: 1. Estimation Techniques.  
  \emph{Water Resources Research} \bold{22}, 135-146.

  Gibbons, RD., D.K. Bhaumik, and S. Aryal. (2009).  \emph{Statistical Methods 
  for Groundwater Monitoring}.  Second Edition.  John Wiley and Sons, Hoboken, NJ.

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
  The zero-modified normal distribution is sometimes used to model chemical 
  concentrations for which some observations are reported as 
  \dQuote{Below Detection Limit}.  See, for example USEPA (1992c, pp.27-34) and 
  Gibbons et al. (2009, Chapter 12).  Note, however, that USEPA (1992c) has been 
  superseded by USEPA (2009) which recommends this strategy only in specific 
  situations (see Chapter 15 of the document).  This strategy is strongly 
  discouraged by Helsel (2012, Chapter 1).

  In cases where you want to model chemical concentrations for which some 
  observations are reported as \dQuote{Below Detection Limit} and you want to treat 
  the non-detects as equal to 0, it will usually be more appropriate to model the 
  data with a \link[=ZeroModifiedLognormal]{zero-modified lognormal (delta) 
  distribution} since chemical concentrations are bounded below at 
  0 (e.g., Gilliom and Helsel, 1986; Owen and DeRouen, 1980).

  One way to try to assess whether a zero-modified lognormal (delta), 
  zero-modified normal, censored normal, or censored lognormal is the best 
  model for the data is to construct both censored and detects-only probability 
  plots (see \code{\link{qqPlotCensored}}).
}
\seealso{
  \link[=ZeroModifiedLognormal]{Zero-Modified Lognormal}, \link[stats:Normal]{Normal}, 
  \code{\link{ezmnorm}}, \link{Probability Distributions and Random Numbers}.
}
\examples{
  # Density of the zero-modified normal distribution with parameters 
  # mean=2, sd=1, and p.zero=0.5, evaluated at 0, 0.5, 1, 1.5, and 2:

  dzmnorm(seq(0, 2, by = 0.5), mean = 2) 
  #[1] 0.5000000 0.0647588 0.1209854 0.1760327 0.1994711

  #----------

  # The cdf of the zero-modified normal distribution with parameters 
  # mean=3, sd=2, and p.zero=0.1, evaluated at 4:

  pzmnorm(4, 3, 2, .1) 
  #[1] 0.7223162

  #----------

  # The median of the zero-modified normal distribution with parameters 
  # mean=3, sd=1, and p.zero=0.1:

  qzmnorm(0.5, 3, 1, 0.1) 
  #[1] 2.86029

  #----------

  # Random sample of 3 observations from the zero-modified normal distribution 
  # with parameters mean=3, sd=1, and p.zero=0.4. 
  # (Note: The call to set.seed simply allows you to reproduce this example.)

  set.seed(20) 
  rzmnorm(3, 3, 1, 0.4) 
  #[1] 0.000000 0.000000 3.073168
}
\keyword{ distribution }
\keyword{ datagen }

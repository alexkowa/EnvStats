\name{EVD}
\alias{EVD}
\alias{Extreme Value Distribution}
\alias{Gumbel Distribution}
\alias{devd}
\alias{pevd}
\alias{qevd}
\alias{revd}
\title{
  The Extreme Value (Gumbel) Distribution
}
\description{
  Density, distribution function, quantile function, and random generation 
  for the (largest) extreme value distribution.
}
\usage{
  devd(x, location = 0, scale = 1)
  pevd(q, location = 0, scale = 1)
  qevd(p, location = 0, scale = 1)
  revd(n, location = 0, scale = 1)
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
  \item{location}{
  vector of location parameters.
}
  \item{scale}{
  vector of positive scale parameters.
}
}
\details{
  Let \eqn{X} be an extreme value random variable with parameters 
  \code{location=}\eqn{\eta} and \code{scale=}\eqn{\theta}.  
  The density function of \eqn{X} is given by:
  \deqn{f(x; \eta, \theta) = \frac{1}{\theta} e^{-(x-\eta)/\theta} exp[-e^{-(x-\eta)/\theta}]}
  where \eqn{-\infty < x, \eta < \infty} and \eqn{\theta > 0}.

  The cumulative distribution function of \eqn{X} is given by:
  \deqn{F(x; \eta, \theta) = exp[-e^{-(x-\eta)/\theta}]}

  The \eqn{p^{th}} quantile of \eqn{X} is given by:
  \deqn{x_{p} = \eta - \theta log[-log(p)]}

  The mode, mean, variance, skew, and kurtosis of \eqn{X} are given by:
  \deqn{Mode(X) = \eta}
  \deqn{E(X) = \eta + \epsilon \theta}
  \deqn{Var(X) = \theta^2 \pi^2 / 6}
  \deqn{Skew(X) = \sqrt{\beta_1} = 1.139547}
  \deqn{Kurtosis(X) = \beta_2 = 5.4}
  where \eqn{\epsilon} denotes \link[=EulersConstant]{Euler's constant}, 
  which is equivalent to \code{-\link{digamma}(1)}.
}
\value{
  density (\code{devd}), probability (\code{pevd}), quantile (\code{qevd}), or 
  random sample (\code{revd}) for the extreme value distribution with 
  location parameter(s) determined by \code{location} and scale 
  parameter(s) determined by \code{scale}.
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
  There are three families of extreme value distributions.  The one 
  described here is the Type I, also called the Gumbel extreme value 
  distribution or simply Gumbel distribution.  The name 
  \dQuote{extreme value} comes from the fact that this distribution is 
  the limiting distribution (as \eqn{n} approaches infinity) of the 
  greatest value among \eqn{n} independent random variables each 
  having the same continuous distribution.

  The Gumbel extreme value distribution is related to the 
  \link[stats:Exponential]{exponential distribution} as follows. 
  Let \eqn{Y} be an \link[stats:Exponential]{exponential} random variable 
  with parameter \code{rate=}\eqn{\lambda}.  Then \eqn{X = \eta - log(Y)} 
  has an extreme value distribution with parameters 
  \code{location=}\eqn{\eta} and \code{scale=}\eqn{1/\lambda}.

  The distribution described above and used by \code{devd}, \code{pevd}, 
  \code{qevd}, and \code{revd} is the \emph{largest} extreme value 
  distribution.  The smallest extreme value distribution is the limiting 
  distribution (as \eqn{n} approaches infinity) of the smallest value among 
  \eqn{n} independent random variables each having the same continuous distribution. 
  If \eqn{X} has a largest extreme value distribution with parameters \cr
  \code{location=}\eqn{\eta} and \code{scale=}\eqn{\theta}, then 
  \eqn{Y = -X} has a smallest extreme value distribution with parameters 
  \code{location=}\eqn{-\eta} and \code{scale=}\eqn{\theta}.  The smallest 
  extreme value distribution is related to the 
  \link[stats:Weibull]{Weibull distribution} as follows.  
  Let \eqn{Y} be a \link[stats:Weibull]{Weibull random variable} with parameters 
  \code{shape=}\eqn{\beta} and \code{scale=}\eqn{\alpha}.  Then \eqn{X = log(Y)} 
  has a smallest extreme value distribution with parameters \code{location=}\eqn{log(\alpha)} 
  and \code{scale=}\eqn{1/\beta}.

  The extreme value distribution has been used extensively to model the distribution 
  of streamflow, flooding, rainfall, temperature, wind speed, and other 
  meteorological variables, as well as material strength and life data.
}
\seealso{
  \code{\link{eevd}}, \code{\link{GEVD}}, 
  \link{Probability Distributions and Random Numbers}.
}
\examples{
  # Density of an extreme value distribution with location=0, scale=1, 
  # evaluated at 0.5:

  devd(.5) 
  #[1] 0.3307043

  #----------

  # The cdf of an extreme value distribution with location=1, scale=2, 
  # evaluated at 0.5:

  pevd(.5, 1, 2) 
  #[1] 0.2769203

  #----------

  # The 25'th percentile of an extreme value distribution with 
  # location=-2, scale=0.5:

  qevd(.25, -2, 0.5) 
  #[1] -2.163317

  #----------

  # Random sample of 4 observations from an extreme value distribution with 
  # location=5, scale=2. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(20) 
  revd(4, 5, 2) 
  #[1] 9.070406 7.669139 4.511481 5.903675
}
\keyword{ distribution }
\keyword{ datagen }

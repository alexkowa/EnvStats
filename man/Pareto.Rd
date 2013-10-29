\name{Pareto}
\alias{Pareto}
\alias{dpareto}
\alias{ppareto}
\alias{qpareto}
\alias{rpareto}
\title{
  The Pareto Distribution
}
\description{
  Density, distribution function, quantile function, and random generation 
  for the Pareto distribution with parameters \code{location} and \code{shape}.
}
\usage{
  dpareto(x, location, shape = 1)
  ppareto(q, location, shape = 1)
  qpareto(p, location, shape = 1)
  rpareto(n, location, shape = 1)
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
  vector of (positive) location parameters.
}
  \item{shape}{
  vector of (positive) shape parameters.  The default is \code{shape=1}.
}
}
\details{
  Let \eqn{X} be a Pareto random variable with parameters \code{location=}\eqn{\eta} 
  and \code{shape=}\eqn{\theta}.  The density function of \eqn{X} is given by:
  \deqn{f(x; \eta, \theta) = \frac{\theta \eta^\theta}{x^{\theta + 1}}, \; \eta > 0, \; \theta > 0, \; x \ge \eta}
  The cumulative distribution function of \eqn{X} is given by:
  \deqn{F(x; \eta, \theta) = 1 - (\frac{\eta}{x})^\theta}
  and the \eqn{p}'th quantile of \eqn{X} is given by:
  \deqn{x_p = \eta (1 - p)^{-1/\theta}, \; 0 \le p \le 1}
  The mode, mean, median, variance, and coefficient of variation of \eqn{X} are given by:
  \deqn{Mode(X) = \eta}
  \deqn{E(X) = \frac{\theta \eta}{\theta - 1}, \; \theta > 1}
  \deqn{Median(X) = x_{0.5} = 2^{1/\theta} \eta}
  \deqn{Var(X) = \frac{\theta \eta^2}{(\theta - 1)^2 (\theta - 1)}, \; \theta > 2}
  \deqn{CV(X) = [\theta (\theta - 2)]^{-1/2}, \; \theta > 2}
}
\value{
  \code{dpareto} gives the density, \code{ppareto} gives the distribution function, 
  \code{qpareto} gives the quantile function, and \code{rpareto} generates random 
  deviates. 
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
  The Pareto distribution is named after Vilfredo Pareto (1848-1923), a professor 
  of economics.  It is derived from Pareto's law, which states that the number of 
  persons \eqn{N} having income \eqn{\ge x} is given by:
  \deqn{N = A x^{-\theta}}
  where \eqn{\theta} denotes Pareto's constant and is the shape parameter for the 
  probability distribution.

  The Pareto distribution takes values on the positive real line.  All values must be 
  larger than the \dQuote{location} parameter \eqn{\eta}, which is really a threshold 
  parameter.  There are three kinds of Pareto distributions.  The one described here 
  is the Pareto distribution of the first kind.  Stable Pareto distributions have 
  \eqn{0 < \theta < 2}.  Note that the \eqn{r}'th moment only exists if 
  \eqn{r < \theta}.

  The Pareto distribution is related to the 
  \link[stats:Exponential]{exponential distribution} and 
  \link[stats:Logistic]{logistic distribution} as follows.  
  Let \eqn{X} denote a Pareto random variable with \code{location=}\eqn{\eta} and 
  \code{shape=}\eqn{\theta}.  Then \eqn{log(X/\eta)} has an exponential distribution 
  with parameter \code{rate=}\eqn{\theta}, and \eqn{-log\{ [(X/\eta)^\theta] - 1 \}} 
  has a logistic distribution with parameters \code{location=}\eqn{0} and 
  \code{scale=}\eqn{1}.

  The Pareto distribution has a very long right-hand tail.  It is often applied in 
  the study of socioeconomic data, including the distribution of income, firm size, 
  population, and stock price fluctuations.
}
\seealso{
  \code{\link{epareto}}, \code{\link{eqpareto}}, \link[stats]{Exponential}, 
  \link{Probability Distributions and Random Numbers}.
}
\examples{
  # Density of a Pareto distribution with parameters location=1 and shape=1, 
  # evaluated at 2, 3 and 4: 

  dpareto(2:4, 1, 1) 
  #[1] 0.2500000 0.1111111 0.0625000

  #----------

  # The cdf of a Pareto distribution with parameters location=2 and shape=1, 
  # evaluated at 3, 4, and 5: 

  ppareto(3:5, 2, 1) 
  #[1] 0.3333333 0.5000000 0.6000000

  #----------

  # The 25'th percentile of a Pareto distribution with parameters 
  # location=1 and shape=1: 

  qpareto(0.25, 1, 1) 
  #[1] 1.333333

  #----------

  # A random sample of 4 numbers from a Pareto distribution with parameters 
  # location=3 and shape=2. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(10) 
  rpareto(4, 3, 2)
  #[1] 4.274728 3.603148 3.962862 5.415322
}
\keyword{ distribution }
\keyword{ datagen }

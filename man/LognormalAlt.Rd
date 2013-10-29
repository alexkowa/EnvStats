\name{LognormalAlt}
\alias{LognormalAlt}
\alias{dlnormAlt}
\alias{plnormAlt}
\alias{qlnormAlt}
\alias{rlnormAlt}
\title{
  The Lognormal Distribution (Alternative Parameterization)
}
\description{
  Density, distribution function, quantile function, and random generation 
  for the lognormal distribution with parameters \code{mean} and \code{cv}.
}
\usage{
  dlnormAlt(x, mean = exp(1/2), cv = sqrt(exp(1) - 1), log = FALSE)
  plnormAlt(q, mean = exp(1/2), cv = sqrt(exp(1) - 1), 
      lower.tail = TRUE, log.p = FALSE)
  qlnormAlt(p, mean = exp(1/2), cv = sqrt(exp(1) - 1), 
      lower.tail = TRUE, log.p = FALSE)
  rlnormAlt(n, mean = exp(1/2), cv = sqrt(exp(1) - 1))
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
  vector of (positive) means of the distribution of the random variable.
}
  \item{cv}{
  vector of (positive) coefficients of variation of the random variable.
}
  \item{log, log.p}{
  logical; if \code{TRUE}, probabilities/densities \eqn{p} are returned as \eqn{log(p)}.
}
  \item{lower.tail}{
  logical; if \code{TRUE} (default), probabilities are \eqn{P[X \le x]}, 
  otherwise, \eqn{P[X > x]}.
}
}
\details{
  Let \eqn{X} be a random variable with a \link[stats:Lognormal]{lognormal distribution} 
  with parameters \code{meanlog=}\eqn{\mu} and \code{sdlog=}\eqn{\sigma}.  That is, 
  \eqn{\mu} and \eqn{\sigma} denote the mean and standard deviation of the random variable 
  on the log scale.  The relationship between these parameters and the 
  mean (\code{mean=}\eqn{\theta}) and coefficient of variation (\code{cv=}\eqn{\tau}) 
  of the distribution on the original scale is given by:
  \deqn{\mu = log(\frac{\theta}{\sqrt{\tau^2 + 1}}) \;\;\;\; (1)}
  \deqn{\sigma = [log(\tau^2 + 1)]^{1/2} \;\;\;\; (2)}
  \deqn{\theta = exp[\mu + (\sigma^2/2)] \;\;\;\; (3)}
  \deqn{\tau = [exp(\sigma^2) - 1]^{1/2} \;\;\;\; (4)}
  Thus, the functions \code{dlnormAlt}, \code{plnormAlt}, \code{qlnormAlt}, and 
  \code{rlnormAlt} call the \R functions \code{\link{dlnorm}}, 
  \code{\link{plnorm}}, \code{\link{qlnorm}}, and \code{\link{rlnorm}}, 
  respectively using the following values for the \code{meanlog} and \code{sdlog} 
  parameters: \cr 
  \code{sdlog <- sqrt(log(1 + cv^2))}, \cr
  \code{meanlog <- log(mean) - (sdlog^2)/2}
}
\value{
  \code{dlnormAlt} gives the density, \code{plnormAlt} gives the distribution function, 
  \code{qlnormAlt} gives the quantile function, and \code{rlnormAlt} generates random 
  deviates. 
}
\references{
  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994). 
  \emph{Continuous Univariate Distributions, Volume 1}. 
  Second Edition. John Wiley and Sons, New York.

  Limpert, E., W.A. Stahel, and M. Abbt. (2001).  Log-Normal Distributions Across the 
  Sciences:  Keys and Clues.  \emph{BioScience} \bold{51}, 341--352.

  Ott, W.R. (1995). \emph{Environmental Statistics and Data Analysis}. 
  Lewis Publishers, Boca Raton, FL.

  Singh, A., R. Maichle, and N. Armbya. (2010a).  \emph{ProUCL Version 4.1.00 
  User Guide (Draft)}.  EPA/600/R-07/041, May 2010.  Office of Research and 
  Development, U.S. Environmental Protection Agency, Washington, D.C.

  Singh, A., N. Armbya, and A. Singh. (2010b).  \emph{ProUCL Version 4.1.00 
  Technical Guide (Draft)}.  EPA/600/R-07/041, May 2010.  Office of Research and 
  Development, U.S. Environmental Protection Agency, Washington, D.C.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The two-parameter \link[stats:Lognormal]{lognormal distribution} is the distribution 
  of a random variable whose logarithm is normally distributed.  The two major 
  characteristics of the lognormal distribution are that it is bounded below at 0, 
  and it is skewed to the right.

  Because the empirical distribution of many variables is inherently positive and 
  skewed to the right (e.g., size of organisms, amount of rainfall, size of income, 
  etc.), the lognormal distribution has been widely applied in several fields, 
  including economics, business, industry, biology, ecology, atmospheric science, and 
  geology (Aitchison and Brown, 1957; Crow and Shimizu, 1988).

  Gibrat (1930) derived the lognormal distribution from theoretical assumptions, 
  calling it the "law of proportionate effect", but Kapteyn (1903) had described a 
  machine that was the mechanical equivalent.  The basic idea is that the 
  Central Limit Theorem states that the distribution of the sum of several 
  independent random variables tends to look like a normal distribution, no matter 
  what the underlying distribution(s) of the original random variables, hence the 
  product of several independent random variables tends to look like a lognormal 
  distribution.

  The lognormal distribution is often used to characterize chemical concentrations 
  in the environment.  Ott (1990) has shown mathematically how a series of 
  successive random dilutions gives rise to a distribution that can be approximated 
  by a lognormal distribution.

  A lognormal distribution starts to resemble a normal distribution as the 
  parameter \eqn{\sigma} (the standard deviation of the log of the distribution) 
  tends to 0.

  Some EPA guidance documents (e.g., Singh et al., 2002; Singh et al., 2010a,b) 
  discourage using the assumption of a lognormal distribution for some types of 
  environmental data and recommend instead assessing whether the data appear to 
  fit a gamma distribution.
}
\seealso{
  \link[stats:Lognormal]{Lognormal}, \code{\link{elnormAlt}},  
  \link{Probability Distributions and Random Numbers}.
}
\examples{
  # Density of the lognormal distribution with parameters 
  # mean=10 and cv=1, evaluated at 5: 

  dlnormAlt(5, mean = 10, cv = 1) 
  #[1] 0.08788173

  #----------

  # The cdf of the lognormal distribution with parameters mean=2 and cv=3, 
  # evaluated at 4: 

  plnormAlt(4, 2, 3) 
  #[1] 0.8879132

  #----------

  # The median of the lognormal distribution with parameters 
  # mean=10 and cv=1: 

  qlnormAlt(0.5, mean = 10, cv = 1) 
  #[1] 7.071068

  #----------

  # Random sample of 3 observations from a lognormal distribution with 
  # parameters mean=10 and cv=1. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(20) 
  rlnormAlt(3, mean = 10, cv = 1) 
  #[1] 18.615797  4.341402 31.265293
}
\keyword{ distribution }
\keyword{ datagen }

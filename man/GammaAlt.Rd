\name{GammaAlt}
\alias{GammaAlt}
\alias{Gamma Distribution}
\alias{dgammaAlt}
\alias{pgammaAlt}
\alias{qgammaAlt}
\alias{rgammaAlt}
\title{
  The Gamma Distribution (Alternative Parameterization)
}
\description{
  Density, distribution function, quantile function, and random generation 
  for the gamma distribution with parameters \code{mean} and \code{cv}.
}
\usage{
  dgammaAlt(x, mean, cv = 1, log = FALSE)
  pgammaAlt(q, mean, cv = 1, lower.tail = TRUE, log.p = FALSE)
  qgammaAlt(p, mean, cv = 1, lower.tail = TRUE, log.p = FALSE)
  rgammaAlt(n, mean, cv = 1)
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
  Let \eqn{X} be a random variable with a gamma distribution with parameters 
  \code{shape=}\eqn{\alpha} and \code{scale=}\eqn{\beta}.  The relationship 
  between these parameters and the mean (\code{mean=}\eqn{\mu}) and coefficient 
  of variation (\code{cv=}\eqn{\tau}) of this distribution is given by:
  \deqn{\alpha = \tau^{-2} \;\;\;\;\;\; (1)}
  \deqn{\beta = \mu/\alpha \;\;\;\;\;\; (2)}
  \deqn{\mu = \alpha\beta \;\;\;\;\;\; (3)}
  \deqn{\tau = \alpha^{-1/2} \;\;\;\;\;\; (4)}
  Thus, the functions \code{dgammaAlt}, \code{pgammaAlt}, \code{qgammaAlt}, and 
  \code{rgammaAlt} call the \R functions \code{\link{dgamma}}, 
  \code{\link{pgamma}}, \code{\link{qgamma}}, and \code{\link{rgamma}}, 
  respectively, using the values for the \code{shape} and \code{scale} parameters 
  given by:  \code{shape <- cv^-2}, \code{scale <- mean/shape}.
}
\value{
  \code{dgammaAlt} gives the density, \code{pgammaAlt} gives the distribution function, 
  \code{qgammaAlt} gives the quantile function, and \code{rgammaAlt} generates random 
  deviates. 

  Invalid arguments will result in return value \code{NaN}, with a warning. 
}
\references{
  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  
  \emph{Statistical Distributions, Fourth Edition}. 
  John Wiley and Sons, Hoboken, NJ.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994). 
  \emph{Continuous Univariate Distributions, Volume 1}. 
  Second Edition. John Wiley and Sons, New York. 

  Singh, A., A.K. Singh, and R.J. Iaci. (2002).  Estimation of the Exposure 
  Point Concentration Term Using a Gamma Distribution.  EPA/600/R-02/084.  
  October 2002.  Technology Support Center for Monitoring and Site Characterization, 
  Office of Research and Development, Office of Solid Waste and Emergency Response, 
  U.S. Environmental Protection Agency, Washington, D.C.

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
  The gamma distribution takes values on the positive real line.  Special cases of 
  the gamma are the \link[stats:Exponential]{exponential distribution} and the 
  \link[stats:Chisquare]{chi-square distribution}.  Applications of the gamma include 
  life testing, statistical ecology, queuing theory, inventory control and 
  precipitation processes.  A gamma distribution starts to resemble a normal 
  distribution as the shape parameter \eqn{\alpha} tends to infinity or 
  the cv parameter \eqn{\tau} tends to 0.

  Some EPA guidance documents (e.g., Singh et al., 2002; Singh et al., 2010a,b) 
  discourage using the assumption of a \link[stats:Lognormal]{lognormal distribution} 
  for some types of environmental data and recommend instead assessing whether 
  the data appear to fit a gamma distribution.  
}
\seealso{
  \link{GammaDist}, \code{\link{egammaAlt}},  
  \link{Probability Distributions and Random Numbers}.
}
\examples{
  # Density of a gamma distribution with parameters mean=10 and cv=2, 
  # evaluated at 7:

  dgammaAlt(7, mean = 10, cv = 2) 
  #[1] 0.02139335

  #----------

  # The cdf of a gamma distribution with parameters mean=10 and cv=2, 
  # evaluated at 12:

  pgammaAlt(12, mean = 10, cv = 2) 
  #[1] 0.7713307

  #----------

  # The 25'th percentile of a gamma distribution with parameters 
  # mean=10 and cv=2:

  qgammaAlt(0.25, mean = 10, cv = 2) 
  #[1] 0.1056871

  #----------

  # A random sample of 4 numbers from a gamma distribution with 
  # parameters mean=10 and cv=2. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(10) 
  rgammaAlt(4, mean = 10, cv = 2) 
  #[1] 3.772004230 1.889028078 0.002987823 8.179824976
}
\keyword{ distribution }
\keyword{ datagen }

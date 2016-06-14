\name{Lognormal3}
\alias{Lognormal3}
\alias{Three Parameter Lognormal}
\alias{dlnorm3}
\alias{plnorm3}
\alias{qlnorm3}
\alias{rlnorm3}
\title{
  The Three-Parameter Lognormal Distribution
}
\description{
  Density, distribution function, quantile function, and random generation 
  for the three-parameter lognormal distribution with parameters \code{meanlog}, 
  \code{sdlog}, and \code{threshold}.
}
\usage{
  dlnorm3(x, meanlog = 0, sdlog = 1, threshold = 0)
  plnorm3(q, meanlog = 0, sdlog = 1, threshold = 0)
  qlnorm3(p, meanlog = 0, sdlog = 1, threshold = 0)
  rlnorm3(n, meanlog = 0, sdlog = 1, threshold = 0)
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
  vector of means of the distribution of the random variable on the log scale.  
  The default is \code{meanlog=0}.
}
  \item{sdlog}{
  vector of (positive) standard deviations of the random variable on the log scale.  
  The default is \code{sdlog=1}.
}
  \item{threshold}{
  vector of thresholds of the random variable on the log scale.  The default 
  is \code{threshold=0}.
}
}
\details{
  The three-parameter lognormal distribution is simply the usual 
  \link[stats:Lognormal]{two-parameter lognormal distribution} with a 
  location shift.

  Let \eqn{X} be a random variable with a three-parameter lognormal distribution 
  with parameters \code{meanlog=}\eqn{\mu}, \code{sdlog=}\eqn{\sigma}, and 
  \code{threshold=}\eqn{\gamma}.  Then the random variable \eqn{Y = X - \gamma} 
  has a \link[stats:Lognormal]{lognormal distribution} with parameters 
  \code{meanlog=}\eqn{\mu} and \code{sdlog=}\eqn{\sigma}.  Thus, 
  \itemize{
    \item  \code{dlnorm3} calls \code{\link{dlnorm}} using the arguments 
           \code{x = x - threshold}, \code{meanlog = meanlog}, 
           \code{sdlog = sdlog}
    \item \code{plnorm3} calls \code{\link{plnorm}} using the arguments 
          \code{q = q - threshold}, \code{meanlog = meanlog}, \code{sdlog = sdlog}
    \item \code{qlnorm3} calls \code{\link{qlnorm}} using the arguments 
          \code{q = q}, \code{meanlog = meanlog}, \code{sdlog = sdlog} and then adds 
          the argument \code{threshold} to the result.
    \item \code{rlnorm3} calls \code{\link{rlnorm}} using the arguments 
          \code{n = n}, \code{meanlog = meanlog}, \code{sdlog = sdlog} and then adds
          the argument \code{threshold} to the result.
  }

  The threshold parameter \eqn{\gamma} affects only the location of the 
  three-parameter lognormal distribution; it has no effect on the variance 
  or the shape of the distribution.

  Denote the mean, variance, and coefficient of variation of \eqn{Y = X - \gamma} by:
  \deqn{E(Y) = \theta}
  \deqn{Var(Y) = \eta^2}
  \deqn{CV(Y) = \tau = \eta/\theta}
  Then the mean, variance, and coefficient of variation of \eqn{X} are given by:
  \deqn{E(X) = \theta + \eta}
  \deqn{Var(X) = \eta^2}
  \deqn{CV(X) = \frac{\eta}{\theta + \gamma} = \frac{\tau \theta}{\theta + \gamma}}
  The relationships between the parameters \eqn{\mu}, \eqn{\sigma}, 
  \eqn{\theta}, \eqn{\eta}, and \eqn{\tau} are as follows:
  \deqn{\theta = \beta \sqrt{\omega}}
  \deqn{\eta = \beta \sqrt{\omega (\omega - 1)}}
  \deqn{\tau = \sqrt{\omega - 1}}
  \deqn{\mu = log(\frac{\theta}{\sqrt{\tau^2 + 1}})}
  \deqn{\sigma = \sqrt{log(\tau^2 + 1)}}
  where
  \deqn{\beta = e^\mu, \omega = exp(\sigma^2)}

  Since quantiles of a distribution are preserved under monotonic transformations, 
  the median of \eqn{X} is:
  \deqn{Median(X) = \gamma + \beta}
}
\value{
  \code{dlnorm3} gives the density, \code{plnorm3} gives the distribution function, 
  \code{qlnorm3} gives the quantile function, and \code{rlnorm3} generates random 
  deviates. 
}
\references{
  Aitchison, J., and J.A.C. Brown (1957).  \emph{The Lognormal Distribution 
  (with special references to its uses in economics)}.  Cambridge University 
  Press, London, 176pp.

  Crow, E.L., and K. Shimizu. (1988).  \emph{Lognormal Distributions:  
  Theory and Applications}. Marcel Dekker, New York, 387pp.

  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994). 
  \emph{Continuous Univariate Distributions, Volume 1}. 
  Second Edition. John Wiley and Sons, New York.

  Ott, W.R. (1990).  A Physical Explanation of the Lognormality of Pollutant 
  Concentrations.  \emph{Journal of the Air and Waste Management Association} \bold{40}, 
  1378--1383.

  Ott, W.R. (1995).  \emph{Environmental Statistics and Data Analysis}.  
  Lewis Publishers, Boca Raton, FL, Chapter 9.

  Royston, J.P. (1992b).  Estimation, Reference Ranges and Goodness of Fit for the 
  Three-Parameter Log-Normal Distribution.  \emph{Statistics in Medicine} \bold{11}, 
  897--912.

  Wicksell, S.D. (1917).  On Logarithmic Correlation with an Application to the 
  Distribution of Ages at First Marriage.  \emph{Medd. Lunds. Astr. Obs.} \bold{84}, 
  1--21.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The \link[stats:Lognormal]{two-parameter lognormal distribution} is the 
  distribution of a random variable whose logarithm is normally distributed.  
  The two major characteristics of the two-parameter lognormal distribution 
  are that it is bounded below at 0, and it is skewed to the right.  The 
  three-parameter lognormal distribution is a generalization of the two-parameter 
  lognormal distribution in which the distribution is shifted so that the 
  threshold parameter is some arbitrary number, not necessarily 0.

  The three-parameter lognormal distribution was introduced by Wicksell (1917) in a 
  study of the distribution of ages at first marriage.  Both the two- and 
  three-parameter lognormal distributions have been used in a variety of fields, 
  including economics and business, industry, biology, ecology, atmospheric science, 
  and geology (Crow and Shimizu, 1988).  Royston (1992) has discussed the application 
  of the three-parameter lognormal distribution in the field of medicine.

  The two-parameter lognormal distribution is often used to characterize chemical 
  concentrations in the environment.  Ott (1990) has shown mathematically how a 
  series of successive random dilutions gives rise to a distribution that can be 
  approximated by a two-parameter lognormal distribution.

  The three-pararameter lognormal distribution starts to resemble a normal 
  distribution as the parameter \eqn{\sigma} (the standard deviation of 
  \eqn{log(X-\gamma)} tends to 0.
}
\seealso{
  \link[stats:Lognormal]{Lognormal}, \code{\link{elnorm3}},  
  \link{Probability Distributions and Random Numbers}.
}
\examples{
  # Density of the three-parameter lognormal distribution with 
  # parameters meanlog=1, sdlog=2, and threshold=10, evaluated at 10.5:

  dlnorm3(10.5, 1, 2, 10) 
  #[1] 0.278794

  #----------

  # The cdf of the three-parameter lognormal distribution with 
  # parameters meanlog=2, sdlog=3, and threshold=5, evaluated at 9: 

  plnorm3(9, 2, 3, 5) 
  #[1] 0.4189546

  #----------

  # The median of the three-parameter lognormal distribution with 
  # parameters meanlog=2, sdlog=3, and threshold=20: 

  qlnorm3(0.5, 2, 3, 20) 
  #[1] 27.38906

  #----------

  # Random sample of 3 observations from the three-parameter lognormal 
  # distribution with parameters meanlog=2, sdlog=1, and threshold=-5. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(20) 
  rlnorm3(3, 2, 1, -5) 
  #[1] 18.6339749 -0.8873173 39.0561521
}
\keyword{ distribution }
\keyword{ datagen }

\name{linearTrendTestN}
\alias{linearTrendTestN}
\title{
  Sample Size for a t-Test for Linear Trend
}
\description{
  Compute the sample size necessary to achieve a specified power for a t-test for 
  linear trend, given the scaled slope and significance level.
}
\usage{
  linearTrendTestN(slope.over.sigma, alpha = 0.05, power = 0.95, 
    alternative = "two.sided", approx = FALSE, round.up = TRUE, 
    n.max = 5000, tol = 1e-07, maxiter = 1000)
}
\arguments{
  \item{slope.over.sigma}{
  numeric vector specifying the ratio of the true slope to the standard deviation of 
  the error terms (\eqn{\sigma}).  This is also called the "scaled slope".  The 
  default value is \code{slope.over.sigma=0}.
}
  \item{alpha}{
  numeric vector of numbers between 0 and 1 indicating the Type I error level 
  associated with the hypothesis test.  The default value is \code{alpha=0.05}.
}
  \item{power}{
  numeric vector of numbers between 0 and 1 indicating the power 
  associated with the hypothesis test.  The default value is \code{power=0.95}.
}
  \item{alternative}{
  character string indicating the kind of alternative hypothesis.  The possible values 
  are \code{"two.sided"} (the default), \code{"greater"}, and \code{"less"}.
}
  \item{approx}{
  logical scalar indicating whether to compute the power based on an approximation to 
  the non-central t-distribution.  The default value is \code{approx=FALSE}.
}
  \item{round.up}{
  logical scalar indicating whether to round up the values of the computed 
  sample size(s) to the next smallest integer.  The default value is 
  \code{TRUE}.
}
  \item{n.max}{
  positive integer greater than 2 indicating the maximum sample size.  
  The default value is \code{n.max=5000}.
}
  \item{tol}{
  numeric scalar indicating the toloerance to use in the 
  \code{\link{uniroot}} search algorithm.  
  The default value is \code{tol=1e-7}.
}
  \item{maxiter}{
  positive integer indicating the maximum number of iterations 
  argument to pass to the \code{\link{uniroot}} function.  The default 
  value is \code{maxiter=1000}.
}
}
\details{
  If the arguments \code{slope.over.sigma}, \code{alpha}, and \code{power} are not 
  all the same length, they are replicated to be the same length as the length of 
  the longest argument.

  Formulas for the power of the t-test of linear trend for specified values of 
  the sample size, scaled slope, and Type I error level are given in 
  the help file for \code{\link{linearTrendTestPower}}.  The function 
  \code{linearTrendTestN} uses the \code{\link{uniroot}} search algorithm to 
  determine the required sample size(s) for specified values of the power, 
  scaled slope, and Type I error level. 
}
\value{
  a numeric vector of sample sizes.
}
\references{
  See the help file for \code{\link{linearTrendTestPower}}.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help file for \code{\link{linearTrendTestPower}}.
}
\seealso{
  \code{\link{linearTrendTestPower}}, \code{\link{linearTrendTestScaledMds}}, 
  \code{\link{plotLinearTrendTestDesign}}, \code{\link{lm}}, \cr
  \code{\link{summary.lm}}, \code{\link{kendallTrendTest}}, 
  \link{Power and Sample Size}, \link{Normal}, \code{\link{t.test}}.
}
\examples{
  # Look at how the required sample size for the t-test for zero slope 
  # increases with increasing required power:

  seq(0.5, 0.9, by = 0.1) 
  #[1] 0.5 0.6 0.7 0.8 0.9 

  linearTrendTestN(slope.over.sigma = 0.1, power = seq(0.5, 0.9, by = 0.1)) 
  #[1] 18 19 21 22 25

  #----------

  # Repeat the last example, but compute the sample size based on the approximate 
  # power instead of the exact:

  linearTrendTestN(slope.over.sigma = 0.1, power = seq(0.5, 0.9, by = 0.1), 
    approx = TRUE) 
  #[1] 18 19 21 22 25

  #==========

  # Look at how the required sample size for the t-test for zero slope decreases 
  # with increasing scaled slope:

  seq(0.05, 0.2, by = 0.05) 
  #[1] 0.05 0.10 0.15 0.20 

  linearTrendTestN(slope.over.sigma = seq(0.05, 0.2, by = 0.05)) 
  #[1] 41 26 20 17

  #==========

  # Look at how the required sample size for the t-test for zero slope decreases 
  # with increasing values of Type I error:

  linearTrendTestN(slope.over.sigma = 0.1, alpha = c(0.001, 0.01, 0.05, 0.1)) 
  #[1] 33 29 26 25
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }

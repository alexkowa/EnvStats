\name{linearTrendTestScaledMds}
\alias{linearTrendTestScaledMds}
\title{
  Scaled Minimal Detectable Slope for a t-Test for Linear Trend
}
\description{
  Compute the scaled minimal detectable slope associated with a t-test for liner 
  trend, given the sample size or predictor variable values, power, and 
  significance level.
}
\usage{
  linearTrendTestScaledMds(n, x = lapply(n, seq), alpha = 0.05, power = 0.95, 
    alternative = "two.sided", two.sided.direction = "greater", approx = FALSE, 
    tol = 1e-07, maxiter = 1000)
}
\arguments{
  \item{n}{
  numeric vector of sample sizes.  All values of \code{n} must be positive integers 
  larger than 2.  This argument is ignored when \code{x} is supplied.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are not allowed.
}
  \item{x}{
  numeric vector of predictor variable values, or a list in which each component is 
  a numeric vector of predictor variable values.  Usually, the predictor variable is 
  time (e.g., days, months, quarters, etc.).  The default value is 
  \code{x=lapply(n,seq)}, which yields a list in which the i'th component is the 
  seqence of integers from 1 to the i'th value of the vector \code{n}.  If \code{x} 
  is a numeric vector, it must contain at least three elements, two of which must be 
  unique.  If \code{x} is a list of numeric vectors, each component of \code{x} 
  must contain at least three elements, two of which must be unique.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are not allowed.
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
  \item{two.sided.direction}{
  character string indicating the direction (positive or negative) for the 
  scaled minimal detectable slope when \code{alternative="two.sided"}.  When \cr
  \code{two.sided.direction="greater"} (the default), the scaled minimal 
  detectable slope is positive.  When \code{two.sided.direction="less"}, 
  the scaled minimal detectable slope is negative.  This argument 
  is ignored if \code{alternative="less"} or \code{alternative="greater"}.
}
  \item{approx}{
  logical scalar indicating whether to compute the power based on an approximation to 
  the non-central t-distribution.  The default value is \code{approx=FALSE}.
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
  If the argument \code{x} is a vector, it is converted into a list with one 
  component.  If the arguments \code{n}, \code{x}, \code{alpha}, and 
  \code{power} are not all the same length, they are replicated to be the same 
  length as the length of the longest argument.

  Formulas for the power of the t-test of linear trend for specified values of 
  the sample size, scaled slope, and Type I error level are given in 
  the help file for \code{\link{linearTrendTestPower}}.  The function 
  \code{linearTrendTestScaledMds} uses the \code{\link{uniroot}} search algorithm to 
  determine the minimal detectable scaled slope for specified values of the power, 
  sample size, and Type I error level. 
}
\value{
  numeric vector of computed scaled minimal detectable slopes.  When 
  \code{alternative="less"}, or \code{alternative="two.sided"} and 
  \code{two.sided.direction="less"}, the computed slopes are negative.  Otherwise, 
  the slopes are positive.
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
  \code{\link{linearTrendTestPower}}, \code{\link{linearTrendTestN}}, 
  \code{\link{plotLinearTrendTestDesign}}, \code{\link{lm}}, 
  \code{\link{summary.lm}}, \code{\link{kendallTrendTest}}, 
  \link{Power and Sample Size}, \link{Normal}, \code{\link{t.test}}.
}
\examples{
  # Look at how the scaled minimal detectable slope for the t-test for linear 
  # trend increases with increasing required power:

  seq(0.5, 0.9, by = 0.1) 
  #[1] 0.5 0.6 0.7 0.8 0.9 

  scaled.mds <- linearTrendTestScaledMds(n = 10, power = seq(0.5, 0.9, by = 0.1)) 

  round(scaled.mds, 2) 
  #[1] 0.25 0.28 0.31 0.35 0.41

  #----------

  # Repeat the last example, but compute the scaled minimal detectable slopes 
  # based on the approximate power instead of the exact:

  scaled.mds <- linearTrendTestScaledMds(n = 10, power = seq(0.5, 0.9, by = 0.1), 
    approx = TRUE) 

  round(scaled.mds, 2) 
  #[1] 0.25 0.28 0.31 0.35 0.41

  #==========

  # Look at how the scaled minimal detectable slope for the t-test for linear trend 
  # decreases with increasing sample size:

  seq(10, 50, by = 10) 
  #[1] 10 20 30 40 50 

  scaled.mds <- linearTrendTestScaledMds(seq(10, 50, by = 10), alternative = "greater") 

  round(scaled.mds, 2) 
  #[1] 0.40 0.13 0.07 0.05 0.03

  #==========

  # Look at how the scaled minimal detectable slope for the t-test for linear trend 
  # decreases with increasing values of Type I error:

  scaled.mds <- linearTrendTestScaledMds(10, alpha = c(0.001, 0.01, 0.05, 0.1), 
    alternative="greater") 

  round(scaled.mds, 2) 
  #[1] 0.76 0.53 0.40 0.34

  #----------

  # Repeat the last example, but compute the scaled minimal detectable slopes 
  # based on the approximate power instead of the exact:
 
  scaled.mds <- linearTrendTestScaledMds(10, alpha = c(0.001, 0.01, 0.05, 0.1), 
    alternative="greater", approx = TRUE) 

  round(scaled.mds, 2) 
  #[1] 0.70 0.52 0.41 0.36

  #==========

  # Clean up
  #---------
  rm(scaled.mds)
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }

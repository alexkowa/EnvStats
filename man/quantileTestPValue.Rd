\name{quantileTestPValue}
\alias{quantileTestPValue}
\title{
  Compute p-Value for the Quantile Test
}
\description{
  Compute the p-value associated with a specified combination of 
  \eqn{m}, \eqn{n}, \eqn{r}, and \eqn{k} for the 
  \link[=quantileTest]{quantile test} (useful for determining \eqn{r} and 
  \eqn{k} for a given significance level \eqn{\alpha}).
}
\usage{
  quantileTestPValue(m, n, r, k, exact.p = TRUE)
}
\arguments{
  \item{m}{
  numeric vector of integers indicating the number of observations from the 
  \dQuote{treatment} group.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{n}{
  numeric vector of integers indicating the number of observations from the 
  \dQuote{reference} group.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{r}{
  numeric vector of integers indicating the ranks of the observations to use as the 
  lower cut off for the quantile test.  All values of \code{r} must be greater than 
  or equal to 2 and less than or equal to the corresponding elements of 
  \code{m+n} (the total number of observations from both groups).  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{k}{
  numeric vector of integers indicating the number of observations from the 
  \dQuote{treatment} group contained in the \eqn{r} largest observations.  This is 
  the critical value used to decide whether to reject the null hypothesis.  
  All values of \code{k} must be greater than or equal to 0 and less than or equal 
  to the corresponding elements of \code{r}.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{exact.p}{
  logical scalar indicating whether to compute the p-value based on the exact 
  distribution of the test statistic (\code{exact.p=TRUE}; the default) or based on 
  the normal approximation (\code{exact.p=FALSE}).  
}
}
\details{
  If the arguments \code{m}, \code{n}, \code{r}, and \code{k} are not all the same 
  length, they are replicated to be the same length as the length of the longest 
  argument.

  For details on how the p-value is computed, see the help file for 
  \code{\link{quantileTest}}.

  The function \code{quantileTestPValue} is useful for determining what values to 
  use for \code{r} and \code{k}, given the values of \code{m}, \code{n}, and a 
  specified significance level \eqn{\alpha}.  The function 
  \code{quantileTestPValue} can be used to reproduce Tables A.6-A.9 in 
  USEPA (1994, pp.A.22-A.25).
}
\value{
  numeric vector of p-values.
}
\references{
  See the help file for \code{\link{quantileTest}}.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help file for \code{\link{quantileTest}}.
}
\seealso{
  \code{\link{quantileTest}}, \code{\link{wilcox.test}}, 
  \code{\link{htest.object}}, \link{Hypothesis Tests}.
}
\examples{
  # Reproduce the first column of Table A.9 in USEPA (1994, p.A.25):
  #-----------------------------------------------------------------

  p.vals <- quantileTestPValue(m = 5, n = seq(15, 45, by = 5), 
    r = c(9, 3, 4, 4, 5, 5, 6), k = c(4, 2, 2, 2, 2, 2, 2)) 

  round(p.vals, 3) 
  #[1] 0.098 0.091 0.119 0.089 0.109 0.087 0.103 

  #==========

  # Clean up
  #---------

  rm(p.vals)
}
\keyword{htest}
\keyword{models}
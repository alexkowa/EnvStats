\name{tolIntNormN}
\alias{tolIntNormN}
\title{
  Sample Size for a Specified Half-Width of a Tolerance Interval for a Normal Distribution
}
\description{
  Compute the sample size necessary to achieve a specified half-width of a 
  tolerance interval for a normal distribution, given the estimated standard 
  deviation, coverage, and confidence level.
}
\usage{
  tolIntNormN(half.width, sigma.hat = 1, coverage = 0.95, cov.type = "content", 
    conf.level = 0.95, method = "wald.wolfowitz", round.up = TRUE, n.max = 5000, 
    tol = 1e-07, maxiter = 1000)
}
\arguments{
  \item{half.width}{
  numeric vector of (positive) half-widths.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
}
  \item{sigma.hat}{
  numeric vector specifying the value(s) of the estimated standard deviation(s).  
  The default value is \code{sigma.hat=1}.
}
  \item{coverage}{
  numeric vector of values between 0 and 1 indicating the desired coverage of the 
  tolerance interval.  The default value is \code{coverage=0.95}.
}
  \item{cov.type}{
  character string specifying the coverage type for the tolerance interval.  The 
  possible values are \code{"content"} (\eqn{\beta}-content; the default), and 
  \code{"expectation"} (\eqn{\beta}-expectation). 
}
  \item{conf.level}{
  numeric vector of values between 0 and 1 indicating the confidence level of the 
  prediction interval.  The default value is \code{conf.level=0.95}.
}
  \item{method}{
  character string specifying the method for constructing the tolerance interval.  
  The possible values are \code{"exact"} (the default) and \code{"wald.wolfowitz"} 
  (the Wald-Wolfowitz approximation).
}
  \item{round.up}{
  logical scalar indicating whether to round up the values of the computed sample 
  size(s) to the next smallest integer.  The default value is \code{round.up=TRUE}.
}
  \item{n.max}{
  positive integer greater than 1 specifying the maximum possible sample size.  
  The default value is \code{n.max=5000}.
}
  \item{tol}{
  numeric scalar indicating the tolerance to use in the \code{\link{uniroot}} 
  search algorithm.  The default value is \code{tol=1e-7}.
}
  \item{maxiter}{
  positive integer indicating the maximum number of iterations to use in the 
  \code{\link{uniroot}} search algorithm.  The default value is 
  \code{maxiter=1000}.
}
}
\details{
  If the arguments \code{half.width}, \code{sigma.hat}, \code{coverage}, and 
  \code{conf.level} are not all the same length, they are replicated to be the same 
  length as the length of the longest argument.

  The help files for \code{\link{tolIntNorm}} and \code{\link{tolIntNormK}} 
  give formulas for a two-sided tolerance interval based on the sample size, the 
  observed sample mean and sample standard deviation, and specified confidence level 
  and coverage.  Specifically, the two-sided tolerance interval is given by:
  \deqn{[\bar{x} - Ks, \bar{x} + Ks] \;\;\;\;\;\; (1)} 
  where \eqn{\bar{x}} denotes the sample mean:
  \deqn{\bar{x} = \frac{1}{n} \sum_{i=1}^n x_i \;\;\;\;\;\; (2)}
  \eqn{s} denotes the sample standard deviation:
  \deqn{s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\;\;\; (3)}
  and \eqn{K} denotes a constant that depends on the sample size \eqn{n}, the 
  confidence level, and the coverage (see the help file for 
  \code{\link{tolIntNormK}}).  Thus, the half-width of the tolerance interval is 
  given by:
  \deqn{HW = Ks \;\;\;\;\;\; (4)}

  The function \code{tolIntNormN} uses the \code{\link{uniroot}} search algorithm to 
  determine the sample size for specified values of the half-width, sample 
  standard deviation, coverage, and confidence level.  \bold{Note that unlike a 
  confidence interval, the half-width of a tolerance interval does \emph{not} 
  approach 0 as the sample size increases.}
}
\value{
  numeric vector of sample sizes.
}
\references{
  See the help file for \code{\link{tolIntNorm}}.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help file for \code{\link{tolIntNorm}}.

  In the course of designing a sampling program, an environmental scientist may wish 
  to determine the relationship between sample size, confidence level, and half-width 
  if one of the objectives of the sampling program is to produce tolerance intervals.  
  The functions \code{\link{tolIntNormHalfWidth}}, \code{tolIntNormN}, and 
  \code{\link{plotTolIntNormDesign}} can be used to investigate these 
  relationships for the case of normally-distributed observations.
}
\seealso{
  \code{\link{tolIntNorm}}, \code{\link{tolIntNormK}}, 
  \code{\link{tolIntNormHalfWidth}}, \code{\link{plotTolIntNormDesign}}, 
  \code{\link{Normal}}.
}
\examples{
  # Look at how the required sample size for a tolerance interval increases 
  # with increasing coverage:

  seq(0.5, 0.9, by = 0.1) 
  #[1] 0.5 0.6 0.7 0.8 0.9 

  tolIntNormN(half.width = 3, coverage = seq(0.5, 0.9, by = 0.1)) 
  #[1] 4 4 5 6 9

  #----------

  # Look at how the required sample size for a tolerance interval decreases 
  # with increasing half-width:

  3:6 
  #[1] 3 4 5 6 

  tolIntNormN(half.width = 3:6) 
  #[1] 15 8 6 5 

  tolIntNormN(3:6, round = FALSE) 
  #[1] 14.199735  7.022572  5.092374  4.214371

  #----------

  # Look at how the required sample size for a tolerance interval increases 
  # with increasing estimated standard deviation for a fixed half-width:

  seq(0.5, 2, by = 0.5) 
  #[1] 0.5 1.0 1.5 2.0 

  tolIntNormN(half.width = 4, sigma.hat = seq(0.5, 2, by = 0.5)) 
  #[1]    4    8   24 3437

  #----------

  # Look at how the required sample size for a tolerance interval increases 
  # with increasing confidence level for a fixed half-width:

  seq(0.5, 0.9, by = 0.1) 
  #[1] 0.5 0.6 0.7 0.8 0.9 

  tolIntNormN(half.width = 3, conf.level = seq(0.5, 0.9, by = 0.1)) 
  #[1]  3  4  5  7 11

  #==========

  # Example 17-3 of USEPA (2009, p. 17-17) shows how to construct a 
  # beta-content upper tolerance limit with 95% coverage and 95% 
  # confidence  using chrysene data and assuming a lognormal distribution.  
  # The data for this example are stored in EPA.09.Ex.17.3.chrysene.df, 
  # which contains chrysene concentration data (ppb) found in water 
  # samples obtained from two background wells (Wells 1 and 2) and 
  # three compliance wells (Wells 3, 4, and 5).  The tolerance limit 
  # is based on the data from the background wells.

  # Here we will first take the log of the data and then estimate the 
  # standard deviation based on the two background wells.  We will use this 
  # estimate of standard deviation to compute required sample sizes for 
  # various half-widths on the log-scale.

  head(EPA.09.Ex.17.3.chrysene.df)
  #  Month   Well  Well.type Chrysene.ppb
  #1     1 Well.1 Background         19.7
  #2     2 Well.1 Background         39.2
  #3     3 Well.1 Background          7.8
  #4     4 Well.1 Background         12.8
  #5     1 Well.2 Background         10.2
  #6     2 Well.2 Background          7.2

  longToWide(EPA.09.Ex.17.3.chrysene.df, "Chrysene.ppb", "Month", "Well")
  #  Well.1 Well.2 Well.3 Well.4 Well.5
  #1   19.7   10.2   68.0   26.8   47.0
  #2   39.2    7.2   48.9   17.7   30.5
  #3    7.8   16.1   30.1   31.9   15.0
  #4   12.8    5.7   38.1   22.2   23.4

  summary.stats <- summaryStats(log(Chrysene.ppb) ~ Well.type, 
    data = EPA.09.Ex.17.3.chrysene.df)

  summary.stats
  #            N   Mean     SD Median    Min    Max
  #Background  8 2.5086 0.6279 2.4359 1.7405 3.6687
  #Compliance 12 3.4173 0.4361 3.4111 2.7081 4.2195

  sigma.hat <- summary.stats["Background", "SD"]
  sigma.hat
  #[1] 0.6279

  tolIntNormN(half.width = c(4, 2, 1), sigma.hat = sigma.hat)
  #[1]  4 12 NA
  #Warning message:
  #In tolIntNormN(half.width = c(4, 2, 1), sigma.hat = sigma.hat) :
  #  Value of 'half.width' is too smallfor element3.  
  #  Try increasing the value of 'n.max'.

  # NOTE:  We cannot achieve a half-width of 1 for the given value of 
  #        sigma.hat for a tolerance interval with 95% coverage and 
  #        95% confidence.  The default value of n.max is 5000, but in fact, 
  #        even with a million observations the half width is greater than 1.

  tolIntNormHalfWidth(n = 1e6, sigma.hat = sigma.hat)
  #[1] 1.232095

  #==========

  # Clean up
  #---------
  rm(summary.stats, sigma.hat)
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }

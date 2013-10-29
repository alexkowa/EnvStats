\name{predIntNormN}
\alias{predIntNormN}
\title{
  Sample Size for a Specified Half-Width of a Prediction Interval for the next \eqn{k} Observations from a Normal Distribution
}
\description{
  Compute the sample size necessary to achieve a specified half-width of a 
  prediction interval for the next \eqn{k} observations from a normal distribution.
}
\usage{
  predIntNormN(half.width, n.mean = 1, k = 1, sigma.hat = 1, 
    method = "Bonferroni", conf.level = 0.95, round.up = TRUE, 
    n.max = 5000, tol = 1e-07, maxiter = 1000)
}
\arguments{
  \item{half.width}{
  numeric vector of (positive) half-widths.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
}
  \item{n.mean}{
  numeric vector of positive integers specifying the sample size associated with 
  the \eqn{k} future \emph{averages}.  The default value is 
  \code{n.mean=1} (i.e., individual observations).  Note that all future averages 
  must be based on the same sample size.
}
  \item{k}{
  numeric vector of positive integers specifying the number of future observations 
  or averages the prediction interval should contain with confidence level 
  \code{conf.level}.  The default value is \code{k=1}.
}
  \item{sigma.hat}{
  numeric vector specifying the value(s) of the estimated standard deviation(s).  
  The default value is \code{sigma.hat=1}.
}
  \item{method}{
  character string specifying the method to use if the number of future observations 
  (\code{k}) is greater than 1.  The possible values are \code{method="Bonferroni"} 
  (approximate method based on Bonferonni inequality; the default), and \cr
  \code{method="exact"} (exact method due to Dunnett, 1955).  
  This argument is ignored if \code{k=1}.
}
  \item{conf.level}{
  numeric vector of values between 0 and 1 indicating the confidence level of the 
  prediction interval.  The default value is \code{conf.level=0.95}.
}
  \item{round.up}{
  logical scalar indicating whether to round up the values of the computed sample 
  size(s) to the next smallest integer.  The default value is \code{round.up=TRUE}.
}
  \item{n.max}{
  positive integer greater than 1 indicating the maximum possible sample size.  The 
  default value is \code{n.max=5000}.
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
  If the arguments \code{half.width}, \code{k}, \code{n.mean}, \code{sigma.hat}, and 
  \code{conf.level} are not all the same length, they are replicated to be the same 
  length as the length of the longest argument.

  The help files for \code{\link{predIntNorm}} and \code{\link{predIntNormK}} 
  give formulas for a two-sided prediction interval based on the sample size, the 
  observed sample mean and sample standard deviation, and specified confidence level.  
  Specifically, the two-sided prediction interval is given by:
  \deqn{[\bar{x} - Ks, \bar{x} + Ks] \;\;\;\;\;\; (1)} 
  where \eqn{\bar{x}} denotes the sample mean:
  \deqn{\bar{x} = \frac{1}{n} \sum_{i=1}^n x_i \;\;\;\;\;\; (2)}
  \eqn{s} denotes the sample standard deviation:
  \deqn{s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\;\;\; (3)}
  and \eqn{K} denotes a constant that depends on the sample size \eqn{n}, the 
  confidence level, the number of future averages \eqn{k}, and the 
  sample size associated with the future averages, \eqn{m} (see the help file for 
  \code{\link{predIntNormK}}).  Thus, the half-width of the prediction interval is 
  given by:
  \deqn{HW = Ks \;\;\;\;\;\; (4)}

  The function \code{predIntNormN} uses the \code{\link{uniroot}} search algorithm to 
  determine the sample size for specified values of the half-width, number of 
  observations used to create a single future average, number of future observations or 
  averages, the sample standard deviation, and the confidence level.  \bold{Note that 
  unlike a confidence interval, the half-width of a prediction interval does \emph{not} 
  approach 0 as the sample size increases.}
}
\value{
  numeric vector of sample sizes.
}
\references{
  See the help file for \code{\link{predIntNorm}}.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help file for \code{\link{predIntNorm}}.
}
\seealso{
  \code{\link{predIntNorm}}, \code{\link{predIntNormK}}, 
  \code{\link{predIntNormHalfWidth}}, \code{\link{plotPredIntNormDesign}}.
}
\examples{
  # Look at how the required sample size for a prediction interval increases 
  # with increasing number of future observations:

  1:5 
  #[1] 1 2 3 4 5 

  predIntNormN(half.width = 3, k = 1:5) 
  #[1]  6  9 11 14 18

  #----------

  # Look at how the required sample size for a prediction interval decreases 
  # with increasing half-width:

  2:5 
  #[1] 2 3 4 5 

  predIntNormN(half.width = 2:5) 
  #[1] 86  6  4  3

  predIntNormN(2:5, round = FALSE) 
  #[1] 85.567387  5.122911  3.542393  2.987861

  #----------

  # Look at how the required sample size for a prediction interval increases 
  # with increasing estimated standard deviation for a fixed half-width:

  seq(0.5, 2, by = 0.5) 
  #[1] 0.5 1.0 1.5 2.0 

  predIntNormN(half.width = 4, sigma.hat = seq(0.5, 2, by = 0.5)) 
  #[1]  3  4  7 86

  #----------

  # Look at how the required sample size for a prediction interval increases 
  # with increasing confidence level for a fixed half-width:

  seq(0.5, 0.9, by = 0.1) 
  #[1] 0.5 0.6 0.7 0.8 0.9 

  predIntNormN(half.width = 2, conf.level = seq(0.5, 0.9, by = 0.1)) 
  #[1] 2 2 3 4 9

  #==========

  # The data frame EPA.92c.arsenic3.df contains arsenic concentrations (ppb) 
  # collected quarterly for 3 years at a background well and quarterly for 
  # 2 years at a compliance well.  Using the data from the background well, 
  # compute the required sample size in order to achieve a half-width of 
  # 2.25, 2.5, or 3 times the estimated standard deviation for a two-sided 
  # 90% prediction interval for k=4 future observations.
  #
  # For a half-width of 2.25 standard deviations, the required sample size is 526, 
  # or about 131 years of quarterly observations!  For a half-width of 2.5 
  # standard deviations, the required sample size is 20, or about 5 years of 
  # quarterly observations.  For a half-width of 3 standard deviations, the required 
  # sample size is 9, or about 2 years of quarterly observations.

  EPA.92c.arsenic3.df
  #   Arsenic Year  Well.type
  #1     12.6    1 Background
  #2     30.8    1 Background
  #3     52.0    1 Background
  #...
  #18     3.8    5 Compliance
  #19     2.6    5 Compliance
  #20    51.9    5 Compliance

  mu.hat <- with(EPA.92c.arsenic3.df, 
    mean(Arsenic[Well.type=="Background"])) 

  mu.hat 
  #[1] 27.51667 

  sigma.hat <- with(EPA.92c.arsenic3.df, 
    sd(Arsenic[Well.type=="Background"]))

  sigma.hat 
  #[1] 17.10119 

  predIntNormN(half.width=c(2.25, 2.5, 3) * sigma.hat, k = 4, 
    sigma.hat = sigma.hat, conf.level = 0.9) 
  #[1] 526  20   9 

  #==========

  # Clean up
  #---------
  rm(mu.hat, sigma.hat)
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }

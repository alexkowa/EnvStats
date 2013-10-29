\name{predIntNormHalfWidth}
\alias{predIntNormHalfWidth}
\title{
  Half-Width of a Prediction Interval for the next \eqn{k} Observations from a Normal Distribution
}
\description{
  Compute the half-width of a prediction interval for the next \eqn{k} observations 
  from a normal distribution.
}
\usage{
  predIntNormHalfWidth(n, df = n - 1, n.mean = 1, k = 1, sigma.hat = 1, 
    method = "Bonferroni", conf.level = 0.95)
}
\arguments{
  \item{n}{
  numeric vector of positive integers greater than 1 indicating the sample size upon 
  which the prediction interval is based.
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are not allowed. 
}
  \item{df}{
  numeric vector of positive integers indicating the degrees of freedom associated 
  with the prediction interval.  The default is \code{df=n-1}.
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
}
\details{
  If the arguments \code{n}, \code{k}, \code{n.mean}, \code{sigma.hat}, and 
  \code{conf.level} are not all the same length, they are replicated to be the 
  same length as the length of the longest argument.

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
}
\value{
  numeric vector of half-widths.
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
  \code{\link{predIntNormN}}, \code{\link{plotPredIntNormDesign}}.
}
\examples{
  # Look at how the half-width of a prediction interval increases with 
  # increasing number of future observations:

  1:5 
  #[1] 1 2 3 4 5 

  hw <- predIntNormHalfWidth(n = 10, k = 1:5)

  round(hw, 2) 
  #[1] 2.37 2.82 3.08 3.26 3.41

  #----------

  # Look at how the half-width of a prediction interval decreases with 
  # increasing sample size:

  2:5 
  #[1] 2 3 4 5 

  hw <- predIntNormHalfWidth(n = 2:5)

  round(hw, 2) 
  #[1] 15.56  4.97  3.56  3.04

  #----------

  # Look at how the half-width of a prediction interval increases with 
  # increasing estimated standard deviation for a fixed sample size:

  seq(0.5, 2, by = 0.5) 
  #[1] 0.5 1.0 1.5 2.0 

  hw <- predIntNormHalfWidth(n = 10, sigma.hat = seq(0.5, 2, by = 0.5))

  round(hw, 2) 
  #[1] 1.19 2.37 3.56 4.75

  #----------

  # Look at how the half-width of a prediction interval increases with 
  # increasing confidence level for a fixed sample size:

  seq(0.5, 0.9, by = 0.1) 
  #[1] 0.5 0.6 0.7 0.8 0.9 

  hw <- predIntNormHalfWidth(n = 5, conf = seq(0.5, 0.9, by = 0.1))

  round(hw, 2) 
  #[1] 0.81 1.03 1.30 1.68 2.34

  #==========

  # The data frame EPA.92c.arsenic3.df contains arsenic concentrations (ppb) 
  # collected quarterly for 3 years at a background well and quarterly for 
  # 2 years at a compliance well.  Using the data from the background well, compute 
  # the half-width associated with sample sizes of 12 (3 years of quarterly data), 
  # 16 (4 years of quarterly data), and 20 (5 years of quarterly data) for a 
  # two-sided 90% prediction interval for k=4 future observations.

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

  hw <- predIntNormHalfWidth(n = c(12, 16, 20), k = 4, sigma.hat = sigma.hat, 
    conf.level = 0.9)

  round(hw, 2) 
  #[1] 46.16 43.89 42.64

  #==========

  # Clean up
  #---------
  rm(hw, mu.hat, sigma.hat)
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }

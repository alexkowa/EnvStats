\name{predIntNparN}
\alias{predIntNparN}
\title{
  Sample Size for a Nonparametric Prediction Interval for a Continuous Distribution
}
\description{
  Compute the sample size necessary for a nonparametric prediction interval to 
  contain at least \eqn{k} out of the next \eqn{m} future observations with 
  probability \eqn{(1-\alpha)100\%} for a continuous distribution.
}
\usage{
  predIntNparN(k = m, m = 1, lpl.rank = ifelse(pi.type == "upper", 0, 1), 
    n.plus.one.minus.upl.rank = ifelse(pi.type == "lower", 0, 1), 
    pi.type = "two.sided", conf.level = 0.95, n.max = 5000, maxiter = 1000)
}
\arguments{
  \item{k}{
  vector of positive integers specifying the minimum number of future 
  observations out of \code{m} that should be contained in the prediction interval.  
  The default value is \code{k=m}.
}
  \item{m}{
  vector of positive integers specifying the number of future observations.  
  The default value is \code{m=1}.
}
  \item{lpl.rank}{
  vector of positive integers indicating the rank of the order statistic to use for 
  the lower bound of the prediction interval.  If \code{pi.type="two-sided"} or \cr
  \code{pi.type="lower"}, the default value is \code{lpl.rank=1} (implying the 
  minimum value is used as the lower bound of the prediction interval).  
  If \code{pi.type="upper"}, this argument is set equal to \code{0}.
}
  \item{n.plus.one.minus.upl.rank}{
  vector of positive integers related to the rank of the order statistic to use for 
  the upper bound of the prediction interval.  A value of 
  \code{n.plus.one.minus.upl.rank=1} (the default) means use the 
  first largest value, and in general a value of \cr
  \code{n.plus.one.minus.upl.rank=}\eqn{i} means use the \eqn{i}'th largest value.  
  If \cr
  \code{pi.type="lower"}, this argument is set equal to \code{0}.
}
  \item{pi.type}{
  character string indicating what kind of prediction interval to compute.  
  The possible values are \code{"two.sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.
}
  \item{conf.level}{
  numeric vector of values between 0 and 1 indicating the confidence level 
  associated with the prediction interval.  The default value is 
  \code{conf.level=0.95}.
}
  \item{n.max}{
  positive integer greater than 1 indicating the maximum possible sample size.  The 
  default value is \code{n.max=5000}.
}
  \item{maxiter}{
  positive integer indicating the maximum number of iterations to use in the 
  \code{\link{uniroot}} search algorithm.  The default value is 
  \code{maxiter=1000}.
}
}
\details{
  If the arguments \code{k}, \code{m}, \code{lpl.rank}, and 
  \code{n.plus.one.minus.upl.rank} are not all the same length, they are replicated 
  to be the same length as the length of the longest argument.

  The function \code{predIntNparN} initially computes the required sample size \eqn{n} 
  by solving Equation (11) or (12) in the help file for \code{\link{predIntNpar}} for 
  \eqn{n}, depending on the value of the argument \code{pi.type}.  If \code{k < m}, 
  \code{lpl.rank > 1} (two-sided and lower prediction intervals only), or \cr
  \code{n.plus.one.minus.upl.rank > 1} (two-sided and upper prediction intervals only), 
  then this initial value of \eqn{n} is used as the upper bound in a binary search 
  based on Equation (8) in the help file for \code{\link{predIntNpar}} and is 
  implemented via the \R function \code{\link{uniroot}} with the argument 
  \code{tolerance} set to \code{1}.
}
\value{
  vector of positive integers indicating the required sample size(s) for the 
  specified nonparametric prediction interval(s).
}
\references{
  See the help file for \code{\link{predIntNpar}}.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help file for \code{\link{predIntNpar}}.
}
\seealso{
  \code{\link{predIntNpar}}, \code{\link{predIntNparConfLevel}}, 
  \code{\link{plotPredIntNparDesign}}.
}
\examples{
  # Look at how the required sample size for a nonparametric prediction interval 
  # increases with increasing confidence level:

  seq(0.5, 0.9, by = 0.1) 
  #[1] 0.5 0.6 0.7 0.8 0.9 

  predIntNparN(conf.level = seq(0.5, 0.9, by = 0.1)) 
  #[1] 3 4 6 9 19

  #----------

  # Look at how the required sample size for a nonparametric prediction interval 
  # increases with number of future observations (m):

  1:5
  #[1] 1 2 3 4 5

  predIntNparN(m = 1:5) 
  #[1] 39 78 116 155 193

  #----------

  # Look at how the required sample size for a nonparametric prediction interval 
  # increases with minimum number of observations that must be contained within 
  # the interval (k):

  predIntNparN(k = 1:5, m = 5) 
  #[1] 4 7 13 30 193

  #----------

  # Look at how the required sample size for a nonparametric prediction interval 
  # increases with the rank of the lower prediction limit:

  predIntNparN(lpl.rank = 1:5) 
  #[1]  39  59  79 100 119

  #==========

  # Example 18-3 of USEPA (2009, p.18-19) shows how to construct 
  # a one-sided upper nonparametric prediction interval for the next 
  # 4 future observations of trichloroethylene (TCE) at a downgradient well.  
  # The data for this example are stored in EPA.09.Ex.18.3.TCE.df.  
  # There are 6 monthly observations of TCE (ppb) at 3 background wells, 
  # and 4 monthly observations of TCE at a compliance well.

  # Look at the data
  #-----------------

  EPA.09.Ex.18.3.TCE.df

  #   Month Well  Well.type TCE.ppb.orig TCE.ppb Censored
  #1      1 BW-1 Background           <5     5.0     TRUE
  #2      2 BW-1 Background           <5     5.0     TRUE
  #3      3 BW-1 Background            8     8.0    FALSE
  #...
  #22     4 CW-4 Compliance           <5     5.0     TRUE
  #23     5 CW-4 Compliance            8     8.0    FALSE
  #24     6 CW-4 Compliance           14    14.0    FALSE


  longToWide(EPA.09.Ex.18.3.TCE.df, "TCE.ppb.orig", "Month", "Well", 
    paste.row.name = TRUE)

  #        BW-1 BW-2 BW-3 CW-4
  #Month.1   <5    7   <5     
  #Month.2   <5  6.5   <5     
  #Month.3    8   <5 10.5  7.5
  #Month.4   <5    6   <5   <5
  #Month.5    9   12   <5    8
  #Month.6   10   <5    9   14


  # If we construct the prediction limit based on the background well
  # data using the maximum value as the upper prediction limit, 
  # the associated confidence level is only 82%.  
  #-----------------------------------------------------------------

  predIntNparConfLevel(n = 18, m = 4, pi.type = "upper")
  #[1] 0.8181818

  # We would have to collect an additional 18 observations to achieve a 
  # confidence level of at least 90%:

  predIntNparN(m = 4, pi.type = "upper", conf.level = 0.9)
  #[1] 36

  predIntNparConfLevel(n = 36, m = 4, pi.type = "upper")
  #[1] 0.9
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }


\name{tolIntNparConfLevel}
\alias{tolIntNparConfLevel}
\title{
  Confidence Level for Nonparametric Tolerance Interval for Continuous Distribution
}
\description{
  Compute the confidence level associated with a nonparametric \eqn{\beta}-content tolerance 
  interval for a continuous distribution given the sample size, coverage, and ranks of the 
  order statistics used for the interval.
}
\usage{
  tolIntNparConfLevel(n, coverage = 0.95, 
    ltl.rank = ifelse(ti.type == "upper", 0, 1), 
    n.plus.one.minus.utl.rank = ifelse(ti.type == "lower", 0, 1), 
    ti.type = "two.sided")
}
\arguments{
  \item{n}{
  vector of positive integers specifying the sample sizes.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are not allowed.  
}
  \item{coverage}{
  numeric vector of values between 0 and 1 indicating the desired coverage of the 
  \eqn{\beta}-content tolerance interval.
}
  \item{ltl.rank}{
  vector of positive integers indicating the rank of the order statistic to use for the lower bound 
  of the tolerance interval.  If \code{ti.type="two-sided"} or \cr
  \code{ti.type="lower"}, 
  the default value is \code{ltl.rank=1} (implying the minimum value of \code{x} is used 
  as the lower bound of the tolerance interval).  If \cr
  \code{ti.type="upper"}, this argument 
  is set equal to \code{0}.
}
  \item{n.plus.one.minus.utl.rank}{
  vector of positive integers related to the rank of the order statistic to use for 
  the upper bound of the tolerance interval.  A value of 
  \code{n.plus.one.minus.utl.rank=1} (the default) means use the 
  first largest value, and in general a value of \cr
  \code{n.plus.one.minus.utl.rank=}\eqn{i} means use the \eqn{i}'th largest value.  
  If \cr
  \code{ti.type="lower"}, this argument is set equal to \code{0}.
}
  \item{ti.type}{
  character string indicating what kind of tolerance interval to compute.  
  The possible values are \code{"two-sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.
}
}
\details{
  If the arguments \code{n}, \code{coverage}, \code{ltl.rank}, and 
  \code{n.plus.one.minus.utl.rank} are not all the same length, they are replicated to be the 
  same length as the length of the longest argument.

  The help file for \code{\link{tolIntNpar}} explains how nonparametric \eqn{\beta}-content 
  tolerance intervals are constructed and how the confidence level 
  associated with the tolerance interval is computed based on specified values 
  for the sample size, the coverage, and the ranks of the order statistics used for 
  the bounds of the tolerance interval. 
}
\value{
  vector of values between 0 and 1 indicating the confidence level associated with 
  the specified nonparametric tolerance interval.
}
\references{
  See the help file for \code{\link{tolIntNpar}}.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help file for \code{\link{tolIntNpar}}.

  In the course of designing a sampling program, an environmental scientist may wish to determine 
  the relationship between sample size, coverage, and confidence level if one of the objectives of 
  the sampling program is to produce tolerance intervals.  The functions 
  \code{\link{tolIntNparN}}, \code{\link{tolIntNparCoverage}}, \code{tolIntNparConfLevel}, and 
  \code{\link{plotTolIntNparDesign}} can be used to investigate these relationships for 
  constructing nonparametric tolerance intervals.
}
\seealso{
  \code{\link{tolIntNpar}}, \code{\link{tolIntNparN}}, \code{\link{tolIntNparCoverage}}, 
  \code{\link{plotTolIntNparDesign}}.
}
\examples{
  # Look at how the confidence level of a nonparametric tolerance interval increases with 
  # increasing sample size:

  seq(10, 60, by=10) 
  #[1] 10 20 30 40 50 60 

  round(tolIntNparConfLevel(n = seq(10, 60, by = 10)), 2) 
  #[1] 0.09 0.26 0.45 0.60 0.72 0.81

  #----------

  # Look at how the confidence level of a nonparametric tolerance interval decreases with 
  # increasing coverage:

  seq(0.5, 0.9, by = 0.1) 
  #[1] 0.5 0.6 0.7 0.8 0.9 

  round(tolIntNparConfLevel(n = 10, coverage = seq(0.5, 0.9, by = 0.1)), 2) 
  #[1] 0.99 0.95 0.85 0.62 0.26

  #----------

  # Look at how the confidence level of a nonparametric tolerance interval decreases with the 
  # rank of the lower tolerance limit:

  round(tolIntNparConfLevel(n = 60, ltl.rank = 1:5), 2) 
  #[1] 0.81 0.58 0.35 0.18 0.08

  #==========

  # Example 17-4 on page 17-21 of USEPA (2009) uses copper concentrations (ppb) from 3 
  # background wells to set an upper limit for 2 compliance wells.  There are 6 observations 
  # per well, and the maximum value from the 3 wells is set to the 95% confidence upper 
  # tolerance limit, and we need to determine the coverage of this tolerance interval.  

  tolIntNparCoverage(n = 24, conf.level = 0.95, ti.type = "upper")
  #[1] 0.8826538

  # Here we will modify the example and determine the confidence level of the tolerance 
  # interval when we set the coverage to 95%. 

  tolIntNparConfLevel(n = 24, coverage = 0.95, ti.type = "upper")
  # [1] 0.708011
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }

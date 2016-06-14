\name{ciNparN}
\alias{ciNparN}
\title{
  Sample Size for Nonparametric Confidence Interval for a Quantile
}
\description{
  Compute the sample size necessary to achieve a specified confidence level 
  for a nonparametric confidence interval for a quantile.
}
\usage{
  ciNparN(p = 0.5, lcl.rank = ifelse(ci.type == "upper", 0, 1), 
    n.plus.one.minus.ucl.rank = ifelse(ci.type == "lower", 0, 1), 
    ci.type = "two.sided", conf.level = 0.95)
}
\arguments{
  \item{p}{
  numeric vector of probabilities specifying the quantiles.  
  All values of \code{p} must be between 0 and 
  1.  The default value is \code{p=0.5}.
}
  \item{lcl.rank, n.plus.one.minus.ucl.rank}{
  numeric vectors of non-negative integers indicating the ranks of the 
  order statistics that are used for the lower and upper bounds of the 
  confidence interval for the specified quantile(s).  When \code{lcl.rank=1} 
  that means use the smallest value as the lower bound, when \code{lcl.rank=2} 
  that means use the second to smallest value as the lower bound, etc.  
  When \code{n.plus.one.minus.ucl.rank=1} that means use the largest value 
  as the upper bound, when \code{n.plus.one.minus.ucl.rank=2} that means use 
  the second to largest value as the upper bound, etc.  
  A value of \code{0} for \code{lcl.rank} indicates no lower bound 
  (i.e., -Inf) and a value of 
  \code{0} for \code{n.plus.one.minus.ucl.rank} indicates no upper bound 
  (i.e., \code{Inf}).  When \cr
  \code{ci.type="upper"} then \code{lcl.rank} is set to \code{0} by default, 
  otherwise it is set to \code{1} by default.  
  When \code{ci.type="lower"} then \code{n.plus.one.minus.ucl.rank} is set 
  to \code{0} by default, otherwise it is set to \code{1} by default.
}
  \item{ci.type}{
  character string indicating what kind of confidence interval to compute.  The 
  possible values are \code{"two-sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.
}
  \item{conf.level}{
  numeric vector of numbers between 0 and 1 indicating the confidence level 
  associated with the confidence interval(s).  The default value is 
  \code{conf=0.95}.
}
}
\details{
  If the arguments \code{p}, \code{lcl.rank}, 
  \code{n.plus.one.minus.ucl.rank} and \code{conf.level} are not all the 
  same length, they are replicated to be the 
  same length as the length of the longest argument.

  The help file for \code{\link{eqnpar}} explains how nonparametric confidence 
  intervals for quantiles are constructed and how the confidence level 
  associated with the confidence interval is computed based on specified values 
  for the sample size and the ranks of the order statistics used for 
  the bounds of the confidence interval. 

  The function \code{ciNparN} determines the required the sample size via 
  a nonlinear optimization.
}
\value{
  numeric vector of sample sizes.
}
\references{
  See the help file for \code{\link{eqnpar}}.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help file for \code{\link{eqnpar}}.
}
\seealso{
  \code{\link{eqnpar}}, \code{\link{ciNparConfLevel}}, 
  \code{\link{plotCiNparDesign}}.
}
\examples{
  # Look at how the required sample size for a confidence interval 
  # increases with increasing confidence level for a fixed quantile:

  seq(0.5, 0.9, by = 0.1) 
  #[1] 0.5 0.6 0.7 0.8 0.9 

  ciNparN(p = 0.9, conf.level=seq(0.5, 0.9, by = 0.1)) 
  #[1]  7  9 12 16 22

  #----------

  # Look at how the required sample size for a confidence interval increases 
  # as the quantile moves away from 0.5:

  ciNparN(p = seq(0.5, 0.9, by = 0.1)) 
  #[1]  6  7  9 14 29
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }

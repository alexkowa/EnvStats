\name{tolIntNparN}
\alias{tolIntNparN}
\title{
  Sample Size for Nonparametric Tolerance Interval for Continuous Distribution
}
\description{
  Compute the sample size necessary for a nonparametric tolerance interval (for a continuous 
  distribution) with a specified coverage and, in the case of a \eqn{\beta}-content tolerance 
  interval, a specified confidence level, given the ranks of the order statistics used for the 
  interval.
}
\usage{
  tolIntNparN(coverage = 0.95, conf.level = 0.95, cov.type = "content", 
    ltl.rank = ifelse(ti.type == "upper", 0, 1), 
    n.plus.one.minus.utl.rank = ifelse(ti.type == "lower", 0, 1),  
    ti.type = "two.sided")
}
\arguments{
  \item{coverage}{
  numeric vector of values between 0 and 1 indicating the desired coverage of the 
  tolerance interval.
}
  \item{conf.level}{
  numeric vector of values between 0 and 1 indicating the confidence level of the 
  tolerance interval.
}
  \item{cov.type}{
  character string specifying the coverage type for the tolerance interval.  
  The possible values are \code{"content"} (\eqn{\beta}-content; the default), and 
  \code{"expectation"} (\eqn{\beta}-expectation).
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
  If the arguments \code{coverage}, \code{conf.level}, \code{ltl.rank}, and 
  \code{n.plus.one.minus.utl.rank} are not all the same length, they are replicated to be the 
  same length as the length of the longest argument.

  The help file for \code{\link{tolIntNpar}} explains how nonparametric tolerance intervals 
  are constructed.  

  \emph{Computing Required Sample Size for a \eqn{\beta}-Content Tolerance Interval} (\code{cov.type="content"}) \cr
  For a \eqn{\beta}-content tolerance interval, if the coverage \eqn{C=\beta} is specified, then the 
  associated confidence level \eqn{(1-\alpha)100\%} is computed as:
  \deqn{1 - \alpha = 1 - F(\beta, v-u, w+u) \;\;\;\;\;\; (1)}
  where \eqn{F(y, \delta, \gamma)} denotes the cumulative distribution function of a 
  \link[stats:Beta]{beta random variable} with parameters \code{shape1=}\eqn{\delta} and 
  \code{shape2=}\eqn{\gamma} evaluated at \eqn{y}.  The value of \eqn{1-\alpha} is determined by 
  the argument \code{conf.level}.  The value of \eqn{\beta} is determined by the argument 
  \code{coverage}.  The value of \eqn{u} is determined by the argument \code{ltl.rank}.  The value 
  of \eqn{w} is determined by the argument \cr
  \code{n.plus.one.minus.utl.rank}.  Once these values 
  have been determined, the above equation can be solved implicitly for \eqn{n}, since
  \deqn{v = n + 1 - w \;\;\;\;\;\; (2)}

  \emph{Computing Required Sample Size for a \eqn{\beta}-Expectation Tolerance Interval} (\code{cov.type="expectation"}) \cr
  For a \eqn{\beta}-expectation tolerance interval, the expected coverage is simply the mean of a 
  \link[stats:Beta]{beta random variable} with parameters \code{shape1=}\eqn{v-u} and 
  \code{shape2=}\eqn{w+u}, which is given by:
  \deqn{E(C) = \frac{v-u}{n+1} \;\;\;\;\;\; (3)}
  or, using Equation (2) above, we can re-write the formula for the expected coverage as:
  \deqn{E(C) = \frac{n+1-w-u}{n+1} = 1 - \frac{u+w}{n+1} \;\;\;\;\;\; (4)}
  Thus, for user-specified values of \eqn{u} (\code{ltl.rank}), 
  \eqn{w} (\code{n.plus.one.minus.utl.rank}), and expected coverage, the required sample 
  size is computed as:
  \deqn{n = Ceiling\{ [ \frac{u+w}{1-E(C)} ] - 1 \} \;\;\;\;\;\; (5)}
  where \eqn{Ceiling(x)} denotes the smallest integer greater than or equal to \eqn{x}.  
  (See the \R help file for \code{\link{ceiling}}).
}
\value{
  A vector of positive integers indicating the required sample size(s) for the specified 
  nonparametric tolerance interval(s).
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
  \code{tolIntNparN}, \code{\link{tolIntNparCoverage}}, \code{\link{tolIntNparConfLevel}}, and 
  \code{\link{plotTolIntNparDesign}} can be used to investigate these relationships for 
  constructing nonparametric tolerance intervals.
}
\seealso{
  \code{\link{tolIntNpar}}, \code{\link{tolIntNparConfLevel}}, \code{\link{tolIntNparCoverage}}, 
  \code{\link{plotTolIntNparDesign}}.
}
\examples{
  # Look at how the required sample size for a nonparametric tolerance interval increases 
  # with increasing confidence level:

  seq(0.5, 0.9, by = 0.1) 
  #[1] 0.5 0.6 0.7 0.8 0.9 

  tolIntNparN(conf.level = seq(0.5, 0.9, by = 0.1)) 
  #[1] 34 40 49 59 77

  #----------

  # Look at how the required sample size for a nonparametric tolerance interval increases 
  # with increasing coverage:

  tolIntNparN(coverage = seq(0.5, 0.9, by = 0.1)) 
  #[1]  8 10 14 22 46

  #----------

  # Look at how the required sample size for a nonparametric tolerance interval increases 
  # with the rank of the lower tolerance limit:

  tolIntNparN(ltl.rank = 1:5) 
  #[1]  93 124 153 181 208

  #==========

  # Example 17-4 on page 17-21 of USEPA (2009) uses copper concentrations (ppb) from 3 
  # background wells to set an upper limit for 2 compliance wells.  The maximum value from 
  # the 3 wells is set to the 95% confidence upper tolerance limit, and we need to 
  # determine the coverage of this tolerance interval.  

  tolIntNparCoverage(n = 24, conf.level = 0.95, ti.type = "upper")
  #[1] 0.8826538

  # Here we will modify the example and determine the sample size required to produce 
  # a tolerance interval with 95% confidence level AND 95% coverage. 

  tolIntNparN(coverage = 0.95, conf.level = 0.95, ti.type = "upper")
  #[1] 59
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }
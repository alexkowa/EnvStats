\name{tolIntNpar}
\alias{tolIntNpar}
\title{
  Nonparametric Tolerance Interval for a Continuous Distribution
}
\description{
  Construct a \eqn{\beta}-content or \eqn{\beta}-expectation tolerance interval 
  nonparametrically without making any assumptions about the form of the 
  distribution except that it is continuous.
}
\usage{
  tolIntNpar(x, coverage, conf.level, cov.type = "content", 
    ltl.rank = ifelse(ti.type == "upper", 0, 1), 
    n.plus.one.minus.utl.rank = ifelse(ti.type == "lower", 0, 1), 
    lb = -Inf, ub = Inf, ti.type = "two-sided")
}
\arguments{
  \item{x}{
  numeric vector of observations. Missing (\code{NA}), undefined (\code{NaN}), and 
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{coverage}{
  a scalar between 0 and 1 indicating the desired coverage of the \eqn{\beta}-content 
  tolerance interval.  
  The default value is \code{coverage=0.95}.  If \code{cov.type="content"}, you must 
  supply a value for \code{coverage} or a value for \code{conf.level}, but not both.  
  If \code{cov.type="expectation"}, this argument is ignored.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level associated with the \eqn{\beta}-content 
  tolerance interval.  The default value is \code{conf.level=0.95}.  If \cr
  \code{cov.type="content"}, 
  you must supply a value for \code{coverage} or a value for \code{conf.level}, but not both.  
  If \code{cov.type="expectation"}, this argument is ignored.
}
  \item{cov.type}{
  character string specifying the coverage type for the tolerance interval.  
  The possible values are \code{"content"} (\eqn{\beta}-content; the default), and 
  \code{"expectation"} (\eqn{\beta}-expectation).  See the DETAILS section for more 
  information.
}
  \item{ltl.rank}{
  positive integer indicating the rank of the order statistic to use for the lower bound 
  of the tolerance interval.  If \code{ti.type="two-sided"} or \code{ti.type="lower"}, 
  the default value is \code{ltl.rank=1} (implying the minimum value of \code{x} is used 
  as the lower bound of the tolerance interval).  If \code{ti.type="upper"}, this argument 
  is set equal to \code{0} and the value of \code{lb} is used as the lower bound of the 
  tolerance interval.
}
  \item{n.plus.one.minus.utl.rank}{
  positive integer related to the rank of the order statistic to use for 
  the upper bound of the toleracne interval.  A value of 
  \code{n.plus.one.minus.utl.rank=1} (the default) means use the 
  first largest value of \code{x}, and in general a value of 
  \code{n.plus.one.minus.utl.rank=}\eqn{i} means use the \eqn{i}'th largest value.  If \cr
  \code{ti.type="lower"}, 
  this argument is set equal to \code{0} and the value of \code{ub} is used as the upper 
  bound of the tolerance interval.
}
  \item{lb, ub}{
  scalars indicating lower and upper bounds on the distribution.  By default, \code{lb=-Inf} and 
  \code{ub=Inf}.  If you are constructing a tolerance interval for a distribution 
  that you know has a lower bound other than \code{-Inf} (e.g., \code{0}), set \code{lb} to this 
  value.  Similarly, if you know the distribution has an upper bound other than \code{Inf}, set 
  \code{ub} to this value.  The argument \code{lb} is ignored if \code{ti.type="two-sided"} or 
  \code{ti.type="lower"}.  The argument \code{ub} is ignored if \code{ti.type="two-sided"} or 
  \code{ti.type="upper"}.
}
  \item{ti.type}{
  character string indicating what kind of tolerance interval to compute.  
  The possible values are \code{"two-sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.
}
}
\details{
  A tolerance interval for some population is an interval on the real line constructed so as to 
  contain \eqn{100 \beta \%} of the population (i.e., \eqn{100 \beta \%} of all 
  future observations), where \eqn{0 < \beta < 1}.  The quantity \eqn{100 \beta \%} is called 
  the \emph{coverage}.

  There are two kinds of tolerance intervals (Guttman, 1970):
  \itemize{ 
    \item A \eqn{\beta}-content tolerance interval with confidence level \eqn{100(1-\alpha)\%} is 
    constructed so that it contains at least \eqn{100 \beta \%} of the population (i.e., the 
    coverage is at least \eqn{100 \beta \%}) with probability \eqn{100(1-\alpha)\%}, where 
    \eqn{0 < \alpha < 1}. The quantity \eqn{100(1-\alpha)\%} is called the confidence level or 
    confidence coefficient associated with the tolerance interval.

    \item A \eqn{\beta}-expectation tolerance interval is constructed so that the \emph{average} coverage of 
    the interval is \eqn{100 \beta \%}.
  } 

  \bold{Note:} A \eqn{\beta}-expectation tolerance interval with coverage \eqn{100 \beta \%} is 
  equivalent to a prediction interval for one future observation with associated confidence level 
  \eqn{100 \beta \%}.  Note that there is no explicit confidence level associated with a 
  \eqn{\beta}-expectation tolerance interval.  If a \eqn{\beta}-expectation tolerance interval is 
  treated as a \eqn{\beta}-content tolerance interval, the confidence level associated with this 
  tolerance interval is usually around 50\% (e.g., Guttman, 1970, Table 4.2, p.76).  
  \cr

  \bold{The Form of a Nonparametric Tolerance Interval} \cr
  Let \eqn{\underline{x}} denote a random sample of \eqn{n} independent observations 
  from some continuous distribution and let \eqn{x_{(i)}} denote the \eqn{i}'th order 
  statistic in \eqn{\underline{x}}.  A two-sided nonparametric tolerance interval is 
  constructed as:
  \deqn{[x_{(u)}, x_{(v)}] \;\;\;\;\;\; (1)}
  where \eqn{u} and \eqn{v} are positive integers between \eqn{1} and \eqn{n}, and 
  \eqn{u < v}.  That is, \eqn{u} denotes the rank of the lower tolerance limit, and 
  \eqn{v} denotes the rank of the upper tolerance limit.  To make it easier to write 
  some equations later on, we can also write the tolerance interval (1) in a slightly 
  different way as:
  \deqn{[x_{(u)}, x_{(n+1-w)}] \;\;\;\;\;\; (2)}
  where
  \deqn{w = n + 1 - v \;\;\;\;\;\; (3)}
  so that \eqn{w} is a positive integer between \eqn{1} and \eqn{n-1}, and \eqn{u < n+1-w}.  
  In terms of the arguments to the function \code{tolIntNpar}, the argument 
  \code{ltl.rank} corresponds to \eqn{u}, and the argument \code{n.plus.one.minus.utl.rank} 
  corresponds to \eqn{w}.

  If we allow \eqn{u=0} and \eqn{w=0} and define lower and upper bounds as:
  \deqn{x_{(0)} = lb \;\;\;\;\;\; (4)}
  \deqn{x_{(n+1)} = ub \;\;\;\;\;\; (5)}
  then equation (2) above can also represent a one-sided lower or one-sided upper 
  tolerance interval as well.  That is, a one-sided lower nonparametric tolerance interval 
  is constructed as:
  \deqn{[x_{(u)}, x_{(n+1)}] = [x_{(u)}, ub] \;\;\;\;\;\; (6)}
  and a one-sided upper nonparametric tolerance interval is constructed as:
  \deqn{[x_{(0)}, x_{(v)}] = [lb, x_{(v)}] \;\;\;\;\;\; (7)}
  Usually, \eqn{lb = -\infty} or \eqn{lb = 0} and \eqn{ub = \infty}.

  Let \eqn{C} be a random variable denoting the coverage of the above nonparametric 
  tolerance intervals.  Wilks (1941) showed that the distribution of \eqn{C} follows a 
  \link[stats:Beta]{beta distribution} with parameters \code{shape1=}\eqn{v-u} and 
  \code{shape2=}\eqn{w+u} when the unknown distribution is continuous.
  \cr
 
  \bold{Computations for a \eqn{\beta}-Content Tolerance Interval} \cr
  For a \eqn{\beta}-content tolerance interval, if the coverage \eqn{C = \beta} is specified, 
  then the associated confidence level \eqn{(1-\alpha)100\%} is computed as:
  \deqn{1 - \alpha = 1 - F(\beta, v-u, w+u) \;\;\;\;\;\; (8)}
  where \eqn{F(y, \delta, \gamma)} denotes the cumulative distribution function of a 
  \link[stats:Beta]{beta random variable} with parameters \code{shape1=}\eqn{\delta} and 
  \code{shape2=}\eqn{\gamma} evaluated at \eqn{y}.

  Similarly, if the confidence level associated with the tolerance interval is specified as 
  \eqn{(1-\alpha)100\%}, then the coverage \eqn{C = \beta} is computed as:
  \deqn{\beta = B(\alpha, v-u, w+u) \;\;\;\;\;\; (9)}
  where \eqn{B(p, \delta, \gamma)} denotes the \eqn{p}'th quantile of a 
  \link[stats:Beta]{beta distribution} with parameters \code{shape1=}\eqn{\delta} 
  and \code{shape2=}\eqn{\gamma}. 
  \cr 

  \bold{Computations for a \eqn{\beta}-Expectation Tolerance Interval} \cr
  For a \eqn{\beta}-expectation tolerance interval, the expected coverage is simply 
  the mean of a \link[stats:Beta]{beta random variable} with parameters 
  \code{shape1=}\eqn{v-u} and \code{shape2=}\eqn{w+u}, which is given by:
  \deqn{E(C) = \frac{v-u}{n+1} \;\;\;\;\;\; (10)}
  As stated above, a \eqn{\beta}-expectation tolerance interval with coverage 
  \eqn{\beta 100\%} is equivalent to a prediction interval for one future observation 
  with associated confidence level \eqn{\beta 100\%}.  This is because the probability 
  that any single future observation will fall into this interval is \eqn{\beta 100\%}, 
  so the distribution of the number of \eqn{N} future observations that will fall into 
  this interval is \link[stats:Binomial]{binomial} with parameters \code{size=}\eqn{N} 
  and \code{prob=}\eqn{\beta}.  Hence the expected proportion of future observations 
  that fall into this interval is \eqn{\beta 100\%} and is independent of the value of \eqn{N}. 
  See the help file for \code{\link{predIntNpar}} for more information on constructing 
  a nonparametric prediction interval.
}
\value{
  A list of class \code{"estimate"} containing the estimated parameters, 
  the tolerance interval, and other information.  See \code{\link{estimate.object}} 
  for details.
}
\references{
  Conover, W.J. (1980). \emph{Practical Nonparametric Statistics}. Second Edition. 
  John Wiley and Sons, New York.

  Danziger, L., and S. Davis. (1964).  Tables of Distribution-Free Tolerance Limits.  
  \emph{Annals of Mathematical Statistics} \bold{35}(5), 1361--1365.

  Davis, C.B. (1994).  Environmental Regulatory Statistics. In Patil, G.P., and C.R. Rao, eds., 
  \emph{Handbook of Statistics, Vol. 12: Environmental Statistics}. 
  North-Holland, Amsterdam, a division of Elsevier, New York, NY, Chapter 26, 817--865.

  Davis, C.B., and R.J. McNichols. (1994a).  Ground Water Monitoring Statistics Update: Part I: 
  Progress Since 1988.  \emph{Ground Water Monitoring and Remediation} \bold{14}(4), 148--158.

  Gibbons, R.D. (1991b).  Statistical Tolerance Limits for Ground-Water Monitoring.  
  \emph{Ground Water} \bold{29}, 563--570.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009). 
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.  
  John Wiley & Sons, Hoboken.

  Guttman, I. (1970).  \emph{Statistical Tolerance Regions: Classical and Bayesian}.  
  Hafner Publishing Co., Darien, CT, Chapter 2.

  Hahn, G.J., and W.Q. Meeker. (1991).  \emph{Statistical Intervals: A Guide for Practitioners}.  
  John Wiley and Sons, New York, 392pp.

  Helsel, D.R., and R.M. Hirsch. (1992). \emph{Statistical Methods in Water Resources Research}. 
  Elsevier, New York, NY, pp.88-90.

  Krishnamoorthy K., and T. Mathew. (2009). 
  \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}. 
  John Wiley and Sons, Hoboken.

  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C.

  Wilks, S.S. (1941).  Determination of Sample Sizes for Setting Tolerance Limits.  
  \emph{Annals of Mathematical Statistics} \bold{12}, 91--96.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Tolerance intervals have long been applied to quality control and 
  life testing problems (Hahn, 1970b,c; Hahn and Meeker, 1991; Krishnamoorthy and Mathew, 2009).  
  References that discuss tolerance intervals in the context of environmental monitoring include:  
  Berthouex and Brown (2002, Chapter 21), Gibbons et al. (2009), 
  Millard and Neerchal (2001, Chapter 6), Singh et al. (2010b), and USEPA (2009).
}
\seealso{
  \code{\link{eqnpar}}, \code{\link{estimate.object}}, 
  \code{\link{tolIntNparN}}, \link{Tolerance Intervals}, 
  \link{Estimating Distribution Parameters}, \link{Estimating Distribution Quantiles}.
}
\examples{
  # Generate 20 observations from a lognormal mixture distribution
  # with parameters mean1=1, cv1=0.5, mean2=5, cv2=1, and p.mix=0.1.  
  # The exact two-sided interval that contains 90\% of this distribution is given by: 
  # [0.682312, 13.32052].  Use tolIntNpar to construct a two-sided 90\% 
  # \eqn{\beta}-content tolerance interval.  Note that the associated confidence level 
  # is only 61%.  A larger sample size is required to obtain a larger confidence 
  # level (see the help file for tolIntNparN). 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(23) 
  dat <- rlnormMixAlt(20, 1, 0.5, 5, 1, 0.1) 
  tolIntNpar(dat, coverage = 0.9) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            None
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Tolerance Interval Coverage:     90%
  #
  #Coverage Type:                   content
  #
  #Tolerance Interval Method:       Exact
  #
  #Tolerance Interval Type:         two-sided
  #
  #Confidence Level:                60.8253%
  #
  #Tolerance Limit Rank(s):         1 20 
  #
  #Tolerance Interval:              LTL = 0.5035035
  #                                 UTL = 9.9504662

  #----------

  # Clean up
  rm(dat)

  #----------

  # Reproduce Example 17-4 on page 17-21 of USEPA (2009).  This example uses 
  # copper concentrations (ppb) from 3 background wells to set an upper 
  # limit for 2 compliance wells.  The maximum value from the 3 wells is set 
  # to the 95% confidence upper tolerance limit, and we need to determine the 
  # coverage of this tolerance interval.  The data are stored in EPA.92c.copper2.df.  
  # Note that even though these data are Type I left singly censored, it is still 
  # possible to compute an upper tolerance interval using any of the uncensored 
  # observations as the upper limit. 

  EPA.92c.copper2.df
  #   Copper.orig Copper Censored Month Well  Well.type
  #1           <5    5.0     TRUE     1    1 Background
  #2           <5    5.0     TRUE     2    1 Background
  #3          7.5    7.5    FALSE     3    1 Background
  #...
  #9          9.2    9.2    FALSE     1    2 Background
  #10          <5    5.0     TRUE     2    2 Background
  #11          <5    5.0     TRUE     3    2 Background
  #...
  #17          <5    5.0     TRUE     1    3 Background
  #18         5.4    5.4    FALSE     2    3 Background
  #19         6.7    6.7    FALSE     3    3 Background
  #...
  #29         6.2    6.2    FALSE     5    4 Compliance
  #30          <5    5.0     TRUE     6    4 Compliance
  #31         7.8    7.8    FALSE     7    4 Compliance
  #...
  #38          <5    5.0     TRUE     6    5 Compliance
  #39         5.6    5.6    FALSE     7    5 Compliance
  #40          <5    5.0     TRUE     8    5 Compliance

  with(EPA.92c.copper2.df, 
    tolIntNpar(Copper[Well.type=="Background"], 
      conf.level = 0.95, lb = 0, ti.type = "upper")) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            None
  #
  #Data:                            Copper[Well.type == "Background"]
  #
  #Sample Size:                     24
  #
  #Tolerance Interval Coverage:     88.26538%
  #
  #Coverage Type:                   content
  #
  #Tolerance Interval Method:       Exact
  #
  #Tolerance Interval Type:         upper
  #
  #Confidence Level:                95%
  #
  #Tolerance Limit Rank(s):         24 
  #
  #Tolerance Interval:              LTL = 0.0
  #                                 UTL = 9.2

  #----------

  # Repeat the last example, except compute an upper 
  # \eqn{\beta}-expectation tolerance interval:

  with(EPA.92c.copper2.df, 
    tolIntNpar(Copper[Well.type=="Background"], 
      cov.type = "expectation", lb = 0, ti.type = "upper")) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            None
  #
  #Data:                            Copper[Well.type == "Background"]
  #
  #Sample Size:                     24
  #
  #Tolerance Interval Coverage:     96%
  #
  #Coverage Type:                   expectation
  #
  #Tolerance Interval Method:       Exact
  #
  #Tolerance Interval Type:         upper
  #
  #Tolerance Limit Rank(s):         24 
  #
  #Tolerance Interval:              LTL = 0.0
  #                                 UTL = 9.2
}
\keyword{ distribution }
\keyword{ htest }

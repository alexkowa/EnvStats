\name{predIntNparSimultaneous}
\alias{predIntNparSimultaneous}
\alias{Nonparametric Simultaneous Prediction Interval}
\title{
   Nonparametric Simultaneous Prediction Interval for a Continuous Distribution
}
\description{
  Construct a nonparametric simultaneous prediction interval for the next 
  \eqn{r} sampling \dQuote{occasions} based on one of three 
  possible rules: k-of-m, California, or Modified California.  The simultaneous 
  prediction interval assumes the observations from from a continuous distribution.
}
\usage{
  predIntNparSimultaneous(x, n.median = 1, k = 1, m = 2, r = 1, rule = "k.of.m", 
    lpl.rank = ifelse(pi.type == "upper", 0, 1), 
    n.plus.one.minus.upl.rank = ifelse(pi.type == "lower", 0, 1), 
    lb = -Inf, ub = Inf, pi.type = "upper", integrate.args.list = NULL)
}
\arguments{
  \item{x}{
  a numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), and 
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{n.median}{
  positive odd integer specifying the sample size associated with the future medians.  
  The default value is \code{n.median=1} (i.e., individual observations).  Note that 
  all future medians must be based on the same sample size.
}
  \item{k}{
  for the \eqn{k}-of-\eqn{m} rule (\code{rule="k.of.m"}), a positive integer 
  specifying the minimum number of observations (or medians) out of \eqn{m} 
  observations (or medians) (all obtained on one future sampling \dQuote{occassion}) 
  the prediction interval should contain.  
  The default value is \code{k=1}.  This argument is ignored when the argument 
  \code{rule} is not equal to \code{"k.of.m"}.
}
  \item{m}{
  positive integer specifying the maximum number of future observations (or 
  medians) on one future sampling \dQuote{occasion}.  
  The default value is \code{m=2}, except when \code{rule="Modified.CA"}, in which 
  case this argument is ignored and \code{m} is automatically set equal to \code{4}.
}
  \item{r}{
  positive integer specifying the number of future sampling \dQuote{occasions}.  
  The default value is \code{r=1}.
}
  \item{rule}{
  character string specifying which rule to use.  The possible values are 
  \code{"k.of.m"} (\eqn{k}-of-\eqn{m} rule; the default), \code{"CA"} (California rule), 
  and \code{"Modified.CA"} (modified California rule).  
  See the DETAILS section below for more information.
}
  \item{lpl.rank}{
  positive integer indicating the rank of the order statistic to use for the lower 
  bound of the prediction interval.  When \code{pi.type="lower"}, the default value 
  is \code{lpl.rank=1} (implying the minimum value of \code{x} is used as the lower 
  bound of the prediction interval).  When \code{pi.type="upper"}, the argument 
  \code{lpl.rank} is set equal to \code{0} and the value of \code{lb} is used as the 
  lower bound of the tolerance interval.
}
  \item{n.plus.one.minus.upl.rank}{
  positive integer related to the rank of the order statistic to use for the upper 
  bound of the prediction interval.  A value of \code{n.plus.one.minus.upl.rank=1} 
  (the default) means use the first largest value, and in general a value of \cr
  \code{n.plus.one.minus.upl.rank=}\eqn{i} means use the \eqn{i}'th largest value. 
  When \cr
  \code{pi.type="lower"}, the argument \code{n.plus.one.minus.upl.rank} is set 
  equal to \code{0} and the value of \code{ub} is used as the upper bound of the 
  prediction interval.
}
  \item{lb, ub}{
  scalars indicating lower and upper bounds on the distribution.  By default, 
  \code{lb=-Inf} and \code{ub=Inf}.  If you are constructing a prediction interval for 
  a distribution that you know has a lower bound other than \code{-Inf} 
  (e.g., \code{0}), set \code{lb} to this value.  Similarly, if you know the 
  distribution has an upper bound other than \code{Inf}, set \code{ub} to this value.  
  The argument \code{lb} is ignored if \code{pi.type="two-sided"} or 
  \code{pi.type="lower"}.  The argument \code{ub} is ignored if 
  \code{pi.type="two-sided"} or \code{pi.type="upper"}.
}
  \item{pi.type}{
  character string indicating what kind of prediction interval to compute.  
  The possible values are \code{"upper"} (the default) and \code{"lower"}.
}
  \item{integrate.args.list}{
  a list of arguments to supply to the \code{\link{integrate}} function.  The 
  default value is \code{integrate.args.list=NULL} which means that the 
  default values of \code{\link{integrate}} are used.
}
}
\details{
  \emph{What is a Nonparametric Simultaneous Prediction Interval?} \cr
  A nonparametric prediction interval for some population is an interval on the real line 
  constructed so that it will contain at least \eqn{k} of \eqn{m} future observations from 
  that population with some specified probability \eqn{(1-\alpha)100\%}, where 
  \eqn{0 < \alpha < 1} and \eqn{k} and \eqn{m} are some pre-specified positive integers 
  and \eqn{k \le m}.  The quantity \eqn{(1-\alpha)100\%} is called  
  the confidence coefficient or confidence level associated with the prediction 
  interval.  The function \code{\link{predIntNpar}} computes a standard 
  nonparametric prediction interval.

  The function \code{predIntNparSimultaneous} computes a nonparametric simultaneous 
  prediction interval that will contain a certain number of future observations 
  with probability \eqn{(1-\alpha)100\%} for each of \eqn{r} future sampling 
  \dQuote{occasions}, 
  where \eqn{r} is some pre-specified positive integer.  The quantity \eqn{r} may 
  refer to \eqn{r} distinct future sampling occasions in time, or it may for example 
  refer to sampling at \eqn{r} distinct locations on one future sampling occasion, 
  assuming that the population standard deviation is the same at all of the \eqn{r} 
  distinct locations.

  The function \code{predIntNparSimultaneous} computes a nonparametric simultaneous  
  prediction interval based on one of three possible rules:
  \itemize{
  \item For the \eqn{k}-of-\eqn{m} rule (\code{rule="k.of.m"}), at least \eqn{k} of 
    the next \eqn{m} future observations will fall in the prediction 
    interval with probability \eqn{(1-\alpha)100\%} on each of the \eqn{r} future 
    sampling occasions.  If obserations are being taken sequentially, for a particular 
    sampling occasion, up to \eqn{m} observations may be taken, but once 
    \eqn{k} of the observations fall within the prediction interval, sampling can stop.  
    Note:  For this rule, when \eqn{r=1}, the results of \code{predIntNparSimultaneous} 
    are equivalent to the results of \code{\link{predIntNpar}}.

  \item For the California rule (\code{rule="CA"}), with probability 
    \eqn{(1-\alpha)100\%}, for each of the \eqn{r} future sampling occasions, either 
    the first observation will fall in the prediction interval, or else all of the next 
    \eqn{m-1} observations will fall in the prediction interval. That is, if the first 
    observation falls in the prediction interval then sampling can stop.  Otherwise, 
    \eqn{m-1} more observations must be taken.

  \item For the Modified California rule (\code{rule="Modified.CA"}), with probability 
    \eqn{(1-\alpha)100\%}, for each of the \eqn{r} future sampling occasions, either the 
    first observation will fall in the prediction interval, or else at least 2 out of 
    the next 3 observations will fall in the prediction interval.  That is, if the first 
    observation falls in the prediction interval then sampling can stop.  Otherwise, up 
    to 3 more observations must be taken.
  }

  Nonparametric simultaneous prediction intervals can be extended to using medians 
  in place of single observations (USEPA, 2009, Chapter 19).  That is, you can 
  create a nonparametric simultaneous prediction interval that will contain a 
  specified number of medians (based on which rule you choose) on each of \eqn{r} 
  future sampling occassions, where each each median is based on \eqn{b} individual 
  observations.  For the function \code{predIntNparSimultaneous}, the argument 
  \code{n.median} corresponds to \eqn{b}.
  \cr
 
  \bold{The Form of a Nonparametric Prediction Interval} \cr
  Let \eqn{\underline{x} = x_1, x_2, \ldots, x_n} denote a vector of \eqn{n} 
  independent observations from some continuous distribution, and let 
  \eqn{x_{(i)}} denote the the \eqn{i}'th order statistics in \eqn{\underline{x}}.  
  A two-sided nonparametric prediction interval is constructed as:
  \deqn{[x_{(u)}, x_{(v)}] \;\;\;\;\;\; (1)}
  where \eqn{u} and \eqn{v} are positive integers between 1 and \eqn{n}, and 
  \eqn{u < v}.  That is, \eqn{u} denotes the rank of the lower prediction limit, and 
  \eqn{v} denotes the rank of the upper prediction limit.  To make it easier to write 
  some equations later on, we can also write the prediction interval (1) in a slightly 
  different way as:
  \deqn{[x_{(u)}, x_{(n + 1 - w)}] \;\;\;\;\;\; (2)}
  where
  \deqn{w = n + 1 - v \;\;\;\;\;\; (3)}
  so that \eqn{w} is a positive integer between 1 and \eqn{n-1}, and 
  \eqn{u < n+1-w}.  In terms of the arguments to the function \code{predIntNparSimultaneous}, 
  the argument \code{lpl.rank} corresponds to \eqn{u}, and the argument 
  \code{n.plus.one.minus.upl.rank} corresponds to \eqn{w}.

  If we allow \eqn{u=0} and \eqn{w=0} and define lower and upper bounds as:
  \deqn{x_{(0)} = lb \;\;\;\;\;\; (4)}
  \deqn{x_{(n+1)} = ub \;\;\;\;\;\; (5)}
  then Equation (2) above can also represent a one-sided lower or one-sided upper 
  prediction interval as well.  That is, a one-sided lower nonparametric prediction 
  interval is constructed as:
  \deqn{[x_{(u)}, x_{(n + 1)}] =  [x_{(u)}, ub] \;\;\;\;\;\; (6)}
  and a one-sided upper nonparametric prediction interval is constructed as:
  \deqn{[x_{(0)}, x_{(n + 1 - w)}]  = [lb, x_{(n + 1 - w)}] \;\;\;\;\;\; (7)}
  Usually, \eqn{lb = -\infty} or \eqn{lb = 0} and \eqn{ub = \infty}.

  \bold{Note:}  For nonparametric simultaneous prediction intervals, only lower 
  (\code{pi.type="lower"}) and upper (\code{pi.type="upper"}) prediction 
  intervals are available.
  \cr  

  \bold{Constructing Nonparametric Simultaneous Prediction Intervals for Future Observations} \cr
  First we will show how to construct a nonparametric simultaneous prediction interval based on 
  future observations (i.e., \eqn{b=1}, \code{n.median=1}), and then extend the formulas to 
  future medians.
  \cr  

  \emph{Simultaneous Prediction Intervals for the k-of-m Rule} (\code{rule="k.of.m"}) \cr
  For the \eqn{k}-of-\eqn{m} rule (\code{rule="k.of.m"}) with \eqn{w=1} 
  (i.e., \code{n.median=1}), at least \eqn{k} of the next \eqn{m} future 
  observations will fall in the prediction interval 
  with probability \eqn{(1-\alpha)100\%} on each of the \eqn{r} future sampling 
  occasions.  If observations are being taken sequentially, for a particular 
  sampling occasion, up to \eqn{m} observations may be taken, but once \eqn{k} 
  of the observations fall within the prediction interval, sampling can stop.  
  Note: When \eqn{r=1}, this kind of simultaneous prediction 
  interval becomes the same as a standard nonparametric prediction interval 
  (see \code{\link{predIntNpar}}).

  Chou and Owen (1986) developed the theory for nonparametric simultaneous prediction limits 
  for various rules, including the 1-of-\eqn{m} rule.  Their theory, however, does not cover 
  the California or Modified California rules, and uses an \eqn{r}-fold summation involving a 
  minimum of \eqn{2^r} terms.  Davis and McNichols (1994b; 1999) extended the results of 
  Chou and Owen (1986) to include the California and Modified California rule, and developed 
  algorithms that involve summing far fewer terms.

  Davis and McNichols (1999) give formulas for the probabilities associated with the 
  one-sided upper simultaneous prediction interval shown in Equation (7).  For the 
  \eqn{k}-of-\eqn{m} rule, the probability that at least \eqn{k} of the next \eqn{m} 
  future observations will be contained in the interval given in Equation (7) for each 
  of \eqn{r} future sampling occasions is given by:
  \tabular{lll}{
  \eqn{1 - \alpha} \tab \eqn{=} \tab \eqn{E[\sum_{i=0}^{m-k} {{k-1+i} \choose {k-1}} Y^k (1-Y)^i]^r} \cr
                   \tab \eqn{=} \tab \eqn{\int_0^1 [\sum_{i=0}^{m-k} {{k-1+i} \choose {k-1}} y^k (1-y)^i]^r f(y) dy \;\;\;\;\;\; (8)} \cr
  }
  where \eqn{Y} denotes a random variable with a \link[stats:Beta]{beta distribution} 
  with parameters \eqn{v} and \eqn{n+1-v}, and \eqn{f()} denotes the pdf of this 
  distribution.  Note that \eqn{v} denotes the rank of the order statistic used as the 
  upper prediction limit (i.e., \code{n.plus.one.minus.upl.rank=}\eqn{n+1-v}), and 
  that \eqn{v} is usually equal to \eqn{n}.

  Also note that the summation term in Equation (8) corresponds to the cumulative 
  distribution function of a \link[stats:NegBinomial]{Negative Binomial distribution} 
  with parameters \code{size=}\eqn{k} and \code{prob=}\eqn{y} evaluated at 
  \code{q=}\eqn{m-k}.

  When \code{pi.type="lower"}, \eqn{Y} denotes a random variable with a 
  \link[stats:Beta]{beta distribution} with parameters \eqn{n+1-u} and 
  \eqn{u}.  Note that \eqn{u} denotes the rank of the order statistic used as the 
  lower prediction limit (i.e., \code{lpl.rank=}\eqn{u}), and 
  that \eqn{u} is usually equal to \eqn{1}.
  \cr

  \emph{Simultaneous Prediction Intervals for the California Rule} (\code{rule="CA"}) \cr
  For the California rule (\code{rule="CA"}), with probability \eqn{(1-\alpha)100\%}, 
  for each of the \eqn{r} future sampling occasions, either the first observation will 
  fall in the prediction interval, or else all of the next \eqn{m-1} observations will 
  fall in the prediction interval.  That is, if the first observation falls in the 
  prediction interval then sampling can stop.  Otherwise, \eqn{m-1} more observations 
  must be taken.

  In this case, the probability is given by:
  \tabular{lll}{
  \eqn{1 - \alpha} \tab \eqn{=} \tab \eqn{E[\sum_{i=0}^r {r \choose i} Y^{r - i + (m-1)i} (1-Y)^i]} \cr
                   \tab \eqn{=} \tab \eqn{\int_0^1  [\sum_{i=0}^r {r \choose i} y^{r - i + (m-1)i} (1-y)^i] f(y) dy \;\;\;\;\;\; (9)} \cr
  }


  \emph{Simultaneous Prediction Intervals for the Modified California Rule} (\code{rule="Modified.CA"}) \cr
  For the Modified California rule (\code{rule="Modified.CA"}), with probability 
  \eqn{(1-\alpha)100\%}, for each of the \eqn{r} future sampling occasions, either the 
  first observation will fall in the prediction interval, or else at least 2 out of 
  the next 3 observations will fall in the prediction interval.  That is, if the first 
  observation falls in the prediction interval then sampling can stop.  Otherwise, up 
  to 3 more observations must be taken.

  In this case, the probability is given by:
  \tabular{lll}{
  \eqn{1 - \alpha} \tab \eqn{=} \tab \eqn{E[Y^r (1 + Q + Q^2 - 2Q^3)^r]} \cr
                   \tab \eqn{=} \tab \eqn{\int_0^1 [y^r (1 + q + q^2 - 2q^3)^r] f(y) dy \;\;\;\;\;\; (10)} \cr
  }
  where \eqn{Q = 1 - Y} and \eqn{q = 1 - y}.

  Davis and McNichols (1999) provide algorithms for computing the probabilities based on expanding 
  polynomials and the formula for the expected value of a beta random variable.  In the discussion 
  section of Davis and McNichols (1999), however, Vangel points out that numerical integration is 
  adequate, and this is how these probabilities are computed in the function 
  \code{predIntNparSimultaneous}.
  \cr

  \bold{Constructing Nonparametric Simultaneous Prediction Intervals for Future Medians} \cr
  USEPA (2009, Chapter 19; Cameron, 2011) extends nonparametric simultaneous 
  prediction intervals to testing future medians for the case of the 1-of-1 and 
  1-of-2 plans for medians of order 3.  In general, each of the rules 
  (\eqn{k}-of-\eqn{m}, California, and Modified California) can be easily 
  extended to the case of using medians as long as the medians are based on an 
  odd (as opposed to even) sample size.  

  For each of the above rules, if we are interested in using medians instead of 
  single observations (i.e., \eqn{b \ge 1}; \code{n.median}\eqn{\ge 1}), and we 
  force \eqn{b} to be odd, then a median will be less than a prediction limit 
  once \eqn{(b+1)/2} observations are less than the prediction limit.  Thus, 
  Equations (8) - (10) are modified by replacing \eqn{y} with the term:
  \deqn{\sum_{i=0}^{b - b'} {{b' - 1 + i} \choose {b' - 1}} y^{b'} (1 - y)^i \;\;\;\;\;\; (11)}
  where
  \deqn{b' = \frac{b+1}{2} \;\;\;\;\;\; (12)}
}
\value{
  a list of class \code{"estimate"} containing the simultaneous prediction interval 
  and other information.  See the help file for \code{\link{estimate.object}} for 
  details.
}
\references{
  Cameron, Kirk. (2011).  Personal communication, February 16, 2011.  
    MacStat Consulting, Ltd., Colorado Springs, Colorado.

  Chew, V. (1968).  Simultaneous Prediction Intervals.  \emph{Technometrics} 
  \bold{10}(2), 323--331.

  Danziger, L., and S. Davis. (1964).  Tables of Distribution-Free Tolerance Limits.  
  \emph{Annals of Mathematical Statistics} \bold{35}(5), 1361--1365.

  Davis, C.B. (1994).  Environmental Regulatory Statistics.  In Patil, G.P., 
  and C.R. Rao, eds., \emph{Handbook of Statistics, Vol. 12: Environmental Statistics}.  
  North-Holland, Amsterdam, a division of Elsevier, New York, NY, Chapter 26, 817--865.

  Davis, C.B., and R.J. McNichols. (1987).  One-sided Intervals for at Least p of m 
  Observations from a Normal Population on Each of r Future Occasions.  
  \emph{Technometrics} \bold{29}, 359--370.

  Davis, C.B., and R.J. McNichols. (1994a).  Ground Water Monitoring Statistics Update:  
  Part I: Progress Since 1988.  \emph{Ground Water Monitoring and Remediation} 
  \bold{14}(4), 148--158.

  Davis, C.B., and R.J. McNichols. (1994b).  Ground Water Monitoring Statistics Update:  
  Part II:  Nonparametric Prediction Limits.  
  \emph{Ground Water Monitoring and Remediation} \bold{14}(4), 159--175.

  Davis, C.B., and R.J. McNichols. (1999).  Simultaneous Nonparametric Prediction 
  Limits (with Discusson).  \emph{Technometrics} \bold{41}(2), 89--112.

  Gibbons, R.D. (1987a).  Statistical Prediction Intervals for the Evaluation of 
  Ground-Water Quality.  \emph{Ground Water} \bold{25}, 455--465.

  Gibbons, R.D. (1991b).  Statistical Tolerance Limits for Ground-Water Monitoring.  
  \emph{Ground Water} \bold{29}, 563--570.

  Gibbons, R.D., and J. Baker. (1991).  The Properties of Various Statistical 
  Prediction Intervals for Ground-Water Detection Monitoring.  
  \emph{Journal of Environmental Science and Health} \bold{A26}(4), 535--553.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009). 
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.  
  John Wiley & Sons, Hoboken.

  Hahn, G.J., and W.Q. Meeker. (1991).  
  \emph{Statistical Intervals: A Guide for Practitioners}.  John Wiley and Sons, 
  New York, 392pp.

  Hahn, G., and W. Nelson. (1973).  A Survey of Prediction Intervals and Their 
  Applications.  \emph{Journal of Quality Technology} \bold{5}, 178--188.

  Hall, I.J., R.R. Prairie, and C.K. Motlagh. (1975).  Non-Parametric Prediction 
  Intervals.  \emph{Journal of Quality Technology} \bold{7}(3), 109--114.

  Millard, S.P., and Neerchal, N.K. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton, Florida.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Prediction and tolerance intervals have long been applied to quality control and 
  life testing problems (Hahn, 1970b,c; Hahn and Nelson, 1973; 
  Krishnamoorthy and Mathew, 2009).  
  In the context of environmental statistics, prediction intervals are useful for 
  analyzing data from groundwater detection monitoring programs at hazardous and 
  solid waste facilities (e.g., Gibbons et al., 2009; Millard and Neerchal, 2001; 
  USEPA, 2009).
}
\seealso{
  \code{\link{predIntNparSimultaneousConfLevel}}, 
  \code{\link{predIntNparSimultaneousN}}, \cr
  \code{\link{plotPredIntNparSimultaneousDesign}},
  \code{\link{predIntNparSimultaneousTestPower}}, 
  \code{\link{predIntNpar}}, \code{\link{tolIntNpar}}, 
  \code{\link{estimate.object}}.
}
\examples{
  # Generate 20 observations from a lognormal mixture distribution with 
  # parameters mean1=1, cv1=0.5, mean2=5, cv2=1, and p.mix=0.1.  Use 
  # predIntNparSimultaneous to construct an upper one-sided prediction interval 
  # using the maximum observed value using the 1-of-3 rule.  
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rlnormMixAlt(n = 20, mean1 = 1, cv1 = 0.5, 
    mean2 = 5, cv2 = 1, p.mix = 0.1) 

  predIntNparSimultaneous(dat, k = 1, m = 3, lb = 0) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            None
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Prediction Interval Method:      exact 
  #
  #Prediction Interval Type:        upper
  #
  #Confidence Level:                99.94353%
  #
  #Prediction Limit Rank(s):        20 
  #
  #Minimum Number of
  #Future Observations
  #Interval Should Contain:         1
  #
  #Total Number of
  #Future Observations:             3
  #
  #Prediction Interval:             LPL = 0.000000
  #                                 UPL = 1.817311

  #----------

  # Compare the confidence levels for the 1-of-3 rule, California Rule, and 
  # Modified California Rule.

  predIntNparSimultaneous(dat, k = 1, m = 3, lb = 0)$interval$conf.level
  #[1] 0.9994353

  predIntNparSimultaneous(dat, m = 3, rule = "CA", lb = 0)$interval$conf.level
  #[1] 0.9919066

  predIntNparSimultaneous(dat, rule = "Modified.CA", lb = 0)$interval$conf.level
  #[1] 0.9984943

  #=========

  # Repeat the above example, but create the baseline data using just 
  # n=8 observations and set r to 4 future sampling occasions

  set.seed(598) 
  dat <- rlnormMixAlt(n = 8, mean1 = 1, cv1 = 0.5, 
    mean2 = 5, cv2 = 1, p.mix = 0.1) 

  predIntNparSimultaneous(dat, k = 1, m = 3, r = 4, lb = 0) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            None
  #
  #Data:                            dat
  #
  #Sample Size:                     8
  #
  #Prediction Interval Method:      exact 
  #
  #Prediction Interval Type:        upper
  #
  #Confidence Level:                97.7599%
  #
  #Prediction Limit Rank(s):        8 
  #
  #Minimum Number of
  #Future Observations
  #Interval Should Contain
  #(per Sampling Occasion):         1
  #
  #Total Number of
  #Future Observations
  #(per Sampling Occasion):         3
  #
  #Number of Future
  #Sampling Occasions:              4
  #
  #Prediction Interval:             LPL = 0.000000
  #                                 UPL = 5.683453

  #----------

  # Compare the confidence levels for the 1-of-3 rule, California Rule, and 
  # Modified California Rule.

  predIntNparSimultaneous(dat, k = 1, m = 3, r = 4, lb = 0)$interval$conf.level
  #[1] 0.977599

  predIntNparSimultaneous(dat, m = 3, r = 4, rule = "CA", lb = 0)$interval$conf.level
  #[1] 0.8737798

  predIntNparSimultaneous(dat, r = 4, rule = "Modified.CA", lb = 0)$interval$conf.level
  #[1] 0.9510178

  #==========

  # Example 19-5 of USEPA (2009, p. 19-33) shows how to compute nonparametric upper 
  # simultaneous prediction limits for various rules based on trace mercury data (ppb) 
  # collected in the past year from a site with four background wells and 10 compliance 
  # wells (data for two of the compliance wells  are shown in the guidance document).  
  # The facility must monitor the 10 compliance wells for five constituents 
  # (including mercury) annually.
  
  # Here we will compute the confidence level associated with two different sampling plans: 
  # 1) the 1-of-2 retesting plan for a median of order 3 using the background maximum and 
  # 2) the 1-of-4 plan on individual observations using the 3rd highest background value.
  # The data for this example are stored in EPA.09.Ex.19.5.mercury.df.

  # We will pool data from 4 background wells that were sampled on 
  # a number of different occasions, giving us a sample size of 
  # n = 20 to use to construct the prediction limit.

  # There are 10 compliance wells and we will monitor 5 different 
  # constituents at each well annually.  For this example, USEPA (2009) 
  # recommends setting r to the product of the number of compliance wells and 
  # the number of evaluations per year.  

  # To determine the minimum confidence level we require for 
  # the simultaneous prediction interval, USEPA (2009) recommends 
  # setting the maximum allowed individual Type I Error level per constituent to:
 
  # 1 - (1 - SWFPR)^(1 / Number of Constituents)
  
  # which translates to setting the confidence limit to 

  # (1 - SWFPR)^(1 / Number of Constituents)

  # where SWFPR = site-wide false positive rate.  For this example, we 
  # will set SWFPR = 0.1.  Thus, the required individual Type I Error level 
  # and confidence level per constituent are given as follows:

  # n  = 20 based on 4 Background Wells
  # nw = 10 Compliance Wells
  # nc =  5 Constituents
  # ne =  1 Evaluation per year

  n  <- 20
  nw <- 10
  nc <-  5
  ne <-  1

  # Set number of future sampling occasions r to 
  # Number Compliance Wells x Number Evaluations per Year
  r  <-  nw * ne

  conf.level <- (1 - 0.1)^(1 / nc)
  conf.level
  #[1] 0.9791484

  alpha <- 1 - conf.level
  alpha
  #[1] 0.02085164

  #----------

  # Look at the data:

  head(EPA.09.Ex.19.5.mercury.df)
  #  Event Well  Well.type Mercury.ppb.orig Mercury.ppb Censored
  #1     1 BG-1 Background             0.21        0.21    FALSE
  #2     2 BG-1 Background              <.2        0.20     TRUE
  #3     3 BG-1 Background              <.2        0.20     TRUE
  #4     4 BG-1 Background              <.2        0.20     TRUE
  #5     5 BG-1 Background              <.2        0.20     TRUE
  #6     6 BG-1 Background                           NA    FALSE

  longToWide(EPA.09.Ex.19.5.mercury.df, "Mercury.ppb.orig", 
    "Event", "Well", paste.row.name = TRUE)
  #        BG-1 BG-2 BG-3 BG-4 CW-1 CW-2
  #Event.1 0.21  <.2  <.2  <.2 0.22 0.36
  #Event.2  <.2  <.2 0.23 0.25  0.2 0.41
  #Event.3  <.2  <.2  <.2 0.28  <.2 0.28
  #Event.4  <.2 0.21 0.23  <.2 0.25 0.45
  #Event.5  <.2  <.2 0.24  <.2 0.24 0.43
  #Event.6                      <.2 0.54


  # Construct the upper simultaneous prediction limit using the 1-of-2  
  # retesting plan for a median of order 3 based on the background maximum 

  Hg.Back <- with(EPA.09.Ex.19.5.mercury.df, 
    Mercury.ppb[Well.type == "Background"])

  pred.int.1.of.2.med.3 <- predIntNparSimultaneous(Hg.Back, n.median = 3, 
    k = 1, m = 2, r = r, lb = 0)

  pred.int.1.of.2.med.3

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            None
  #
  #Data:                            Hg.Back
  #
  #Sample Size:                     20
  #
  #Number NA/NaN/Inf's:             4
  #
  #Prediction Interval Method:      exact 
  #
  #Prediction Interval Type:        upper
  #
  #Confidence Level:                99.40354%
  #
  #Prediction Limit Rank(s):        20 
  #
  #Minimum Number of
  #Future Medians
  #Interval Should Contain
  #(per Sampling Occasion):         1
  #
  #Total Number of
  #Future Medians
  #(per Sampling Occasion):         2
  #
  #Number of Future
  #Sampling Occasions:              10
  #
  #Sample Size for Medians:         3
  #
  #Prediction Interval:             LPL = 0.00
  #                                 UPL = 0.28

  # Note that the achieved confidence level of 99.4% is greater than the 
  # required confidence level of 97.9%.

  # Now determine whether either compliance well indicates evidence of 
  # Mercury contamination.

  # Compliance Well 1
  #------------------
  Hg.CW.1 <- with(EPA.09.Ex.19.5.mercury.df, Mercury.ppb.orig[Well == "CW-1"])

  Hg.CW.1
  #[1] "0.22" "0.2"  "<.2"  "0.25" "0.24" "<.2"

  # The median of the first 3 observations is 0.2, which is less than 
  # the UPL of 0.28, so there is no evidence of contamination.

  # Compliance Well 2
  #------------------
  Hg.CW.2 <- with(EPA.09.Ex.19.5.mercury.df, Mercury.ppb.orig[Well == "CW-2"])

  Hg.CW.2
  #[1] "0.36" "0.41" "0.28" "0.45" "0.43" "0.54"

  # The median of the first 3 observations is 0.36, so 3 more observations have to 
  # be looked at.  The median of the second 3 observations is 0.45, which is 
  # larger than the UPL of 0.28, so there is evidence of contamination.  

  #----------

  # Now create the upper simultaneous prediction limit using the 1-of-4 plan 
  # on individual observations using the 3rd highest background value.

  pred.int.1.of.4.3rd <- predIntNparSimultaneous(Hg.Back, k = 1, m = 4, 
    r = r, lb = 0, n.plus.one.minus.upl.rank = 3)

  pred.int.1.of.4.3rd  

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            None
  #
  #Data:                            Hg.Back
  #
  #Sample Size:                     20
  #
  #Number NA/NaN/Inf's:             4
  #
  #Prediction Interval Method:      exact 
  #
  #Prediction Interval Type:        upper
  #
  #Confidence Level:                98.64909%
  #
  #Prediction Limit Rank(s):        18 
  #
  #Minimum Number of
  #Future Observations
  #Interval Should Contain
  #(per Sampling Occasion):         1
  #
  #Total Number of
  #Future Observations
  #(per Sampling Occasion):         4
  #
  #Number of Future
  #Sampling Occasions:              10
  #
  #Prediction Interval:             LPL = 0.00
  #                                 UPL = 0.24

  # Note that the achieved confidence level of 98.6% is greater than the 
  # required confidence level of 97.9%.


  # Now determine whether either compliance well indicates evidence of 
  # Mercury contamination.

  # Compliance Well 1
  #------------------
  Hg.CW.1 <- with(EPA.09.Ex.19.5.mercury.df, Mercury.ppb.orig[Well == "CW-1"])

  Hg.CW.1
  #[1] "0.22" "0.2"  "<.2"  "0.25" "0.24" "<.2"

  # The first observation is less than the UPL of 0.24, which is less than 
  # the UPL of 0.28, so there is no evidence of contamination.


  # Compliance Well 2
  #------------------
  Hg.CW.2 <- with(EPA.09.Ex.19.5.mercury.df, Mercury.ppb.orig[Well == "CW-2"])

  Hg.CW.2
  #[1] "0.36" "0.41" "0.28" "0.45" "0.43" "0.54"

  # All of the first 4 observations are greater than the UPL of 0.24, so there 
  # is evidence of contamination.  

   #==========

  # Cleanup
  #--------
  rm(dat, n, nw, nc, ne, r, conf.level, alpha, Hg.Back, pred.int.1.of.2.med.3, 
    pred.int.1.of.4.3rd, Hg.CW.1, Hg.CW.2)
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }

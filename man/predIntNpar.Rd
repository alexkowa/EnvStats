\name{predIntNpar}
\alias{predIntNpar}
\alias{Nonparametric Prediction Interval}
\title{
   Nonparametric Prediction Interval for a Continuous Distribution
}
\description{
  Construct a nonparametric prediction interval to contain at least \eqn{k} out of the 
  next \eqn{m} future observations with probability \eqn{(1-\alpha)100\%} for a 
  continuous distribution.
}
\usage{
  predIntNpar(x, k = m, m = 1,  lpl.rank = ifelse(pi.type == "upper", 0, 1), 
    n.plus.one.minus.upl.rank = ifelse(pi.type == "lower", 0, 1), 
    lb = -Inf, ub = Inf, pi.type = "two-sided")
}
\arguments{
  \item{x}{
  a numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), and 
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{k}{
  positive integer specifying the minimum number of future observations out of \code{m} 
  that should be contained in the prediction interval. The default value is \code{k=m}.
}
  \item{m}{
  positive integer specifying the number of future observations.  The default value is 
  \code{m=1}.
}
  \item{lpl.rank}{
  positive integer indicating the rank of the order statistic to use for the lower 
  bound of the prediction interval.  If \code{pi.type="two-sided"} or 
  \code{pi.type="lower"}, the default value is \code{lpl.rank=1} (implying the 
  minimum value of \code{x} is used as the lower bound of the prediction interval).  
  If \code{pi.type="upper"}, this argument is set equal to \code{0} and the value of 
  \code{lb} is used as the lower bound of the tolerance interval.
}
  \item{n.plus.one.minus.upl.rank}{
  positive integer related to the rank of the order statistic to use for the upper 
  bound of the prediction interval.  A value of \code{n.plus.one.minus.upl.rank=1} 
  (the default when \code{pi.type="two.sided"} or \code{pi.type="upper"}) means use 
  the first largest value, and in general a value of 
  \code{n.plus.one.minus.upl.rank=}\eqn{i} means use the \eqn{i}'th largest value.  
  If \code{pi.type="lower"}, this argument is set equal to \code{0} and the value of 
  \code{ub} is used as the upper bound of the prediction interval.
}
  \item{lb, ub}{
  scalars indicating lower and upper bounds on the distribution.  By default, \cr
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
  The possible values are \code{"two-sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.
}
}
\details{
  \bold{What is a Nonparametric Prediction Interval?} \cr
  A nonparametric prediction interval for some population is an interval on the 
  real line constructed so that it will contain at least \eqn{k} of \eqn{m} future 
  observations from that population with some specified probability 
  \eqn{(1-\alpha)100\%}, where \eqn{0 < \alpha < 1} and \eqn{k} and \eqn{m} are 
  pre-specified positive integer where \eqn{k \le m}.  
  The quantity \eqn{(1-\alpha)100\%} is called  
  the confidence coefficient or confidence level associated with the prediction 
  interval.

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
  \eqn{u < n+1-w}.  In terms of the arguments to the function \code{predIntNpar}, 
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

  \bold{Constructing Nonparametric Prediction Intervals for Future Observations} \cr
  Danziger and Davis (1964) show that the probability that at least \eqn{k} out of 
  the next \eqn{m} observations will fall in the interval defined in Equation (2) 
  is given by:
  \deqn{(1 - \alpha) = [\sum_{i=k}^m {{m-i+u+w-1} \choose {m-i}} {{i+n-u-w} \choose i}] / {{n+m} \choose m} \;\;\;\;\;\; (8)}

  (Note that computing a nonparametric prediction interval for the case 
  \eqn{k = m = 1} is equivalent to computing a nonparametric \eqn{\beta}-expectation 
  tolerance interval with coverage \eqn{(1-\alpha)100\%}; see \code{\link{tolIntNpar}}).
  \cr

  \emph{The Special Case of Using the Minimum and the Maximum} \cr
  Setting \eqn{u = w = 1} implies using the smallest and largest observed values as 
  the prediction limits.  In this case, it can be shown that the probability that at 
  least \eqn{k} out of the next \eqn{m} observations will fall in the interval
  \deqn{[x_{(1)}, x_{(n)}] \;\;\;\;\;\; (9)}
  is given by:
  \deqn{(1 - \alpha) = [\sum_{i=k}^m (m-i-1){{n+i-2} \choose i}] / {{n+m} \choose m} \;\;\;\;\;\; (10)}

  Setting \eqn{k=m} in Equation (10), the probability that all of the next \eqn{m} 
  observations will fall in the interval defined in Equation (9) is given by:
  \deqn{(1 - \alpha) = \frac{n(n-1)}{(n+m)(n+m-1)} \;\;\;\;\;\; (11)}

  For one-sided prediction limits, the probability that all \eqn{m} future 
  observations will fall below \eqn{x_{(n)}} (upper prediction limit; 
  \code{pi.type="upper"}) and the probabilitiy that all \eqn{m} future observations 
  will fall above \eqn{x_{(1)}} (lower prediction limit; \code{pi.type="lower"}) are 
  both given by:
  \deqn{(1 - \alpha) = \frac{n}{n+m} \;\;\;\;\;\; (12)}
  \cr

  \bold{Constructing Nonparametric Prediction Intervals for Future Medians} \cr
  To construct a nonparametric prediction interval for a future median based on 
  \eqn{s} future observations, where \eqn{s} is odd, note that this is equivalent to 
  constructing a nonparametric prediction interval that must hold 
  at least \eqn{k = (s+1)/2} of the next \eqn{m = s} future observations.
  
}
\value{
  a list of class \code{"estimate"} containing the prediction interval and other 
  information.  See the help file for \code{\link{estimate.object}} for details.
}
\references{
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
  \code{\link{estimate.object}}, \code{\link{predIntNparN}}, 
  \code{\link{predIntNparConfLevel}}, \code{\link{plotPredIntNparDesign}}.
}
\examples{
  # Generate 20 observations from a lognormal mixture distribution with 
  # parameters mean1=1, cv1=0.5, mean2=5, cv2=1, and p.mix=0.1.  Use 
  # predIntNpar to construct a two-sided prediction interval using the 
  # minimum and maximum observed values.  Note that the associated confidence 
  # level is 90%.  A larger sample size is required to obtain a larger 
  # confidence level (see the help file for predIntNparN). 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rlnormMixAlt(n = 20, mean1 = 1, cv1 = 0.5, 
    mean2 = 5, cv2 = 1, p.mix = 0.1) 

  predIntNpar(dat) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            None
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Prediction Interval Method:      Exact
  #
  #Prediction Interval Type:        two-sided
  #
  #Confidence Level:                90.47619%
  #
  #Prediction Limit Rank(s):        1 20 
  #
  #Number of Future Observations:   1
  #
  #Prediction Interval:             LPL = 0.3647875
  #                                 UPL = 1.8173115

  #----------

  # Repeat the above example, but specify m=5 future observations should be 
  # contained in the prediction interval.  Note that the confidence level is 
  # now only 63%.

  predIntNpar(dat, m = 5) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            None
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Prediction Interval Method:      Exact
  #
  #Prediction Interval Type:        two-sided
  #
  #Confidence Level:                63.33333%
  #
  #Prediction Limit Rank(s):        1 20 
  #
  #Number of Future Observations:   5
  #
  #Prediction Interval:             LPL = 0.3647875
  #                                 UPL = 1.8173115

  #----------

  # Repeat the above example, but specify that a minimum of k=3 observations 
  # out of a total of m=5 future observations should be contained in the 
  # prediction interval.  Note that the confidence level is now 98%.

  predIntNpar(dat, k = 3, m = 5) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            None
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Prediction Interval Method:      Exact
  #
  #Prediction Interval Type:        two-sided
  #
  #Confidence Level:                98.37945%
  #
  #Prediction Limit Rank(s):        1 20 
  #
  #Minimum Number of
  #Future Observations
  #Interval Should Contain:         3
  #
  #Total Number of
  #Future Observations:             5
  #
  #Prediction Interval:             LPL = 0.3647875
  #                                 UPL = 1.8173115

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


  # Construct the prediction limit based on the background well data 
  # using the maximum value as the upper prediction limit.  
  # Note that since all censored observations are censored at one 
  # censoring level and the censoring level is less than all of the 
  # uncensored observations, we can just supply the censoring level 
  # to predIntNpar.
  #-----------------------------------------------------------------

  with(EPA.09.Ex.18.3.TCE.df, 
    predIntNpar(TCE.ppb[Well.type == "Background"], 
      m = 4, pi.type = "upper", lb = 0))

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            None
  #
  #Data:                            TCE.ppb[Well.type == "Background"]
  #
  #Sample Size:                     18
  #
  #Prediction Interval Method:      Exact
  #
  #Prediction Interval Type:        upper
  #
  #Confidence Level:                81.81818%
  #
  #Prediction Limit Rank(s):        18 
  #
  #Number of Future Observations:   4
  #
  #Prediction Interval:             LPL =  0
  #                                 UPL = 12

  # Since the value of 14 ppb for Month 6 at the compliance well exceeds 
  # the upper prediction limit of 12, we might conclude that there is 
  # statistically significant evidence of an increase over background 
  # at CW-4.  However, the confidence level associated with this 
  # prediction limit is about 82%, which implies a Type I error level of 
  # 18%.  This means there is nearly a one in five chance of a false positive. 
  # Only additional background data and/or use of a retesting strategy 
  # (see predIntNparSimultaneous) would lower the false positive rate.

  #==========

  # Example 18-4 of USEPA (2009, p.18-19) shows how to construct 
  # a one-sided upper nonparametric prediction interval for the next 
  # median of order 3 of xylene at a downgradient well.  
  # The data for this example are stored in EPA.09.Ex.18.4.xylene.df.  
  # There are 8 monthly observations of xylene (ppb) at 3 background wells, 
  # and 3 montly observations of TCE at a compliance well.

  # Look at the data
  #-----------------

  EPA.09.Ex.18.4.xylene.df

  #   Month   Well  Well.type Xylene.ppb.orig Xylene.ppb Censored
  #1      1 Well.1 Background              <5        5.0     TRUE
  #2      2 Well.1 Background              <5        5.0     TRUE
  #3      3 Well.1 Background             7.5        7.5    FALSE
  #...
  #30     6 Well.4 Compliance              <5        5.0     TRUE
  #31     7 Well.4 Compliance             7.8        7.8    FALSE
  #32     8 Well.4 Compliance            10.4       10.4    FALSE

  longToWide(EPA.09.Ex.18.4.xylene.df, "Xylene.ppb.orig", "Month", "Well", 
    paste.row.name = TRUE)

  #        Well.1 Well.2 Well.3 Well.4
  #Month.1     <5    9.2     <5       
  #Month.2     <5     <5    5.4       
  #Month.3    7.5     <5    6.7       
  #Month.4     <5    6.1     <5       
  #Month.5     <5      8     <5       
  #Month.6     <5    5.9     <5     <5
  #Month.7    6.4     <5     <5    7.8
  #Month.8      6     <5     <5   10.4

  # Construct the prediction limit based on the background well data 
  # using the maximum value as the upper prediction limit. 
  # Note that since all censored observations are censored at one 
  # censoring level and the censoring level is less than all of the 
  # uncensored observations, we can just supply the censoring level 
  # to predIntNpar.
  #
  # To compute a prediction interval for a median of order 3 (i.e., 
  # a median based on 3 observations), this is equivalent to 
  # constructing a nonparametric prediction interval that must hold 
  # at least 2 of the next 3 future observations.
  #-----------------------------------------------------------------

  with(EPA.09.Ex.18.4.xylene.df, 
    predIntNpar(Xylene.ppb[Well.type == "Background"], 
      k = 2, m = 3, pi.type = "upper", lb = 0))

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            None
  #
  #Data:                            Xylene.ppb[Well.type == "Background"]
  #
  #Sample Size:                     24
  #
  #Prediction Interval Method:      Exact
  #
  #Prediction Interval Type:        upper
  #
  #Confidence Level:                99.1453%
  #
  #Prediction Limit Rank(s):        24 
  #
  #Minimum Number of
  #Future Observations
  #Interval Should Contain:         2
  #
  #Total Number of
  #Future Observations:             3
  #
  #Prediction Interval:             LPL = 0.0
  #                                 UPL = 9.2

  # The Month 8 observation at the Complance well is 10.4 ppb of Xylene, 
  # which is greater than the upper prediction limit of 9.2 ppb, so
  # conclude there is evidence of contamination at the 
  # 100% - 99% = 1% Type I Error Level

  #==========

  # Cleanup
  #--------

  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }

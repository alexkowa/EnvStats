\name{ciNparConfLevel}
\alias{ciNparConfLevel}
\title{
  Compute Confidence Level Associated with a Nonparametric Confidence Interval for a Quantile
}
\description{
  Compute the confidence level associated with a nonparametric confidence 
  interval for a quantile, given the sample size and order statistics 
  associated with the lower and upper bounds.
}
\usage{
  ciNparConfLevel(n, p = 0.5, lcl.rank = ifelse(ci.type == "upper", 0, 1), 
    n.plus.one.minus.ucl.rank = ifelse(ci.type == "lower", 0, 1),  
    ci.type = "two.sided")
}
\arguments{
  \item{n}{
  numeric vector of sample sizes.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.  
}
  \item{p}{
  numeric vector of probabilities specifying which quantiles to consider for 
  the sample size calculation.  All values of \code{p} must be between 0 and 
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
}
\details{
  If the arguments \code{n}, \code{p}, \code{lcl.rank}, and 
  \code{n.plus.one.minus.ucl.rank} are not all the same length, they are 
  replicated to be the 
  same length as the length of the longest argument.

  The help file for \code{\link{eqnpar}} explains how nonparametric confidence 
  intervals for quantiles are constructed and how the confidence level 
  associated with the confidence interval is computed based on specified values 
  for the sample size and the ranks of the order statistics used for 
  the bounds of the confidence interval.  
}
\value{
  A numeric vector of confidence levels.
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
  \code{\link{eqnpar}}, \code{\link{ciNparN}}, 
  \code{\link{plotCiNparDesign}}.
}
\examples{
  # Look at how the confidence level of a nonparametric confidence interval 
  # increases with increasing sample size for a fixed quantile:

  seq(5, 25, by = 5) 
  #[1] 5 10 15 20 25 

  round(ciNparConfLevel(n = seq(5, 25, by = 5), p = 0.9), 2) 
  #[1] 0.41 0.65 0.79 0.88 0.93

  #---------

  # Look at how the confidence level of a nonparametric confidence interval 
  # decreases as the quantile moves away from 0.5:

  seq(0.5, 0.9, by = 0.1) 
  #[1] 0.5 0.6 0.7 0.8 0.9

  round(ciNparConfLevel(n = 10, p = seq(0.5, 0.9, by = 0.1)), 2) 
  #[1] 1.00 0.99 0.97 0.89 0.65

  #==========

  # Reproduce Example 21-6 on pages 21-21 to 21-22 of USEPA (2009).  
  # Use 12 measurements of nitrate (mg/L) at a well used for drinking water 
  # to determine with 95% confidence whether or not the infant-based, acute 
  # risk standard of 10 mg/L has been violated.  Assume that the risk 
  # standard represents an upper 95'th percentile limit on nitrate 
  # concentrations.  So what we need to do is construct a one-sided 
  # lower nonparametric confidence interval for the 95'th percentile 
  # that has associated confidence level of no more than 95%, and we will 
  # compare the lower confidence limit with the MCL of 10 mg/L.  
  #
  # The data for this example are stored in EPA.09.Ex.21.6.nitrate.df.

  # Look at the data:
  #------------------

  EPA.09.Ex.21.6.nitrate.df
  #   Sampling.Date       Date Nitrate.mg.per.l.orig Nitrate.mg.per.l Censored
  #1      7/28/1999 1999-07-28                  <5.0              5.0     TRUE
  #2       9/3/1999 1999-09-03                  12.3             12.3    FALSE
  #3     11/24/1999 1999-11-24                  <5.0              5.0     TRUE
  #4       5/3/2000 2000-05-03                  <5.0              5.0     TRUE
  #5      7/14/2000 2000-07-14                   8.1              8.1    FALSE
  #6     10/31/2000 2000-10-31                  <5.0              5.0     TRUE
  #7     12/14/2000 2000-12-14                    11             11.0    FALSE
  #8      3/27/2001 2001-03-27                  35.1             35.1    FALSE
  #9      6/13/2001 2001-06-13                  <5.0              5.0     TRUE
  #10     9/16/2001 2001-09-16                  <5.0              5.0     TRUE
  #11    11/26/2001 2001-11-26                   9.3              9.3    FALSE
  #12      3/2/2002 2002-03-02                  10.3             10.3    FALSE

  # Determine what order statistic to use for the lower confidence limit
  # in order to achieve no more than 95% confidence.
  #---------------------------------------------------------------------

  conf.levels <- ciNparConfLevel(n = 12, p = 0.95, lcl.rank = 1:12, 
    ci.type = "lower")
  names(conf.levels) <- 1:12

  round(conf.levels, 2)
  #   1    2    3    4    5    6    7    8    9   10   11   12 
  #1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 0.98 0.88 0.54 
  
  # Using the 11'th largest observation for the lower confidence limit 
  # yields a confidence level of 88%.  Using the 10'th largest 
  # observation yields a confidence level of 98%.  The example in 
  # USEPA (2009) uses the 10'th largest observation.
  #
  # The 10'th largest observation is 11 mg/L which exceeds the 
  # MCL of 10 mg/L, so there is evidence of contamination.
  #--------------------------------------------------------------------

  with(EPA.09.Ex.21.6.nitrate.df, 
    eqnpar(Nitrate.mg.per.l, p = 0.95, ci = TRUE, 
      ci.type = "lower", lcl.rank = 10))

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            None
  #
  #Estimated Quantile(s):           95'th %ile = 22.56
  #
  #Quantile Estimation Method:      Nonparametric
  #
  #Data:                            Nitrate.mg.per.l
  #
  #Sample Size:                     12
  #
  #Confidence Interval for:         95'th %ile
  #
  #Confidence Interval Method:      exact
  #
  #Confidence Interval Type:        lower
  #
  #Confidence Level:                98.04317%
  #
  #Confidence Limit Rank(s):        10 
  #
  #Confidence Interval:             LCL =  11
  #                                 UCL = Inf

  #==========

  # Clean up
  #---------
  rm(conf.levels)
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }

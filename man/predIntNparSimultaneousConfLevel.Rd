\name{predIntNparSimultaneousConfLevel}
\alias{predIntNparSimultaneousConfLevel}
\title{
  Confidence Level of Simultaneous Nonparametric Prediction Interval for Continuous Distribution
}
\description{
  Compute the confidence level associated with a nonparametric simultaneous 
  prediction interval based on one of three possible rules: k-of-m, California, 
  or Modified California.  Observations are assumed to come from from a 
  continuous distribution.
}
\usage{
  predIntNparSimultaneousConfLevel(n, n.median = 1, k = 1, m = 2, r = 1, 
    rule = "k.of.m", lpl.rank = ifelse(pi.type == "upper", 0, 1), 
    n.plus.one.minus.upl.rank = ifelse(pi.type == "lower", 0, 1), 
    pi.type = "upper", integrate.args.list = NULL)
}
\arguments{
  \item{n}{
  vector of positive integers specifying the sample sizes.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are not allowed.  
}
  \item{n.median}{
  vector of positive odd integers specifying the sample size associated with the 
  future medians.  The default value is \code{n.median=1} (i.e., individual 
  observations).  Note that all future medians must be based on the same 
  sample size.
}
  \item{k}{
  for the \eqn{k}-of-\eqn{m} rule (\code{rule="k.of.m"}), a vector of positive integers 
  specifying the minimum number of observations (or medians) out of \eqn{m} 
  observations (or medians) (all obtained on one future sampling \dQuote{occassion}) 
  the prediction interval should contain.  
  The default value is \code{k=1}.  This argument is ignored when the argument 
  \code{rule} is not equal to \code{"k.of.m"}.
}
  \item{m}{
  vector of positive integers specifying the maximum number of future observations (or 
  medians) on one future sampling \dQuote{occasion}.  
  The default value is \code{m=2}, except when \code{rule="Modified.CA"}, in which 
  case this argument is ignored and \code{m} is automatically set equal to \code{4}.
}
  \item{r}{
  vector of positive integers specifying the number of future sampling 
  \dQuote{occasions}.  The default value is \code{r=1}.
}
  \item{rule}{
  character string specifying which rule to use.  The possible values are 
  \code{"k.of.m"} (\eqn{k}-of-\eqn{m} rule; the default), \code{"CA"} (California rule), 
  and \code{"Modified.CA"} (modified California rule).  
}
  \item{lpl.rank}{
  vector of positive integers indicating the rank of the order statistic to use for 
  the lower bound of the prediction interval.  When \code{pi.type="lower"}, the 
  default value is \code{lpl.rank=1} (implying the minimum value of \code{x} is used 
  as the lower bound of the prediction interval).  When \code{pi.type="upper"}, 
  the argument \code{lpl.rank} is set equal to \code{0}.
}
  \item{n.plus.one.minus.upl.rank}{
  vector of positive integers related to the rank of the order statistic to use for 
  the upper 
  bound of the prediction interval.  A value of \code{n.plus.one.minus.upl.rank=1} 
  (the default) means use the first largest value, and in general a value of \cr
  \code{n.plus.one.minus.upl.rank=}\eqn{i} means use the \eqn{i}'th largest value.  If \cr
  \code{pi.type="lower"}, this argument is set equal to \code{0}.
}
  \item{pi.type}{
  character string indicating what kind of prediction interval to compute.  
  The possible values are \code{"two.sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.
}
  \item{integrate.args.list}{
  list of arguments to supply to the \code{\link{integrate}} function.  The default 
  value is \code{NULL}.
}
}
\details{
  If the arguments \code{n}, \code{k}, \code{m}, \code{r}, \code{lpl.rank}, and 
  \code{n.plus.one.minus.upl.rank} are not all the same length, they are replicated 
  to be the same length as the length of the longest argument.

  The function \code{predIntNparSimultaneousConfLevel} computes the confidence level 
  based on Equation (8), (9), or (10) in the help file for 
  \code{\link{predIntNparSimultaneous}}, depending on the value of the argument 
  \code{rule}.

  Note that when \code{rule="k.of.m"} and \code{r=1}, this is equivalent to a 
  standard nonparametric prediction interval and you can use the function 
  \code{\link{predIntNparConfLevel}} instead.
}
\value{
  vector of values between 0 and 1 indicating the confidence level associated with 
  the specified simultaneous nonparametric prediction interval.
}
\references{
  See the help file for \code{\link{predIntNparSimultaneous}}.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help file for \code{\link{predIntNparSimultaneous}}.
}
\seealso{
  \code{\link{predIntNparSimultaneous}}, 
  \code{\link{predIntNparSimultaneousN}}, 
  \code{\link{plotPredIntNparSimultaneousDesign}}, 
  \code{\link{predIntNparSimultaneousTestPower}}, 
  \code{\link{predIntNpar}}, \code{\link{tolIntNpar}}.
}
\examples{
  # For the 1-of-3 rule with r=20 future sampling occasions, look at how the 
  # confidence level of a simultaneous nonparametric prediction interval 
  # increases with increasing sample size:

  seq(5, 25, by = 5) 
  #[1] 5 10 15 20 25 

  conf <- predIntNparSimultaneousConfLevel(n = seq(5, 25, by = 5), 
    k = 1, m = 3, r = 20)
  round(conf, 2) 
  #[1] 0.82 0.95 0.98 0.99 0.99

  #----------

  # For the 1-of-m rule with r=20 future sampling occasions, look at how the 
  # confidence level of a simultaneous nonparametric prediction interval 
  # increases as the number of future observations increases:

  1:5
  #[1] 1 2 3 4 5

  conf <- predIntNparSimultaneousConfLevel(n = 10, k = 1, m = 1:5, r = 20)
  round(conf, 2) 
  #[1] 0.33 0.81 0.95 0.98 0.99

  #----------

  # For the 1-of-3 rule, look at how the confidence level of a simultaneous 
  # nonparametric prediction interval decreases with number of future sampling 
  # occasions (r):

  seq(5, 20, by = 5)
  #[1]  5 10 15 20

  conf <- predIntNparSimultaneousConfLevel(n = 10, k = 1, m = 3, 
    r = seq(5, 20, by = 5))

  round(conf, 2) 
  #[1] 0.98 0.97 0.96 0.95

  #----------

  # For the 1-of-3 rule with r=20 future sampling occasions, look at how the 
  # confidence level of a simultaneous nonparametric prediction interval 
  # decreases as the rank of the upper prediction limit decreases:

  conf <- predIntNparSimultaneousConfLevel(n = 10, k = 1, m = 3, r = 20, 
    n.plus.one.minus.upl.rank = 1:5)

  round(conf, 2) 
  #[1] 0.95 0.82 0.63 0.43 0.25

  #----------

  # Clean up
  #---------
  rm(conf)

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

  # So the required confidence level is 0.98, or 98%.
  # Now determine the confidence level associated with each plan.
  # Note that both plans achieve the required confidence level.
 
  # 1) the 1-of-2 retesting plan for a median of order 3 using the 
  #    background maximum

  predIntNparSimultaneousConfLevel(n = 20, n.median = 3, k = 1, m = 2, r = r)
  #[1] 0.9940354


  # 2) the 1-of-4 plan on individual observations using the 3rd highest 
  #    background value.

  predIntNparSimultaneousConfLevel(n = 20, k = 1, m = 4, r = r, 
    n.plus.one.minus.upl.rank = 3)
  #[1] 0.9864909
 
  #==========

  # Cleanup
  #--------
  rm(n, nw, nc, ne, r, conf.level) 
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }


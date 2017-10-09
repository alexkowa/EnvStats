\name{predIntNparSimultaneousTestPower}
\alias{predIntNparSimultaneousTestPower}
\title{
  Probability That at Least One Set of Future Observations Violates the Given Rule Based on a Nonparametric Simultaneous Prediction Interval
}
\description{
  Compute the probability that at least one set of future observations violates the 
  given rule based on a nonparametric simultaneous prediction interval for the next 
  \eqn{r} future sampling occasions.  The three possible rules are: 
  \eqn{k}-of-\eqn{m}, California, or Modified California.  The probability is based 
  on assuming the true distribution of the observations is \link[stats:Normal]{normal}.
}
\usage{
  predIntNparSimultaneousTestPower(n, n.median = 1, k = 1, m = 2, r = 1, 
    rule = "k.of.m", lpl.rank = ifelse(pi.type == "upper", 0, 1), 
    n.plus.one.minus.upl.rank = ifelse(pi.type == "lower", 0, 1), 
    delta.over.sigma = 0, pi.type = "upper", r.shifted = r,  
    method = "approx", NMC = 100, ci = FALSE, ci.conf.level = 0.95, 
    integrate.args.list = NULL, evNormOrdStats.method = "royston") 
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
  vector of non-negative integers indicating the rank of the order statistic to use for 
  the lower bound of the prediction interval.  When \code{pi.type="lower"}, the 
  default value is \code{lpl.rank=1} (implying the minimum value of \code{x} is used 
  as the lower bound of the prediction interval).  When \code{pi.type="upper"}, 
  the argument \code{lpl.rank} is set equal to \code{0}.
}
  \item{n.plus.one.minus.upl.rank}{
  vector of non-negative integers related to the rank of the order statistic to use for 
  the upper bound of the prediction interval.  A value of \code{n.plus.one.minus.upl.rank=1} 
  (the default) means use the first largest value, and in general a value of \cr
  \code{n.plus.one.minus.upl.rank=}\eqn{i} means use the \eqn{i}'th largest value. 
  When \code{pi.type="lower"}, the argument \code{n.plus.one.minus.upl.rank} is set 
  equal to \code{0}.
}
  \item{delta.over.sigma}{
  numeric vector indicating the ratio \eqn{\Delta/\sigma}.  The quantity 
  \eqn{\Delta} (delta) denotes the difference between the mean of the population 
  that was sampled to construct the prediction interval, and the mean of the 
  population that will be sampled to produce the future observations.  The quantity 
  \eqn{\sigma} (sigma) denotes the population standard deviation for both populations.  
  The default value is \cr
  \code{delta.over.sigma=0}.
}
  \item{pi.type}{
  character string indicating what kind of prediction interval to compute.  
  The possible values are \code{"two.sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.
}
  \item{r.shifted}{
  vector of positive integers specifying the number of future sampling occasions for 
  which the scaled mean is shifted by \eqn{\Delta/\sigma}.  All values must be 
  integeters between \code{1} and the corresponding element of \code{r}.  
  The default value is \code{r.shifted=r}.
}
  \item{method}{
  character string indicating what method to use to compute the power.  The possible 
  values are \code{"approx"} (approximation based on \cr
  \code{\link{predIntNormSimultaneousTestPower}}; the default) and 
  \code{"simulate"} (Monte Carlo simulation).
}
  \item{NMC}{
  positive integer indicating the number of Monte Carlo trials to run when \cr
  \code{method="simulate"}.  The default value is \code{NMC=100}.
}
  \item{ci}{
  logical scalar indicating whether to compute a confidence interval for the power 
  when \code{method="simulate"}.  The default value is \code{ci=FALSE}.
}
  \item{ci.conf.level}{
  numeric scalar between 0 and 1 indicating the confidence level associated with the 
  confidence interval for the power.  The argument is ignored if \code{ci=FALSE} 
  or \code{method="approx"}.
}
  \item{integrate.args.list}{
  list of arguments to supply to the \code{\link{integrate}} function.  The default 
  value is \code{NULL}.
}
  \item{evNormOrdStats.method}{
  character string indicating which method to use in the call to 
  \code{\link{evNormOrdStatsScalar}} when \code{method="approx"}.  The default 
  value is \code{evNormOrdStats.method="royston"}.  See the DETAILS section for 
  more information.
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

  The function \code{\link{predIntNparSimultaneous}} computes a nonparametric simultaneous 
  prediction interval that will contain a certain number of future observations 
  with probability \eqn{(1-\alpha)100\%} for each of \eqn{r} future sampling 
  \dQuote{occasions}, 
  where \eqn{r} is some pre-specified positive integer.  The quantity \eqn{r} may 
  refer to \eqn{r} distinct future sampling occasions in time, or it may for example 
  refer to sampling at \eqn{r} distinct locations on one future sampling occasion, 
  assuming that the population standard deviation is the same at all of the \eqn{r} 
  distinct locations.

  The function \code{\link{predIntNparSimultaneous}} computes a nonparametric simultaneous  
  prediction interval based on one of three possible rules:
  \itemize{
  \item For the \eqn{k}-of-\eqn{m} rule (\code{rule="k.of.m"}), at least \eqn{k} of 
    the next \eqn{m} future observations will fall in the prediction 
    interval with probability \eqn{(1-\alpha)100\%} on each of the \eqn{r} future 
    sampling occasions.  If obserations are being taken sequentially, for a particular 
    sampling occasion, up to \eqn{m} observations may be taken, but once 
    \eqn{k} of the observations fall within the prediction interval, sampling can stop.  
    Note:  For this rule, when \eqn{r=1}, the results of \code{\link{predIntNparSimultaneous}} 
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
  observations.  For the function \code{\link{predIntNparSimultaneous}}, the argument 
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

  \bold{Computing Power} \cr
  The "power" of the prediction interval is defined as the probability that 
  at least one set of future observations violates the given rule based on a 
  simultaneous prediction interval for the next \eqn{r} future sampling occasions, 
  where the population for the future observations is allowed to differ from 
  the population for the observations used to construct the prediction interval.

  For the function \code{predIntNparSimultaneousTestPower}, power is computed assuming 
  both the background and future the observations come from normal distributions with 
  the same standard deviation, but the means of the distributions are allowed to differ.
  The quantity \eqn{\Delta} (upper case delta) denotes the difference between the 
  mean of the population that was sampled to construct the prediction interval, and 
  the mean of the population that will be sampled to produce the future observations.  
  The quantity \eqn{\sigma} (sigma) denotes the population standard deviation of both 
  of these populations.  The argument \code{delta.over.sigma} corresponds to the 
  quantity \eqn{\Delta/\sigma}.

  \emph{Approximate Power} (\code{method="approx"}) \cr
  Based on Gansecki (2009), the power of a nonparametric simultaneous prediction 
  interval when the underlying observations come from a nomral distribution 
  can be approximated by the power of a normal simultaneous prediction 
  interval (see \code{\link{predIntNormSimultaneousTestPower}}) where the multiplier 
  \eqn{K} is replaced with the expected value of the normal order statistic that 
  corresponds to the rank of the order statistic used for the upper or lower bound 
  of the prediction interval.  Gansecki (2009) uses the approximation:
  \deqn{K = \Phi^{-1}(\frac{i - 0.5}{n}) \;\;\;\;\;\; (8)}
  where \eqn{\Phi} denotes the cumulative distribution function of the standard 
  normal distribution and \eqn{i} denotes the rank of the order statistic used 
  as the prediction limit.  By default, the value of the argument \cr
  \code{evNormOrdStats.method="royston"}, so the function 
  \code{predIntNparSimultaneousTestPower} uses the exact value of the 
  expected value of the normal order statistic in the call to 
  \code{\link{evNormOrdStatsScalar}}.  You can change the 
  method of computing the expected value of the normal order statistic by 
  changing the value of the argument \code{evNormOrdStats.method}.
  \cr

  \emph{Power Based on Monte Carlo Simulation} (\code{method="simulate"}) \cr
  When \code{method="simulate"}, the power of the nonparametric simultaneous 
  prediction interval is estimated based on a Monte Carlo simulation.  The argument 
  \code{NMC} determines the number of Monte Carlo trials.  If \code{ci=TRUE}, a 
  confidence interval for the power is created based on the \code{NMC} Monte Carlo 
  estimates of power.
}
\value{
  vector of values between 0 and 1 equal to the probability that 
  the rule will be violated.
}
\references{
  See the help file for \code{\link{predIntNparSimultaneous}}.

  Gansecki, M. (2009).  \emph{Using the Optimal Rank Values Calculator}.  
  US Environmental Protection Agency, Region 8, March 10, 2009.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help file for \code{\link{predIntNparSimultaneous}}.

  In the course of designing a sampling program, an environmental scientist may wish 
  to determine the relationship between sample size, significance level, power, and 
  scaled difference if one of the objectives of the sampling program is to determine 
  whether two distributions differ from each other.  The functions 
  \code{predIntNparSimultaneousTestPower} and \cr
  \code{\link{plotPredIntNparSimultaneousTestPowerCurve}} can be 
  used to investigate these relationships for the case of normally-distributed 
  observations. 
}
\seealso{
  \code{\link{plotPredIntNparSimultaneousTestPowerCurve}}, 
  \code{\link{predIntNparSimultaneous}}, \cr
  \code{\link{predIntNparSimultaneousN}}, 
  \code{\link{predIntNparSimultaneousConfLevel}}, \cr
  \code{\link{plotPredIntNparSimultaneousDesign}}, 
  \code{\link{predIntNpar}}, \code{\link{tolIntNpar}}.
}
\examples{
  # Example 19-5 of USEPA (2009, p. 19-33) shows how to compute nonparametric upper 
  # simultaneous prediction limits for various rules based on trace mercury data (ppb) 
  # collected in the past year from a site with four background wells and 10 compliance 
  # wells (data for two of the compliance wells  are shown in the guidance document).  
  # The facility must monitor the 10 compliance wells for five constituents 
  # (including mercury) annually.
  
  # Here we will compute the confidence levels and powers associated with  
  # two different sampling plans: 
  # 1) the 1-of-2 retesting plan for a median of order 3 using the 
  #    background maximum and 
  # 2) the 1-of-4 plan on individual observations using the 3rd highest 
  #    background value.
  # Power will be computed assuming a normal distribution and setting 
  # delta.over.sigma equal to 2, 3, and 4.
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


  # 2) the 1-of-4 plan based on individual observations using the 3rd highest 
  #    background value.

  predIntNparSimultaneousConfLevel(n = 20, k = 1, m = 4, r = r, 
    n.plus.one.minus.upl.rank = 3)
  #[1] 0.9864909

  #------------------------------------------------------------------------------
  # Compute approximate power of each plan to detect contamination at just 1 well 
  # assuming true underying distribution of Hg is Normal at all wells and 
  # using delta.over.sigma equal to 2, 3, and 4.
  #------------------------------------------------------------------------------

  # Computer aproximate power for 
  # 1) the 1-of-2 retesting plan for a median of order 3 using the 
  #    background maximum

  predIntNparSimultaneousTestPower(n = 20, n.median = 3, k = 1, m = 2, r = r, 
    delta.over.sigma = 2:4, r.shifted = 1)
  #[1] 0.3953712 0.9129671 0.9983054


  # Compute approximate power for
  # 2) the 1-of-4 plan based on individual observations using the 3rd highest 
  #    background value.

  predIntNparSimultaneousTestPower(n = 20, k = 1, m = 4, r = r, 
    n.plus.one.minus.upl.rank = 3, delta.over.sigma = 2:4, r.shifted = 1)
  #[1] 0.4367972 0.8694664 0.9888779


  #----------

  \dontrun{
  # Compare estimated power using approximation method with estimated power
  # using Monte Carlo simulation for the 1-of-4 plan based on individual 
  # observations using the 3rd highest background value.

  predIntNparSimultaneousTestPower(n = 20, k = 1, m = 4, r = r, 
    n.plus.one.minus.upl.rank = 3, delta.over.sigma = 2:4, r.shifted = 1, 
    method = "simulate", ci = TRUE, NMC = 1000)
  #[1] 0.437 0.863 0.989
  #attr(,"conf.int")
  #         [,1]      [,2]      [,3]
  #LCL 0.4111999 0.8451148 0.9835747
  #UCL 0.4628001 0.8808852 0.9944253
  }
 
  #==========

  # Cleanup
  #--------
  rm(n, nw, nc, ne, r, conf.level) 
}
\keyword{ distribution }
\keyword{ htest }


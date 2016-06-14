\name{propTestPower}
\alias{propTestPower}
\title{
  Compute the Power of a One- or Two-Sample Proportion Test
}
\description{
  Compute the power of a one- or two-sample proportion test, given the sample size(s), 
  true proportion(s), and significance level.
}
\usage{
  propTestPower(n.or.n1, p.or.p1 = 0.5, n2 = n.or.n1, 
    p0.or.p2 = 0.5, alpha = 0.05, sample.type = "one.sample", 
    alternative = "two.sided", approx = TRUE, 
    correct = sample.type == "two.sample", warn = TRUE, 
    return.exact.list = TRUE)
}
\arguments{
  \item{n.or.n1}{
  numeric vector of sample sizes.  When \code{sample.type="one.sample"}, 
  this argument denotes \eqn{n}, the number of observations in the single sample.  
  When \code{sample.type="two.sample"}, this argument denotes \eqn{n_1}, 
  the number of observations from group 1.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
}
  \item{p.or.p1}{
  numeric vector of proportions.  When \code{sample.type="one.sample"}, 
  this argument denotes the true value of \eqn{p}, the probability of \dQuote{success}.  When \cr
  \code{sample.type="two.sample"}, this argument denotes the value of \eqn{p_1}, 
  the probability of \dQuote{success} in group 1.  The default value is \code{p.or.p1=0.5}.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
}
  \item{n2}{
  numeric vector of sample sizes for group 2.  The default value is \code{n2=n.or.n1}.  
  This argument is ignored when \code{sample.type="one.sample"}.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
}
  \item{p0.or.p2}{
  numeric vector of proportions.  When \code{sample.type="one.sample"}, 
  this argument denotes the hypothesized value of \eqn{p}, the probability of \dQuote{success}.  
  When \code{sample.type="two.sample"}, this argument denotes the value of \eqn{p_2}, 
  the probability of \dQuote{success} in group 2.  The default value is \cr
  \code{p0.or.p2=0.5}.  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
}
  \item{alpha}{
  numeric vector of numbers between 0 and 1 indicating the Type I error level 
  associated with the hypothesis test.  The default value is \code{alpha=0.05}.
}
  \item{sample.type}{
  character string indicating whether to compute power based on a one-sample or 
  two-sample hypothesis test.  When \code{sample.type="one.sample"}, 
  the computed power is based on a hypothesis test for a single proportion.  
  When \cr
  \code{sample.type="two.sample"}, the computed power is based on a 
  hypothesis test for the difference between two proportions.  
  The default value is \cr
  \code{sample.type="one.sample"}.
}
  \item{alternative}{
  character string indicating the kind of alternative hypothesis.  
  The possible values are \code{"two.sided"} (the default), \code{"less"}, and 
  \code{"greater"}.
}
  \item{approx}{
  logical scalar indicating whether to compute the power based on the normal 
  approximation to the binomial distribution.  The default value is \code{approx=TRUE}.  
  Currently, the exact method (\code{approx=FALSE}) is only available for the 
  one-sample case (i.e., \code{sample.type="one.sample"}).
}
  \item{correct}{
  logical scalar indicating whether to use the continuity correction when \cr
  \code{approx=TRUE}.  The default value is \code{approx=TRUE} when \cr
  \code{sample.type="two.sample"} and \code{approx=FALSE} when \cr
  \code{sample.type="one.sample"}.  This argument is ignored when \cr
  \code{approx=FALSE}.
}
  \item{warn}{
  logical scalar indicating whether to issue a warning.  The default value is \cr
  \code{warn=TRUE}.  When \code{approx=TRUE} (power based on the normal approximation) and 
  \code{warn=TRUE}, a warning is issued for cases when the normal approximation to the 
  binomial distribution probably is not accurate.  When \cr
  \code{approx=FALSE} (power based on the exact test) and \code{warn=TRUE}, 
  a warning is issued when the user-supplied sample size is too small to 
  yield a significance level less than or equal to the user-supplied value of 
  \code{alpha}.
}
  \item{return.exact.list}{
  logical scalar relevant to the case when \code{approx=FALSE} 
  (i.e., when the power is based on the exact test).  This argument indicates whether 
  to return a list containing extra information about the exact test in addition to 
  the power of the exact test.  By default, \code{propTestPower} returns only a vector 
  containing the computed power(s) (see the VALUE section below).  When \cr
  \code{return.exact.list=TRUE} (the default) and \code{approx=FALSE}, \cr
  \code{propTestPower} returns a list with components indicating the power of the 
  exact test, the true significance level associated with the exact test, and the 
  critical values associated with the exact test (see the DETAILS section for more information).
}
}
\details{
  If the arguments \code{n.or.n1}, \code{p.or.p1}, \code{n2}, \code{p0.or.p2}, and 
  \code{alpha} are not all the same length, they are replicated to be the same length 
  as the length of the longest argument.

  The power is based on the difference \code{p.or.p1 - p0.or.p2}.

  \strong{One-Sample Case (\code{sample.type="one.sample"})}.  
  \describe{

  \item{\code{approx=TRUE}}{ When \code{sample.type="one.sample"} and \code{approx=TRUE}, 
  power is computed based on the test that uses the normal approximation to the 
  binomial distribution; see the help file for \code{\link{prop.test}}.  
  The formula for this test and its associated power is presented in most standard statistics 
  texts, including Zar (2010, pp. 534-537, 539-541).
  }

  \item{\code{approx=FALSE}}{ When \code{sample.type="one.sample"} and \code{approx=FALSE}, 
  power is computed based on the exact binomial test; see the help file for \code{\link{binom.test}}.  
  The formula for this test and its associated power is presented in most standard statistics 
  texts, including Zar (2010, pp. 532-534, 539) and 
  Millard and Neerchal (2001, pp. 385-386, 504-506).
  }

  }

 \strong{Two-Sample Case (\code{sample.type="two.sample"})}. 

  When \code{sample.type="two.sample"}, power is computed based on the test that uses the 
  normal approximation to the binomial distribution; 
  see the help file for \code{\link{prop.test}}.  
  The formula for this test and its associated power is presented in standard statistics texts, 
  including Zar (2010, pp. 549-550, 552-553) and 
  Millard and Neerchal (2001, pp. 443-445, 508-510).
}
\value{
  By default, \code{propTestPower} returns a numeric vector of powers.  
  For the one-sample proportion test (\code{sample.type="one.sample"}), 
  when \code{approx=FALSE} and \cr
  \code{return.exact.list=TRUE}, \code{propTestPower} 
  returns a list with the following components:

  \item{power}{numeric vector of powers.}
  \item{alpha}{numeric vector containing the true significance levels. 
    Because of the discrete nature of the binomial distribution, the true significance 
    levels usually do not equal the significance level supplied by the user in the 
    argument \code{alpha}.}
  \item{q.critical.lower}{numeric vector of lower critical values for rejecting the null 
    hypothesis.  If the observed number of "successes" is \emph{less than or equal to} these values, 
    the null hypothesis is rejected. (Not present if \code{alternative="greater"}.)}
  \item{q.critical.upper}{numeric vector of upper critical values for rejecting the null 
    hypothesis.  If the observed number of "successes" is \emph{greater than} these values, 
    the null hypothesis is rejected. (Not present if \code{alternative="less"}.)}
}
\references{
  Berthouex, P.M., and L.C. Brown. (1994). \emph{Statistics for Environmental Engineers}. 
  Lewis Publishers, Boca Raton, FL, Chapter 15.

  Casagrande, J.T., M.C. Pike, and P.G. Smith. (1978). 
  An Improved Approximation Formula for Calculating Sample Sizes for Comparing Two Binomial Distributions. 
  \emph{Biometrics} \bold{34}, 483-486.

  Fleiss, J. L. (1981). \emph{Statistical Methods for Rates and Proportions}. Second Edition. 
  John Wiley and Sons, New York, Chapters 1-2.

  Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution Monitoring}. 
  Van Nostrand Reinhold, New York, NY.

  Haseman, J.K. (1978). Exact Sample Sizes for Use with the Fisher-Irwin Test for 2x2 Tables. 
  \emph{Biometrics} \bold{34}, 106-109.

  Millard, S.P., and N. Neerchal. (2001). \emph{Environmental Statistics with S-Plus}.  
  CRC Press, Boca Raton, FL.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition. 
  Prentice-Hall, Upper Saddle River, NJ. 
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The binomial distribution is used to model processes with binary (Yes-No, Success-Failure, 
  Heads-Tails, etc.) outcomes.  It is assumed that the outcome of any one trial is independent 
  of any other trial, and that the probability of \dQuote{success}, \eqn{p}, is the same on each trial.  
  A binomial discrete random variable \eqn{X} is the number of "successes" in \eqn{n} independent 
  trials.  A special case of the binomial distribution occurs when \eqn{n=1}, in which case \eqn{X} 
  is also called a Bernoulli random variable.

  In the context of environmental statistics, the binomial distribution is sometimes used to model the 
  proportion of times a chemical concentration exceeds a set standard in a given period of time 
  (e.g., Gilbert, 1987, p.143), or to compare the proportion of detects in a compliance well vs. 
  a background well (e.g., USEPA, 1989b, Chapter 8, p.3-7).

  In the course of designing a sampling program, an environmental scientist may wish to determine the 
  relationship between sample size, power, significance level, and the difference between the 
  hypothesized and true proportions if one of the objectives of the sampling program is to 
  determine whether a proprtion differs from a specified level or two proportions differ from each other.  
  The functions \code{propTestPower}, \code{\link{propTestN}}, \code{\link{propTestMdd}}, and 
  \code{\link{plotPropTestDesign}} can be used to investigate these relationships for the case of 
  binomial proportions.

  Studying the two-sample proportion test, Haseman (1978) found that the formulas used to estimate the 
  power that do not incorporate the continuity correction tend to underestimate the power.  
  Casagrande, Pike, and Smith (1978) found that the formulas that do incorporate the continuity 
  correction provide an excellent approximation.
}
\seealso{
  \code{\link{propTestN}}, \code{\link{propTestMdd}}, \code{\link{plotPropTestDesign}}, 
  \code{\link{prop.test}}, \code{\link{binom.test}}.
}
\examples{
  # Look at how the power of the one-sample proportion test 
  # increases with increasing sample size:

  seq(20, 50, by=10) 
  #[1] 20 30 40 50 

  power <- propTestPower(n.or.n1 = seq(20, 50, by=10), 
    p.or.p1 = 0.7, p0 = 0.5) 

  round(power, 2) 
  #[1] 0.43 0.60 0.73 0.83

  #----------

  # Repeat the last example, but compute the power based on 
  # the exact test instead of the approximation. 
  # Note that the significance level varies with sample size and 
  # never attains the requested level of 0.05.

  prop.test.list <- propTestPower(n.or.n1 = seq(20, 50, by=10), 
    p.or.p1 = 0.7, p0 = 0.5, approx=FALSE) 

  lapply(prop.test.list, round, 2) 
  #$power: 
  #[1] 0.42 0.59 0.70 0.78 
  #
  #$alpha: 
  #[1] 0.04 0.04 0.04 0.03 
  #
  #$q.critical.lower: 
  #[1] 5 9 13 17 
  #
  #$q.critical.upper: 
  #[1] 14 20 26 32

  #==========

  # Look at how the power of the two-sample proportion test 
  # increases with increasing difference between the two 
  # population proportions:

  seq(0.5, 0.1, by=-0.1) 
  #[1] 0.5 0.4 0.3 0.2 0.1 

  power <- propTestPower(30, sample.type = "two", 
    p.or.p1 = seq(0.5, 0.1, by=-0.1)) 
  #Warning message:
  #In propTestPower(30, sample.type = "two", p.or.p1 = seq(0.5, 0.1,  :
  #The sample sizes 'n1' and 'n2' are too small, relative to the given 
  # values of 'p1' and 'p2', for the normal approximation to work well 
  # for the following element indices:
  #         5 

  round(power, 2) 
  #[1] 0.05 0.08 0.26 0.59 0.90

  #----------

  # Look at how the power of the two-sample proportion test 
  # increases with increasing values of Type I error:

  power <- propTestPower(30, sample.type = "two", 
    p.or.p1 = 0.7, 
    alpha = c(0.001, 0.01, 0.05, 0.1)) 

  round(power, 2) 
  #[1] 0.02 0.10 0.26 0.37

  #==========

  # Clean up
  #---------
  rm(power, prop.test.list)

  #==========

  # Modifying the example on pages 8-5 to 8-7 of USEPA (1989b), 
  # determine how adding another 20 observations to the background 
  # well to increase the sample size from 24 to 44 will affect the 
  # power of detecting a difference in the proportion of detects of 
  # cadmium between the background and compliance wells.  Set the 
  # compliance well to "group 1" and set the background well to 
  # "group 2".  Assume the true probability of a "detect" at the 
  # background well is 1/3, set the probability of a "detect" at the 
  # compliance well to 0.4, use a 5% significance level, and use the 
  # upper one-sided alternative (probability of a "detect" at the 
  # compliance well is greater than the probability of a "detect" at 
  # the background well). 
  # (The original data are stored in EPA.89b.cadmium.df.) 
  #
  # Note that the power does increase (from 9% to 12%), but is relatively 
  # very small.

  EPA.89b.cadmium.df
  #   Cadmium.orig Cadmium Censored  Well.type
  #1           0.1   0.100    FALSE Background
  #2          0.12   0.120    FALSE Background
  #3           BDL   0.000     TRUE Background
  # ..........................................
  #86          BDL   0.000     TRUE Compliance
  #87          BDL   0.000     TRUE Compliance
  #88          BDL   0.000     TRUE Compliance


  p.hat.back <- with(EPA.89b.cadmium.df, 
    mean(!Censored[Well.type=="Background"])) 

  p.hat.back 
  #[1] 0.3333333 

  p.hat.comp <- with(EPA.89b.cadmium.df, 
    mean(!Censored[Well.type=="Compliance"])) 

  p.hat.comp 
  #[1] 0.375 

  n.back <- with(EPA.89b.cadmium.df, 
    sum(Well.type == "Background")) 

  n.back 
  #[1] 24 

  n.comp <- with(EPA.89b.cadmium.df, 
    sum(Well.type == "Compliance")) 

  n.comp 
  #[1] 64 

  propTestPower(n.or.n1 = n.comp, 
    p.or.p1 = 0.4, 
    n2 = c(n.back, 44), p0.or.p2 = p.hat.back, 
    alt="greater", sample.type="two") 
  #[1] 0.08953013 0.12421135

  #----------

  # Clean up
  #---------
  rm(p.hat.back, p.hat.comp, n.back, n.comp)
}
\keyword{ design }
\keyword{ htest }
\keyword{ models }
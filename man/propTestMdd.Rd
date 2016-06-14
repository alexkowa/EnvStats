\name{propTestMdd}
\alias{propTestMdd}
\title{
  Minimal Detectable Difference Associated with a One- or Two-Sample Proportion Test
}
\description{
  Compute the minimal detectable difference associated with a one- or two-sample 
  proportion test, given the sample size, power, and significance level.
}
\usage{
  propTestMdd(n.or.n1, n2 = n.or.n1, p0.or.p2 = 0.5, alpha = 0.05, power = 0.95, 
    sample.type = "one.sample", alternative = "two.sided", 
    two.sided.direction = "greater", approx = TRUE, 
    correct = sample.type == "two.sample", warn = TRUE, 
    return.exact.list = TRUE, tol = 1e-07, maxiter = 1000)
}
\arguments{
  \item{n.or.n1}{
  numeric vector of sample sizes.  When \code{sample.type="one.sample"}, 
  this argument denotes \eqn{n}, the number of observations in the single sample.  
  When \code{sample.type="two.sample"}, this argument denotes \eqn{n_1}, 
  the number of observations from group 1.  
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
  \item{power}{
  numeric vector of numbers between 0 and 1 indicating the power associated with 
  the hypothesis test. The default value is \code{power=0.95}.
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
  \item{two.sided.direction}{
  character string indicating the direction (positive or negative) for the 
  minimal detectable difference when \code{alternative="two.sided"}.  When \cr
  \code{two.sided.direction="greater"} (the default), the minimal 
  detectable difference is positive.  When \code{two.sided.direction="less"}, 
  the minimal detectable difference is negative.  This argument 
  is ignored if \code{alternative="less"} or \code{alternative="greater"}.
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
  the power of the exact test.  By default, \code{propTestMdd} returns only a vector 
  containing the computed minimal detectable difference(s) (see the VALUE section below). 
  When \cr
  \code{return.exact.list=TRUE} (the default) and \code{approx=FALSE}, \cr
  \code{propTestMdd} returns a list with components indicating the minimal detectable 
  difference(s), power of the exact test, the true significance level associated with 
  the exact test, and the critical values associated with the exact test (see the 
  DETAILS section for more information).
}
  \item{tol}{
  numeric scalar passed to the \code{\link{uniroot}} function that indicates the 
  tolerance to use in the search algorithm.  The default value is \code{tol=1e-7}.
}  \item{maxiter}{
  integer passed to the \code{\link{uniroot}} function that indicates the maximum 
  number of iterations to use in the search algorithm.  The default value is 
  \code{maxiter=1000}.
}
}
\details{
  If the arguments \code{n.or.n1}, \code{n2}, \code{p0.or.p2}, \code{alpha}, and 
  \code{power} are not all the same length, they are replicated to be the same 
  length as the length of the longest argument.

  \emph{One-Sample Case} (\code{sample.type="one.sample"}) \cr
  The help file for \code{\link{propTestPower}} gives references that explain 
  how the power of the one-sample proportion test is computed based on the values of 
  \eqn{p_0} (the hypothesized value for \eqn{p}, the probability of \dQuote{success}), 
  \eqn{p} (the true value of \eqn{p}), the sample size \eqn{n}, and the Type 
  I error level \eqn{\alpha}.  The function \code{propTestMdd} computes the value 
  of the minimal detectable difference \eqn{p - p_0} for specified values of 
  sample size, power, and Type I error level by calling the \code{\link{uniroot}} 
  function to perform a search.
  \cr
  
  \emph{Two-Sample Case} (\code{sample.type="two.sample"}) \cr
  The help file for \code{\link{propTestPower}} gives references that explain 
  how the power of the two-sample proportion test is computed based on the values of 
  \eqn{p_1} (the value of the probability of \dQuote{success} for group 1), 
  \eqn{p_2} (the value of the probability of \dQuote{success} for group 2), 
  the sample sizes for groups 1 and 2 (\eqn{n_1} and \eqn{n_2}), and the Type 
  I error level \eqn{\alpha}.  The function \code{propTestMdd} computes the value 
  of the minimal detectable difference \eqn{p_1 - p_2} for specified values of 
  sample size, power, and Type I error level by calling the \code{\link{uniroot}} 
  function to perform a search.
}
\value{
  \strong{Approximate Test (\code{approx=TRUE})}.
  numeric vector of minimal detectable differences.

  \strong{Exact Test (\code{approx=FALSE})}.
  If \code{return.exact.list=FALSE}, \code{propTestMdd} returns a numeric vector of 
  minimal detectable differences.

  If \code{return.exact.list=TRUE}, \code{propTestMdd} returns a list with the 
  following components:

  \item{delta}{numeric vector of minimal detectable differences.}
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
  See the help file for \code{\link{propTestPower}}.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help file for \code{\link{propTestPower}}.
}
\seealso{
  \code{\link{propTestPower}}, \code{\link{propTestN}}, 
  \code{\link{plotPropTestDesign}}, \code{\link{prop.test}}, \code{\link{binom.test}}.
}
\examples{
  # Look at how the minimal detectable difference of the one-sample 
  # proportion test increases with increasing required power:

  seq(0.5, 0.9, by = 0.1) 
  #[1] 0.5 0.6 0.7 0.8 0.9 

  mdd <- propTestMdd(n.or.n1 = 50, power = seq(0.5, 0.9, by=0.1)) 

  round(mdd, 2) 
  #[1] 0.14 0.16 0.17 0.19 0.22

  #----------

  # Repeat the last example, but compute the minimal detectable difference 
  # based on the exact test instead of the approximation.  Note that with a 
  # sample size of 50, the largest significance level less than or equal to 
  # 0.05 for the two-sided alternative is 0.03.

  mdd.list <- propTestMdd(n.or.n1 = 50, power = seq(0.5, 0.9, by = 0.1), 
    approx = FALSE) 

  lapply(mdd.list, round, 2) 
  #$delta
  #[1] 0.15 0.17 0.18 0.20 0.23
  #
  #$power
  #[1] 0.5 0.6 0.7 0.8 0.9
  #
  #$alpha
  #[1] 0.03 0.03 0.03 0.03 0.03
  #
  #$q.critical.lower
  #[1] 17 17 17 17 17
  #
  #$q.critical.upper
  #[1] 32 32 32 32 32

  #==========

  # Look at how the minimal detectable difference for the two-sample 
  # proportion test decreases with increasing sample sizes.  Note that for 
  # the specified significance level, power, and true proportion in group 2, 
  # no minimal detectable difference is attainable for a sample size of 10 in 
  # each group.

  seq(10, 50, by=10) 
  #[1] 10 20 30 40 50 

  propTestMdd(n.or.n1 = seq(10, 50, by = 10), p0.or.p2 = 0.5, 
    sample.type = "two", alternative="greater") 
  #[1]        NA 0.4726348 0.4023564 0.3557916 0.3221412
  #Warning messages:
  #1: In propTestMdd(n.or.n1 = seq(10, 50, by = 10), p0.or.p2 = 0.5, 
  #     sample.type = "two",  :
  #  Elements with a missing value (NA) indicate no attainable minimal detectable 
  #    difference for the given values of 'n1', 'n2', 'p2', 'alpha', and 'power'
  #2: In propTestMdd(n.or.n1 = seq(10, 50, by = 10), p0.or.p2 = 0.5, 
  #      sample.type = "two",  :
  #  The sample sizes 'n1' and 'n2' are too small, relative to the computed value 
  #    of 'p1' and the given value of 'p2', for the normal approximation to work 
  #    well for the following element indices:
  #         2 3 

  #----------

  # Look at how the minimal detectable difference for the two-sample proportion 
  # test decreases with increasing values of Type I error:

  mdd <- propTestMdd(n.or.n1 = 100, n2 = 120, p0.or.p2 = 0.4, sample.type = "two", 
     alpha = c(0.01, 0.05, 0.1, 0.2)) 

  round(mdd, 2) 
  #[1] 0.29 0.25 0.23 0.20

  #----------

  # Clean up
  #---------
  rm(mdd, mdd.list) 

  #==========

  # Modifying the example on pages 8-5 to 8-7 of USEPA (1989b), determine the 
  # minimal detectable difference to detect a difference in the proportion of 
  # detects of cadmium between the background and compliance wells.  Set the 
  # compliance well to "group 1" and the background well to "group 2".  Assume 
  # the true probability of a "detect" at the background well is 1/3, use a 
  # 5% significance level, use 80%, 90%, and 95% power, use the given sample 
  # sizes of 64 observations at the compliance well and 24 observations at the 
  # background well, and use the upper one-sided alternative (probability of a 
  # "detect" at the compliance well is greater than the probability of a "detect" 
  # at the background well). 
  # (The data are stored in EPA.89b.cadmium.df.)  
  #
  # Note that the minimal detectable difference increases from 0.32 to 0.37 to 0.40 as 
  # the required power increases from 80% to 90% to 95%.  Thus, in order to detect a 
  # difference in probability of detection between the compliance and background 
  # wells, the probability of detection at the compliance well must be 0.65, 0.70, 
  # or 0.74 (depending on the required power).

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

  mdd <- propTestMdd(n.or.n1 = n.comp, n2 = n.back, 
    p0.or.p2 = p.hat.back, power = c(.80, .90, .95), 
    sample.type = "two", alternative = "greater") 

  round(mdd, 2) 
  #[1] 0.32 0.37 0.40 

  round(mdd + p.hat.back, 2) 
  #[1] 0.65 0.70 0.73

  #----------

  # Clean up
  #---------
  rm(p.hat.back, p.hat.comp, n.back, n.comp, mdd)
}
\keyword{ design }
\keyword{ htest }
\keyword{ models }

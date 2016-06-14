\name{propTestN}
\alias{propTestN}
\title{
  Compute Sample Size Necessary to Achieve a Specified Power for a One- or Two-Sample Proportion Test
}
\description{
  Compute the sample size necessary to achieve a specified power for a one- or 
  two-sample proportion test, given the true proportion(s) and significance level.
}
\usage{
  propTestN(p.or.p1, p0.or.p2, alpha = 0.05, power = 0.95, 
    sample.type = "one.sample", alternative = "two.sided", 
    ratio = 1, approx = TRUE, 
    correct = sample.type == "two.sample", 
    round.up = TRUE, warn = TRUE, return.exact.list = TRUE, 
    n.min = 2, n.max = 10000, tol.alpha = 0.1 * alpha, 
    tol = 1e-7, maxiter = 1000)
}
\arguments{
  \item{p.or.p1}{
  numeric vector of proportions.  When \code{sample.type="one.sample"}, this argument denotes the 
  true value of \eqn{p}, the probability of \dQuote{success}.  \cr
  When \code{sample.type="two.sample"}, this argument denotes the value of \eqn{p_1}, 
  the probability of \dQuote{success} in group 1.  The default value is \cr
  \code{p.or.p1=0.5}.  Missing (\code{NA}), undefined (\code{NaN}), and infinite 
  (\code{Inf}, \code{-Inf}) values are not allowed.
}
  \item{p0.or.p2}{
  numeric vector of proportions.  When \code{sample.type="one.sample"}, 
  this argument denotes the hypothesized value of \eqn{p}, the probability of \dQuote{success}.  
  When \code{sample.type="two.sample"}, this argument denotes the value of \eqn{p_2}, 
  the probability of \dQuote{success} in group 2.  The default value is \cr
  \code{p0.or.p2=0.5}.  Missing (\code{NA}), undefined (\code{NaN}), and infinite 
  (\code{Inf}, \code{-Inf}) values are not allowed.
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
  character string indicating whether to compute sample size based on a one-sample or 
  two-sample hypothesis test.  \cr
  When \code{sample.type="one.sample"}, the computed sample size is based 
  on a hypothesis test for a single proportion.  \cr
  When \code{sample.type="two.sample"}, the computed sample size is based 
  on a hypothesis test for the difference between two proportions. \cr  
  The default value is \code{sample.type="one.sample"}.
}
  \item{alternative}{
  character string indicating the kind of alternative hypothesis.  
  The possible values are \code{"two.sided"} (the default), \code{"less"}, and 
  \code{"greater"}.
}
  \item{ratio}{
  numeric vector indicating the ratio of sample size in group 2 to sample size 
  in group 1 (\eqn{n_2/n_1}).  The default value is \code{ratio=1}.  
  All values of \code{ratio} must be greater than or equal to 1.  
  This argument is ignored if \cr
  \code{sample.type="one.sample"}.
}
  \item{approx}{
  logical scalar indicating whether to compute the sample size based on the normal 
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
  \item{round.up}{
  logical scalar indicating whether to round up the values of the computed sample size(s) 
  to the next smallest integer.  The default value is \code{round.up=TRUE}.
}
  \item{warn}{
  logical scalar indicating whether to issue a warning.  The default value is \cr
  \code{warn=TRUE}.  When \code{approx=TRUE} (sample size based on the normal approximation) 
  and \code{warn=T}, a warning is issued for cases when the normal approximation to the 
  binomial distribution probably is not accurate.  \cr
  When \code{approx=FALSE} (sample size based on the exact test) and \code{warn=TRUE}, a warning is issued when 
  the user-supplied sample size is too small to yield a significance level less than or 
  equal to the user-supplied value of \code{alpha}.
}
  \item{return.exact.list}{
  logical scalar relevant to the case when \code{approx=FALSE} 
  (i.e., when the power is based on the exact test).  This argument indicates whether 
  to return a list containing extra information about the exact test in addition to 
  the power of the exact test.  By default, \code{propTestN} returns only a vector 
  containing the computed sample size(s) (see the VALUE section below). When \cr
  \code{return.exact.list=TRUE} (the default) and \code{approx=FALSE}, \cr
  \code{propTestN} returns a list with components indicating the required sample size, 
  power of the exact test, the true significance level associated with the exact test, and the 
  critical values associated with the exact test (see the DETAILS section for more information).
}
  \item{n.min}{
  integer relevant to the case when \code{approx=FALSE} 
  (i.e., when the power is based on the exact test).  This argument indicates the minimum 
  allowed value for n to use in the search algorithm.  The default value is \code{n.min=2}.
}
  \item{n.max}{
  integer relevant to the case when \code{approx=FALSE} 
  (i.e., when the power is based on the exact test).  This argument indicates the maximum 
  allowed value for n to use in the search algorithm.  The default value is \code{n.max=10000}.
}
  \item{tol.alpha}{
  numeric vector relevant to the case when \code{approx=FALSE} 
  (i.e., when the power is based on the exact test).  This argument indicates the tolerance 
  on \code{alpha} to use in the search algorithm (i.e., how close the actual Type 
  I error level is to the value prescribed by \code{alpha}).  The default value is \code{tol.alpha=0.1*alpha}.
}
  \item{tol}{
  numeric scalar relevant to the case when \code{approx=FALSE} 
  (i.e., when the power is based on the exact test).  This argument is passed to the 
  \code{\link{uniroot}} function and indicates the tolerance to use in the search algorithm.  
  The default value is \code{tol=1e-7}.
}  \item{maxiter}{
  integer relevant to the case when \code{approx=FALSE} 
  (i.e., when the power is based on the exact test).  This argument is passed to the 
  \code{\link{uniroot}} function and indicates the maximum number of iterations to use 
  in the search algorithm.  The default value is \code{maxiter=1000}.
}
}
\details{
  If the arguments \code{p.or.p1}, \code{p0.or.p2}, \code{alpha}, \code{power}, \code{ratio}, 
  and \code{tol.alpha} are not all the same length, they are replicated to be the same length 
  as the length of the longest argument.

  The computed sample size is based on the difference \code{p.or.p1 - p0.or.p2}.

  \strong{One-Sample Case (\code{sample.type="one.sample"})}.  
  \describe{

  \item{\code{approx=TRUE}.}{ When \code{sample.type="one.sample"} and \code{approx=TRUE}, 
  sample size is computed based on the test that uses the normal approximation to the 
  binomial distribution; see the help file for \code{\link{prop.test}}.  
  The formula for this test and the associated power is presented in 
  standard statistics texts, including Zar (2010, pp. 534-537, 539-541).  
  These equations can be inverted to solve for the sample size, given a specified power, 
  significance level, hypothesized proportion, and true proportion.
  }

  \item{\code{approx=FALSE}.}{ When \code{sample.type="one.sample"} and \code{approx=FALSE}, 
  sample size is computed based on the exact binomial test; see the help file for \code{\link{binom.test}}.  
  The formula for this test and its associated power is presented in standard statistics texts, 
  including Zar (2010, pp. 532-534, 539) and 
  Millard and Neerchal (2001, pp. 385-386, 504-506).  The formula for the power involves 
  five quantities: the hypothesized proportion (\eqn{p_0}), the true proportion (\eqn{p}), 
  the significance level (\eqn{alpha}), the power, and the sample size (\eqn{n}).  
  In this case the function \code{propTestN} uses a search algorithm to determine the 
  required sample size to attain a specified power, given the values of the 
  hypothesized and true proportions and the significance level.
  }

  }

 \strong{Two-Sample Case (\code{sample.type="two.sample"})}. 

  When \code{sample.type="two.sample"}, sample size is computed based on the test that uses the 
  normal approximation to the binomial distribution; 
  see the help file for \code{\link{prop.test}}.  
  The formula for this test and its associated power is presented in standard statistics texts, 
  including Zar (2010, pp. 549-550, 552-553) and 
  Millard and Neerchal (2001, pp. 443-445, 508-510).  
  These equations can be inverted to solve for the sample size, given a specified power, 
  significance level, true proportions, and ratio of sample size in group 2 to sample size in 
  group 1.
}
\value{

  \strong{Approximate Test (\code{approx=TRUE})}.

  When \code{sample.type="one.sample"}, or \code{sample.type="two.sample"} 
  and \code{ratio=1} (i.e., equal sample sizes for each group), \code{propTestN} 
  returns a numeric vector of sample sizes.  When \cr
  \code{sample.type="two.sample"} and at least one element of \code{ratio} is 
  greater than 1, \code{propTestN} returns a list with two components called 
  \code{n1} and \code{n2}, specifying the sample sizes for each group.


  \strong{Exact Test (\code{approx=FALSE})}.

  If \code{return.exact.list=FALSE}, \code{propTestN} returns a numeric vector of sample sizes.

  If \code{return.exact.list=TRUE}, \code{propTestN} returns a list with the following components:

  \item{n}{numeric vector of sample sizes.}
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
  The functions \code{\link{propTestPower}}, \code{propTestN}, \code{\link{propTestMdd}}, and 
  \code{\link{plotPropTestDesign}} can be used to investigate these relationships for the case of 
  binomial proportions.

  Studying the two-sample proportion test, Haseman (1978) found that the formulas used to estimate the 
  power that do not incorporate the continuity correction tend to underestimate the power.  
  Casagrande, Pike, and Smith (1978) found that the formulas that do incorporate the continuity 
  correction provide an excellent approximation.
}
\seealso{
  \code{\link{propTestPower}}, \code{\link{propTestMdd}}, \code{\link{plotPropTestDesign}}, 
  \code{\link{prop.test}}, \code{\link{binom.test}}.
}
\examples{
  # Look at how the required sample size of the one-sample 
  # proportion test with a two-sided alternative and Type I error
  # set to 5\% increases with increasing power:

  seq(0.5, 0.9, by = 0.1) 
  #[1] 0.5 0.6 0.7 0.8 0.9 

  propTestN(p.or.p1 = 0.7, p0.or.p2 = 0.5, 
    power = seq(0.5, 0.9, by = 0.1)) 
  #[1] 25 31 38 47 62

  #----------

  # Repeat the last example, but compute the sample size based on 
  # the exact test instead of the approximation.  Note that because
  # we require the actual Type I error (alpha) to be within 
  # 10\% of the supplied value of alpha (which is 0.05 by default),
  # due to the discrete nature of the exact binomial test 
  # we end up with more power then we specified.

  n.list <- propTestN(p.or.p1 = 0.7,  p0.or.p2 = 0.5, 
    power = seq(0.5, 0.9, by = 0.1), approx = FALSE) 

  lapply(n.list, round, 3) 
  #$n
  #[1] 37 37 44 51 65
  #
  #$power
  #[1] 0.698 0.698 0.778 0.836 0.910
  #
  #$alpha
  #[1] 0.047 0.047 0.049 0.049 0.046
  #
  #$q.critical.lower
  #[1] 12 12 15 18 24
  #
  #$q.critical.upper
  #[1] 24 24 28 32 40

  #----------

  # Using the example above, see how the sample size changes 
  # if we allow the Type I error to deviate by more than 10 percent 
  # of the value of alpha (i.e., by more than 0.005).  

  n.list <- propTestN(p.or.p1 = 0.7,  p0.or.p2 = 0.5, 
    power = seq(0.5, 0.9, by = 0.1), approx = FALSE, tol.alpha = 0.01) 

  lapply(n.list, round, 3)
  #$n
  #[1] 25 35 42 49 65
  #
  #$power
  #[1] 0.512 0.652 0.743 0.810 0.910
  #
  #$alpha
  #[1] 0.043 0.041 0.044 0.044 0.046
  #
  #$q.critical.lower
  #[1]  7 11 14 17 24
  #
  #$q.critical.upper
  #[1] 17 23 27 31 40

  #----------
  
  # Clean up
  #---------
  rm(n.list)

  #==========

  # Look at how the required sample size for the two-sample 
  # proportion test decreases with increasing difference between 
  # the two population proportions:

  seq(0.4, 0.1, by = -0.1) 
  #[1] 0.4 0.3 0.2 0.1 

  propTestN(p.or.p1 = seq(0.4, 0.1, by = -0.1), 
    p0.or.p2 = 0.5, sample.type = "two") 
  #[1] 661 163 70 36 
  #Warning message:
  #In propTestN(p.or.p1 = seq(0.4, 0.1, by = -0.1), p0.or.p2 = 0.5,  :
  #  The computed sample sizes 'n1' and 'n2' are too small, 
  #  relative to the given values of 'p1' and 'p2', for the normal 
  #  approximation to work well for the following element indices:
  #         4 
   
  #----------

  # Look at how the required sample size for the two-sample 
  # proportion test decreases with increasing values of Type I error:

  propTestN(p.or.p1 = 0.7, p0.or.p2 = 0.5, 
    sample.type = "two", 
    alpha = c(0.001, 0.01, 0.05, 0.1)) 
  #[1] 299 221 163 137

  #==========

  # Modifying the example on pages 8-5 to 8-7 of USEPA (1989b), 
  # determine the required sample size to detect a difference in the 
  # proportion of detects of cadmium between the background and 
  # compliance wells. Set the complicance well to "group 1" and 
  # the backgound well to "group 2".  Assume the true probability 
  # of a "detect" at the background well is 1/3, set the probability 
  # of a "detect" at the compliance well to 0.4 and 0.5, use a 5% 
  # significance level and 95% power, and use the upper 
  # one-sided alternative (probability of a "detect" at the compliance 
  # well is greater than the probability of a "detect" at the background 
  # well).  (The original data are stored in EPA.89b.cadmium.df.) 
  #
  # Note that the required sample size decreases from about 
  # 1160 at each well to about 200 at each well as the difference in 
  # proportions changes from (0.4 - 1/3) to (0.5 - 1/3), but both of 
  # these sample sizes are enormous compared to the number of samples 
  # usually collected in the field.

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

  propTestN(p.or.p1 = c(0.4, 0.50), p0.or.p2 = p.hat.back, 
    alt="greater", sample.type="two") 
  #[1] 1159 199

  #----------

  # Clean up
  #---------
  rm(p.hat.back, p.hat.comp, n.back, n.comp) 
}
\keyword{ design }
\keyword{ htest }
\keyword{ models }

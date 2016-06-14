\name{tTestLnormAltN}
\alias{tTestLnormAltN}
\title{
  Sample Size for a One- or Two-Sample t-Test, Assuming Lognormal Data
}
\description{
  Compute the sample size necessary to achieve a specified power for a one- or 
  two-sample t-test, given the ratio of means, coefficient of variation, and 
  significance level, assuming lognormal data.
}
\usage{
  tTestLnormAltN(ratio.of.means, cv = 1, alpha = 0.05, power = 0.95, 
    sample.type = ifelse(!is.null(n2), "two.sample", "one.sample"), 
    alternative = "two.sided", approx = FALSE, n2 = NULL, round.up = TRUE, 
    n.max = 5000, tol = 1e-07, maxiter = 1000)
}
\arguments{
  \item{ratio.of.means}{
  numeric vector specifying the ratio of the first mean to the second mean.  
  When \code{sample.type="one.sample"}, this is the ratio of the population mean to the 
  hypothesized mean.  When \code{sample.type="two.sample"}, this is the ratio of the 
  mean of the first population to the mean of the second population.  The default 
  value is \code{ratio.of.means=1}.
}
  \item{cv}{
  numeric vector of positive value(s) specifying the coefficient of 
  variation.  When \code{sample.type="one.sample"}, this is the population coefficient 
  of variation.  When \code{sample.type="two.sample"}, this is the coefficient of 
  variation for both the first and second population.  The default value is \code{cv=1}.
}
  \item{alpha}{
  numeric vector of numbers between 0 and 1 indicating the Type I error level 
  associated with the hypothesis test.  The default value is \code{alpha=0.05}.
}
  \item{power}{
  numeric vector of numbers between 0 and 1 indicating the power  
  associated with the hypothesis test.  The default value is \code{power=0.95}.
}
  \item{sample.type}{
  character string indicating whether to compute power based on a one-sample or 
  two-sample hypothesis test.  When \code{sample.type="one.sample"}, the computed 
  power is based on a hypothesis test for a single mean.  When \cr
  \code{sample.type="two.sample"}, the computed power is based on a hypothesis test 
  for the difference between two means.  The default value is \cr
  \code{sample.type="one.sample"} unless the argument \code{n2} is supplied.
}
  \item{alternative}{
  character string indicating the kind of alternative hypothesis.  The possible values 
  are \code{"two.sided"} (the default), \code{"greater"}, and \code{"less"}.
}
  \item{approx}{
  logical scalar indicating whether to compute the power based on an approximation to 
  the non-central t-distribution.  The default value is \code{FALSE}.
}
  \item{n2}{
  numeric vector of sample sizes for group 2.  The default value is 
  \code{NULL} in which case it is assumed that the sample sizes for groups 
  1 and 2 are equal.  
  This argument is ignored when \code{sample.type="one.sample"}. 
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are \bold{\emph{not}} allowed.
}
  \item{round.up}{
  logical scalar indicating whether to round up the values of the computed 
  sample size(s) to the next smallest integer.  The default value is 
  \code{TRUE}.
}
  \item{n.max}{
  positive integer greater than 1 indicating the maximum sample size when \cr
  \code{sample.type="one.sample"} or the maximum sample size for group 1 
  when \code{sample.type="two.sample"}.  The default value is \code{n.max=5000}.
}
  \item{tol}{
  numeric scalar indicating the toloerance to use in the 
  \code{\link{uniroot}} search algorithm.  
  The default value is \code{tol=1e-7}.
}
  \item{maxiter}{
  positive integer indicating the maximum number of iterations 
  argument to pass to the \code{\link{uniroot}} function.  The default 
  value is \code{maxiter=1000}.
}
}
\details{
  If the arguments \code{ratio.of.means}, \code{cv}, \code{alpha}, \code{power}, and 
  \code{n2} are not all the same length, they are replicated to be the same length as 
  the length of the longest argument.

  Formulas for the power of the t-test for lognormal data for specified values of 
  the sample size, ratio of means, and Type I error level are given in 
  the help file for \code{\link{tTestLnormAltPower}}.  The function 
  \code{tTestLnormAltN} uses the \code{\link{uniroot}} search algorithm to determine 
  the required sample size(s) for specified values of the power, 
  scaled difference, and Type I error level. 
}
\value{
  When \code{sample.type="one.sample"}, or \code{sample.type="two.sample"} 
  and \code{n2} is not supplied (so equal sample sizes for each group is 
  assumed), \code{tTestLnormAltN} returns a numeric vector of sample sizes.  When 
  \code{sample.type="two.sample"} and \code{n2} is supplied, 
  \code{tTestLnormAltN} returns a list with two components called \code{n1} and 
  \code{n2}, specifying the sample sizes for each group.
}
\references{
  See \code{\link{tTestLnormAltPower}}.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See \code{\link{tTestLnormAltPower}}.
}
\seealso{
  \code{\link{tTestLnormAltPower}}, \code{\link{tTestLnormAltRatioOfMeans}}, 
  \code{\link{plotTTestLnormAltDesign}}, \link{LognormalAlt}, 
  \code{\link{t.test}}, \link{Hypothesis Tests}. 
}
\examples{
  # Look at how the required sample size for the one-sample test increases with 
  # increasing required power:

  seq(0.5, 0.9, by = 0.1) 
  # [1] 0.5 0.6 0.7 0.8 0.9 

  tTestLnormAltN(ratio.of.means = 1.5, power = seq(0.5, 0.9, by = 0.1)) 
  # [1] 19 23 28 36 47

  #----------

  # Repeat the last example, but compute the sample size based on the approximate 
  # power instead of the exact power:

  tTestLnormAltN(ratio.of.means = 1.5, power = seq(0.5, 0.9, by = 0.1), approx = TRUE) 
  # [1] 19 23 29 36 47

  #==========

  # Look at how the required sample size for the two-sample t-test decreases with 
  # increasing ratio of means:

  seq(1.5, 2, by = 0.1) 
  #[1] 1.5 1.6 1.7 1.8 1.9 2.0 

  tTestLnormAltN(ratio.of.means = seq(1.5, 2, by = 0.1), sample.type = "two") 
  #[1] 111  83  65  54  45  39

  #----------

  # Look at how the required sample size for the two-sample t-test decreases with 
  # increasing values of Type I error:

  tTestLnormAltN(ratio.of.means = 1.5, alpha = c(0.001, 0.01, 0.05, 0.1), 
    sample.type = "two") 
  #[1] 209 152 111  92

  #----------

  # For the two-sample t-test, compare the total sample size required to detect a 
  # ratio of means of 2 for equal sample sizes versus the case when the sample size 
  # for the second group is constrained to be 30.  Assume a coefficient of variation 
  # of 1, a 5% significance level, and 95% power.  Note that for the case of equal 
  # sample sizes, a total of 78 samples (39+39) are required, whereas when n2 is 
  # constrained to be 30, a total of 84 samples (54 + 30) are required.

  tTestLnormAltN(ratio.of.means = 2, sample.type = "two") 
  #[1] 39 

  tTestLnormAltN(ratio.of.means = 2, n2 = 30) 
  #$n1: 
  #[1] 54 
  #
  #$n2: 
  #[1] 30

  #==========

  # The guidance document Soil Screening Guidance: Technical Background Document 
  # (USEPA, 1996c, Part 4) discusses sampling design and sample size calculations 
  # for studies to determine whether the soil at a potentially contaminated site 
  # needs to be investigated for possible remedial action. Let 'theta' denote the 
  # average concentration of the chemical of concern.  The guidance document 
  # establishes the following goals for the decision rule (USEPA, 1996c, p.87):
  #
  #     Pr[Decide Don't Investigate | theta > 2 * SSL] = 0.05
  #
  #     Pr[Decide to Investigate | theta <= (SSL/2)] = 0.2
  #
  # where SSL denotes the pre-established soil screening level.
  #
  # These goals translate into a Type I error of 0.2 for the null hypothesis
  #
  #     H0: [theta / (SSL/2)] <= 1
  #
  # and a power of 95% for the specific alternative hypothesis
  #
  #     Ha: [theta / (SSL/2)] = 4
  #
  # Assuming a lognormal distribution and the above values for Type I error and 
  # power, determine the required samples sizes associated with various values of 
  # the coefficient of variation for the one-sample test.  Based on these calculations, 
  # you need to take at least 6 soil samples to satisfy the requirements for the 
  # Type I and Type II errors when the coefficient of variation is 2.

  cv <- c(0.5, 1, 2)
  N <- tTestLnormAltN(ratio.of.means = 4, cv = cv, alpha = 0.2, 
    alternative = "greater") 

  names(N) <- paste("CV=", cv, sep = "")
  N
  #CV=0.5   CV=1   CV=2 
  #     2      3      6 

  #----------

  # Repeat the last example, but use the approximate power calculation instead of the 
  # exact. Using the approximate power calculation, you need 7 soil samples when the 
  # coefficient of variation is 2 (because the approximation underestimates the 
  # true power).

  N <- tTestLnormAltN(ratio.of.means = 4, cv = cv, alpha = 0.2, 
    alternative = "greater", approx = TRUE) 

  names(N) <- paste("CV=", cv, sep = "")
  N
  #CV=0.5   CV=1   CV=2 
  #     3      5      7

  #----------

  # Repeat the last example, but use a Type I error of 0.05.

  N <- tTestLnormAltN(ratio.of.means = 4, cv = cv, alternative = "greater", 
    approx = TRUE) 

  names(N) <- paste("CV=", cv, sep = "")
  N
  #CV=0.5   CV=1   CV=2 
  #     4      6     12

  #==========

  # Reproduce the second column of Table 2 in van Belle and Martin (1993, p.167).

  tTestLnormAltN(ratio.of.means = 1.10, cv = seq(0.1, 0.8, by = 0.1), 
    power = 0.8, sample.type = "two.sample", approx = TRUE) 
  #[1]  19  69 150 258 387 533 691 856

  #==========

  # Clean up
  #---------
  rm(cv, N)
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }


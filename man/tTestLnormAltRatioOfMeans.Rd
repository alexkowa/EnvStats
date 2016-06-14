\name{tTestLnormAltRatioOfMeans}
\alias{tTestLnormAltRatioOfMeans}
\title{
  Minimal or Maximal Detectable Ratio of Means for One- or Two-Sample t-Test, Assuming Lognormal Data
}
\description{
  Compute the minimal or maximal detectable ratio of means associated with a one- or 
  two-sample t-test, given the sample size, coefficient of variation, significance 
  level, and power, assuming lognormal data.
}
\usage{
  tTestLnormAltRatioOfMeans(n.or.n1, n2 = n.or.n1, cv = 1, alpha = 0.05, power = 0.95, 
    sample.type = ifelse(!missing(n2), "two.sample", "one.sample"), 
    alternative = "two.sided", two.sided.direction = "greater", approx = FALSE, 
    tol = 1e-07, maxiter = 1000)
}
\arguments{
  \item{n.or.n1}{
  numeric vector of sample sizes.  When \code{sample.type="one.sample"}, 
  \code{n.or.n1} denotes \eqn{n}, the number of observations in the single sample.  When \cr
  \code{sample.type="two.sample"}, \code{n.or.n1} denotes \eqn{n_1}, the number 
  of observations from group 1.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are \bold{\emph{not}} allowed.
}
  \item{n2}{
  numeric vector of sample sizes for group 2.  The default value is the value of 
  \code{n.or.n1}. This argument is ignored when \code{sample.type="one.sample"}. 
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are \bold{\emph{not}} allowed.
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
  \item{two.sided.direction}{
  character string indicating the direction (greater than 1 or less than 1) for the 
  detectable ratio of means when \code{alternative="two.sided"}.  When \cr
  \code{two.sided.direction="greater"} (the default), the detectable ratio of means 
  is greater than 1.  When \code{two.sided.direction="less"}, the detectable ratio of 
  means is less than 1 (but greater than 0).  This argument is ignored if 
  \code{alternative="less"} or \code{alternative="greater"}.
}
  \item{approx}{
  logical scalar indicating whether to compute the power based on an approximation to 
  the non-central t-distribution.  The default value is \code{FALSE}.
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
  If the arguments \code{n.or.n1}, \code{n2}, \code{cv}, \code{alpha}, and 
  \code{power} are not all the same length, they are replicated to be the same length 
  as the length of the longest argument.

  Formulas for the power of the t-test for lognormal data for specified values of 
  the sample size, ratio of means, and Type I error level are given in 
  the help file for \code{\link{tTestLnormAltPower}}.  The function 
  \code{tTestLnormAltRatioOfMeans} uses the \code{\link{uniroot}} search algorithm 
  to determine the required ratio of means for specified values of the power, 
  sample size, and Type I error level.
}
\value{
  a numeric vector of computed minimal or maximal detectable ratios of means.  When \cr
  \code{alternative="less"}, or \code{alternative="two.sided"} and 
  \code{two.sided.direction="less"}, the computed ratios are less than 1 
  (but greater than 0).  Otherwise, the ratios are greater than 1.
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
  \code{\link{tTestLnormAltPower}}, \code{\link{tTestLnormAltN}}, 
  \code{\link{plotTTestLnormAltDesign}}, \link{LognormalAlt}, 
  \code{\link{t.test}}, \link{Hypothesis Tests}. 
}
\examples{
  # Look at how the minimal detectable ratio of means for the one-sample t-test 
  # increases with increasing required power:

  seq(0.5, 0.9, by = 0.1) 
  #[1] 0.5 0.6 0.7 0.8 0.9 

  ratio.of.means <- tTestLnormAltRatioOfMeans(n.or.n1 = 20, 
    power = seq(0.5, 0.9, by = 0.1)) 

  round(ratio.of.means, 2) 
  #[1] 1.47 1.54 1.63 1.73 1.89

  #----------

  # Repeat the last example, but compute the minimal detectable ratio of means 
  # based on the approximate power instead of the exact:

  ratio.of.means <- tTestLnormAltRatioOfMeans(n.or.n1 = 20, 
    power = seq(0.5, 0.9, by = 0.1), approx = TRUE) 

  round(ratio.of.means, 2) 
  #[1] 1.48 1.55 1.63 1.73 1.89

  #==========

  # Look at how the minimal detectable ratio of means for the two-sample t-test 
  # decreases with increasing sample size:

  seq(10, 50, by = 10) 
  #[1] 10 20 30 40 50 

  ratio.of.means <- tTestLnormAltRatioOfMeans(seq(10, 50, by = 10), sample.type="two") 

  round(ratio.of.means, 2) 
  #[1] 4.14 2.65 2.20 1.97 1.83

  #----------

  # Look at how the minimal detectable ratio of means for the two-sample t-test 
  # decreases with increasing values of Type I error:

  ratio.of.means <- tTestLnormAltRatioOfMeans(n.or.n1 = 20, 
    alpha = c(0.001, 0.01, 0.05, 0.1), sample.type = "two") 

  round(ratio.of.means, 2) 
  #[1] 4.06 3.20 2.65 2.42

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
  # Assuming a lognormal distribution, the above values for Type I and power, and a 
  # coefficient of variation of 2, determine the minimal detectable increase above 
  # the soil screening level associated with various sample sizes for the one-sample 
  # test.  Based on these calculations, you need to take at least 6 soil samples to 
  # satisfy the requirements for the Type I and Type II errors when the coefficient 
  # of variation is 2.

  N <- 2:8
  ratio.of.means <- tTestLnormAltRatioOfMeans(n.or.n1 = N, cv = 2, alpha = 0.2, 
    alternative = "greater") 

  names(ratio.of.means) <- paste("N=", N, sep = "")
  round(ratio.of.means, 1) 
  # N=2  N=3  N=4  N=5  N=6  N=7  N=8 
  #19.9  7.7  5.4  4.4  3.8  3.4  3.1

  #----------

  # Repeat the last example, but use the approximate power calculation instead of 
  # the exact.  Using the approximate power calculation, you need 7 soil samples 
  # when the coefficient of variation is 2.  Note how poorly the approximation 
  # works in this case for small sample sizes!

  ratio.of.means <- tTestLnormAltRatioOfMeans(n.or.n1 = N, cv = 2, alpha = 0.2, 
    alternative = "greater", approx = TRUE) 

  names(ratio.of.means) <- paste("N=", N, sep = "")
  round(ratio.of.means, 1) 
  #  N=2   N=3   N=4   N=5   N=6   N=7   N=8 
  #990.8  18.5   8.3   5.7   4.6   3.9   3.5

  #==========

  # Clean up
  #---------
  rm(ratio.of.means, N)
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }


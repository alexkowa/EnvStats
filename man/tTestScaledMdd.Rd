\name{tTestScaledMdd}
\alias{tTestScaledMdd}
\title{
  Scaled Minimal Detectable Difference for One- or Two-Sample t-Test
}
\description{
  Compute the scaled minimal detectable difference necessary to achieve a 
  specified power for a one- or two-sample t-test, given the sample size(s) 
  and Type I error level.
}
\usage{
  tTestScaledMdd(n.or.n1, n2 = n.or.n1, alpha = 0.05, power = 0.95, 
    sample.type = ifelse(!missing(n2) && !is.null(n2), "two.sample", "one.sample"), 
    alternative = "two.sided", two.sided.direction = "greater", 
    approx = FALSE, tol = 1e-07, maxiter = 1000)
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
  are: 

  \itemize{
    \item \code{"two.sided"} (the default).  \eqn{H_a: \mu \ne \mu_0} for the one-sample case and 
    \eqn{H_a: \mu_1 \ne \mu_2} for the two-sample case. 

    \item \code{"greater"}.  \eqn{H_a: \mu > \mu_0} for the one-sample case and 
    \eqn{H_a: \mu_1 > \mu_2} for the two-sample case. 

    \item \code{"less"}.  \eqn{H_a: \mu < \mu_0} for the one-sample case and 
    \eqn{H_a: \mu_1 < \mu_2} for the two-sample case.
  }

}
  \item{two.sided.direction}{
  character string indicating the direction (positive or negative) for the 
  scaled minimal detectable difference when \code{alternative="two.sided"}.  When \cr
  \code{two.sided.direction="greater"} (the default), the scaled minimal 
  detectable difference is positive.  When \code{two.sided.direction="less"}, 
  the scaled minimal detectable difference is negative.  This argument 
  is ignored if \cr
  \code{alternative="less"} or \code{alternative="greater"}.
}
  \item{approx}{
  logical scalar indicating whether to compute the power based on an approximation to 
  the non-central t-distribution.  The default value is \code{FALSE}.
}
  \item{tol}{
  numeric scalar indicating the tolerance argument to pass to the  
  \code{\link{uniroot}} function.  
  The default value is \code{tol=1e-7}.
}
  \item{maxiter}{
  positive integer indicating the maximum number of iterations 
  argument to pass to the \code{\link{uniroot}} function.  The default 
  value is \code{maxiter=1000}.
}
}
\details{
  Formulas for the power of the t-test for specified values of 
  the sample size, scaled difference, and Type I error level are given in 
  the help file for \code{\link{tTestPower}}.  The function \code{tTestScaledMdd} 
  uses the \code{\link{uniroot}} search algorithm to determine the 
  required scaled minimal detectable difference for specified values of the 
  sample size, power, and Type I error level.
}
\value{
  numeric vector of scaled minimal detectable differences.
}
\references{
  See \code{\link{tTestPower}}.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See \code{\link{tTestPower}}.
}
\seealso{
  \code{\link{tTestPower}}, \code{\link{tTestAlpha}}, 
  \code{\link{tTestN}}, 
  \code{\link{plotTTestDesign}}, \link[stats]{Normal}, 
  \code{\link{t.test}}, \link{Hypothesis Tests}. 
}
\examples{
  # Look at how the scaled minimal detectable difference for the 
  # one-sample t-test increases with increasing required power:

  seq(0.5, 0.9, by = 0.1) 
  #[1] 0.5 0.6 0.7 0.8 0.9 

  scaled.mdd <- tTestScaledMdd(n.or.n1 = 20, power = seq(0.5,0.9,by=0.1)) 

  round(scaled.mdd, 2) 
  #[1] 0.46 0.52 0.59 0.66 0.76

  #----------

  # Repeat the last example, but compute the scaled minimal detectable 
  # differences based on the approximation to the power instead of the 
  # exact formula:

  scaled.mdd <- tTestScaledMdd(n.or.n1 = 20, power = seq(0.5, 0.9, by = 0.1), 
    approx = TRUE) 

  round(scaled.mdd, 2) 
  #[1] 0.47 0.53 0.59 0.66 0.76

  #==========

  # Look at how the scaled minimal detectable difference for the two-sample 
  # t-test decreases with increasing sample size:

  seq(10,50,by=10) 
  #[1] 10 20 30 40 50 

  scaled.mdd <- tTestScaledMdd(seq(10, 50, by = 10), sample.type = "two") 

  round(scaled.mdd, 2) 
  #[1] 1.71 1.17 0.95 0.82 0.73

  #----------

  # Look at how the scaled minimal detectable difference for the two-sample 
  # t-test decreases with increasing values of Type I error:

  scaled.mdd <- tTestScaledMdd(20, alpha = c(0.001, 0.01, 0.05, 0.1), 
    sample.type="two") 

  round(scaled.mdd, 2) 
  #[1] 1.68 1.40 1.17 1.06

  #==========

  # Modifying the example on pages 21-4 to 21-5 of USEPA (2009), 
  # determine the minimal mean level of aldicarb at the third compliance 
  # well necessary to detect a mean level of aldicarb greater than the 
  # MCL of 7 ppb, assuming 90%, 95%, and 99% power.  Use a 99% significance 
  # level and assume an upper one-sided alternative (third compliance well 
  # mean larger than 7).  Use the estimated standard deviation from the 
  # first four months of data to estimate the true population standard 
  # deviation in order to determine the minimal detectable difference based 
  # on the computed scaled minimal detectable difference, then use this 
  # minimal detectable difference to determine the mean level of aldicarb 
  # necessary to detect a difference.  (The data are stored in 
  # EPA.09.Ex.21.1.aldicarb.df.) 
  #
  # Note that the scaled minimal detectable difference changes from 3.4 to 
  # 3.9 to 4.7 as the power changes from 90% to 95% to 99%.  Thus, the 
  # minimal detectable difference changes from 7.2 to 8.1 to 9.8, and the 
  # minimal mean level of aldicarb changes from 14.2 to 15.1 to 16.8.

  EPA.09.Ex.21.1.aldicarb.df
  #   Month   Well Aldicarb.ppb
  #1      1 Well.1         19.9
  #2      2 Well.1         29.6
  #3      3 Well.1         18.7
  #4      4 Well.1         24.2
  #5      1 Well.2         23.7
  #6      2 Well.2         21.9
  #7      3 Well.2         26.9
  #8      4 Well.2         26.1
  #9      1 Well.3          5.6
  #10     2 Well.3          3.3
  #11     3 Well.3          2.3
  #12     4 Well.3          6.9

  sigma <- with(EPA.09.Ex.21.1.aldicarb.df, 
    sd(Aldicarb.ppb[Well == "Well.3"]))

  sigma
  #[1] 2.101388

  scaled.mdd <- tTestScaledMdd(n.or.n1 = 4, alpha = 0.01, 
    power = c(0.90, 0.95, 0.99), sample.type="one", alternative="greater") 

  scaled.mdd 
  #[1] 3.431501 3.853682 4.668749

  mdd <- scaled.mdd * sigma 
  mdd 
  #[1] 7.210917 8.098083 9.810856

  minimal.mean <- mdd + 7 

  minimal.mean 
  #[1] 14.21092 15.09808 16.81086


  #==========

  # Clean up
  #---------
  rm(scaled.mdd, sigma, mdd, minimal.mean)
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }

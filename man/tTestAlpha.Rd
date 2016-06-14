\name{tTestAlpha}
\alias{tTestAlpha}
\title{
  Type I Error Level for a One- or Two-Sample t-Test
}
\description{
  Compute the Type I Error level necessary to achieve a specified power for a one- 
  or two-sample t-test, given the sample size(s) and scaled difference.
}
\usage{
  tTestAlpha(n.or.n1, n2 = n.or.n1, delta.over.sigma = 0, power = 0.95, 
    sample.type = ifelse(!missing(n2) && !is.null(n2), "two.sample", "one.sample"), 
    alternative = "two.sided", approx = FALSE, tol = 1e-07, maxiter = 1000)
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
  \item{delta.over.sigma}{
  numeric vector specifying the ratio of the true difference (\eqn{\delta}) to the 
  population standard deviation (\eqn{\sigma}).  This is also called the 
  \dQuote{scaled difference}.
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
  \item{tol}{
  numeric scalar indicating the tolerance argument to pass to the  
  \code{\link{uniroot}} function.  
  The default value is \code{tol=1e-7}.
}
  \item{maxiter}{
  positive integer indicating the maximum number of iterations 
  argument to pass to the \code{\link{uniroot}} function.  The default value is 
  \code{maxiter=1000}.
}
}
\details{
  Formulas for the power of the t-test for specified values of 
  the sample size, scaled difference, and Type I error level are given in 
  the help file for \code{\link{tTestPower}}.  The function \code{tTestAlpha} 
  uses the \code{\link{uniroot}} search algorithm to determine the 
  required Type I error level for specified values of the sample size, power, 
  and scaled difference.
}
\value{
  numeric vector of Type I error levels.
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
  \code{\link{tTestPower}}, \code{\link{tTestScaledMdd}}, 
  \code{\link{tTestN}}, 
  \code{\link{plotTTestDesign}}, \link[stats]{Normal}, 
  \code{\link{t.test}}, \link{Hypothesis Tests}. 
}
\examples{
  # Look at how the required Type I error level for the one-sample t-test 
  # decreases with increasing sample size.  Set the power to 80% and 
  # the scaled difference to 0.5.

  seq(5, 30, by = 5) 
  #[1] 5 10 15 20 25 30 

  alpha <- tTestAlpha(n.or.n1 = seq(5, 30, by = 5), 
    power = 0.8, delta.over.sigma = 0.5) 

  round(alpha, 2) 
  #[1] 0.65 0.45 0.29 0.18 0.11 0.07


  #----------


  # Repeat the last example, but use the approximation.
  # Note how the approximation underestimates the power 
  # for the smaller sample sizes.
  #----------------------------------------------------

  alpha <- tTestAlpha(n.or.n1 = seq(5, 30, by = 5), 
    power = 0.8, delta.over.sigma = 0.5, approx = TRUE) 

  round(alpha, 2)
  #[1] 0.63 0.46 0.30 0.18 0.11 0.07


  #----------


  # Look at how the required Type I error level for the two-sample 
  # t-test decreases with increasing scaled difference.  Use 
  # a power of 90% and a sample size of 10 in each group.

  seq(0.5, 2, by = 0.5) 
  #[1] 0.5 1.0 1.5 2.0 

  alpha <- tTestAlpha(10, sample.type = "two.sample", 
    power = 0.9, delta.over.sigma = seq(0.5, 2, by = 0.5)) 

  round(alpha, 2) 
  #[1] 0.82 0.35 0.06 0.01


  #----------


  # Look at how the required Type I error level for the two-sample 
  # t-test increases with increasing values of required power.  Use 
  # a sample size of 20 for each group and a scaled difference of 
  # 1.

  alpha <- tTestAlpha(20, sample.type = "two.sample", delta.over.sigma = 1, 
    power = c(0.8, 0.9, 0.95)) 

  round(alpha, 2)
  #[1] 0.03 0.07 0.14
 

  #----------


  # Clean up
  #---------
  rm(alpha)
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }
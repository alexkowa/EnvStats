\name{tTestLnormAltPower}
\alias{tTestLnormAltPower}
\title{
  Power of a One- or Two-Sample t-Test Assuming Lognormal Data
}
\description{
  Compute the power of a one- or two-sample t-test, given the sample size, 
  ratio of means, coefficient of variation, and significance level, assuming 
  lognormal data.
}
\usage{
  tTestLnormAltPower(n.or.n1, n2 = n.or.n1, ratio.of.means = 1, cv = 1, alpha = 0.05, 
    sample.type = ifelse(!missing(n2), "two.sample", "one.sample"), 
    alternative = "two.sided", approx = FALSE)
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
}
\details{
  If the arguments \code{n.or.n1}, \code{n2}, \code{ratio.of.means}, \code{cv}, and 
  \code{alpha} are not all the same length, they are replicated to be the same length 
  as the length of the longest argument.

  \emph{One-Sample Case} (\code{sample.type="one.sample"}) \cr
  Let \eqn{\underline{x} = x_1, x_2, \ldots, x_n} denote a vector of \eqn{n} 
  observations from a \link[=LognormalAlt]{lognormal distribution} with mean 
  \eqn{\theta} and coefficient of variation \eqn{\tau}, and consider the null hypothesis:
  \deqn{H_0: \theta = \theta_0 \;\;\;\;\;\; (1)}
  The three possible alternative hypotheses are the upper one-sided alternative 
  (\code{alternative="greater"}):
  \deqn{H_a: \theta > \theta_0 \;\;\;\;\;\; (2)}
  the lower one-sided alternative (\code{alternative="less"})
  \deqn{H_a: \theta < \theta_0 \;\;\;\;\;\; (3)}
  and the two-sided alternative (\code{alternative="two.sided"})
  \deqn{H_a: \theta \ne \theta_0 \;\;\;\;\;\; (4)}
  To test the null hypothesis (1) versus any of the three alternatives (2)-(4), one 
  might be tempted to use \link[stats:t.test]{Student's t-test} based on the 
  log-transformed observations.  Unlike the two-sample case with equal coefficients of 
  variation (see below), in the one-sample case Student's t-test applied to the 
  log-transformed observations will not test the correct hypothesis, as now explained.

  Let
  \deqn{y_i = log(x_i), \;\; i = 1, 2, \ldots, n \;\;\;\;\;\; (5)}
  Then \eqn{\underline{y} = y_1, y_2, \ldots, y_n} denote \eqn{n} observations from a 
  normal distribution with mean \eqn{\mu} and standard deviation \eqn{\sigma}, where
  \deqn{\mu = log(\frac{\theta}{\sqrt{\tau^2 + 1}}) \;\;\;\;\;\; (6)}
  \deqn{\sigma = [log(\tau^2 + 1)]^{1/2} \;\;\;\;\;\; (7)}
  \deqn{\theta = exp[\mu + (\sigma^2/2)] \;\;\;\;\;\; (8)}
  \deqn{\tau = [exp(\sigma^2) - 1]^{1/2} \;\;\;\;\;\; (9)}
  (see the help file for \link{LognormalAlt}).  Hence, by Equations (6) and (8) above, 
  the Student's t-test on the log-transformed data would involve a test of hypothesis 
  on both the parameters \eqn{\theta} and \eqn{\tau}, not just on \eqn{\theta}.

  To test the null hypothesis (1) above versus any of the alternatives (2)-(4), you 
  can use the function \code{\link{elnormAlt}} to compute a confidence interval for 
  \eqn{\theta}, and use the relationship between confidence intervals and hypothesis 
  tests.  To test the null hypothesis (1) above versus the upper one-sided alternative 
  (2), you can also use 
  \link[=chenTTest]{Chen's modified t-test for skewed distributions}.

  Although you can't use Student's t-test based on the log-transformed observations to 
  test a hypothesis about \eqn{\theta}, you can use the t-distribution to estimate the 
  power of a test about \eqn{\theta} that is based on confidence intervals or 
  Chen's modified t-test, if you are willing to assume the population coefficient of 
  variation \eqn{\tau} stays constant for all possible values of \eqn{\theta} you are 
  interested in, and you are willing to postulate possible values for \eqn{\tau}.

  First, let's re-write the hypotheses (1)-(4) as follows.  The null hypothesis (1) 
  is equivalent to:
  \deqn{H_0: \frac{\theta}{\theta_0} = 1 \;\;\;\;\;\; (10)}
  The three possible alternative hypotheses are the upper one-sided alternative 
  (\code{alternative="greater"})
  \deqn{H_a: \frac{\theta}{\theta_0} > 1 \;\;\;\;\;\; (11)}
  the lower one-sided alternative (\code{alternative="less"})
  \deqn{H_a: \frac{\theta}{\theta_0} < 1 \;\;\;\;\;\; (12)}
  and the two-sided alternative (\code{alternative="two.sided"})
  \deqn{H_a: \frac{\theta}{\theta_0} \ne 1 \;\;\;\;\;\; (13)}
  For a constant coefficient of variation \eqn{\tau}, the standard deviation of the 
  log-transformed observations \eqn{\sigma} is also constant (see Equation (7) above).  
  Hence, by Equation (8), the ratio of the true mean to the hypothesized mean can be 
  written as:
  \deqn{R = \frac{\theta}{\theta_0} = \frac{exp[\mu + (\sigma^2/2)]}{exp[\mu_0 + (\sigma^2/2)]} = \frac{e^\mu}{e^\mu_0} = e^{\mu - \mu_0} \;\;\;\;\;\; (14)}
  which only involves the difference
  \deqn{\mu - \mu_0 \;\;\;\;\;\; (15)}
  Thus, for given values of \eqn{R} and \eqn{\tau}, the power of the test of the null 
  hypothesis (10) against any of the alternatives (11)-(13) can be computed based on 
  the power of a one-sample t-test with
  \deqn{\frac{\delta}{\sigma} = \frac{log(R)}{\sqrt{log(\tau^2 + 1)}} \;\;\;\;\;\; (16)}
  (see the help file for \code{\link{tTestPower}}).  Note that for the function 
  \code{tTestLnormAltPower}, \eqn{R} corresponds to the argument \code{ratio.of.means}, 
  and \eqn{\tau} corresponds to the argument \code{cv}.
  \cr
 
  \emph{Two-Sample Case} (\code{sample.type="two.sample"}) \cr
  Let \eqn{\underline{x}_1 = x_{11}, x_{12}, \ldots, x_{1n_1}} denote a vector of 
  \eqn{n_1} observations from a \link[=LognormalAlt]{lognormal distribution} with mean 
  \eqn{\theta_1} and coefficient of variaiton \eqn{\tau}, and let 
  \eqn{\underline{x}_2 = x_{21}, x_{22}, \ldots, x_{2n_2}} denote a vector of 
  \eqn{n_2} observations from a lognormal distribution with mean \eqn{\theta_2} and 
  coefficient of variation \eqn{\tau}, and consider the null hypothesis:
  \deqn{H_0: \theta_1 = \theta_2 \;\;\;\;\;\; (17)}
  The three possible alternative hypotheses are the upper one-sided alternative 
  (\code{alternative="greater"}):
  \deqn{H_a: \theta_1 > \theta_2 \;\;\;\;\;\; (18)}
  the lower one-sided alternative (\code{alternative="less"})
  \deqn{H_a: \theta_1 < \theta_2 \;\;\;\;\;\; (19)}
  and the two-sided alternative (\code{alternative="two.sided"})
  \deqn{H_a: \theta_1 \ne \theta_2 \;\;\;\;\;\; (20)}
  Because we are assuming the coefficient of variation \eqn{\tau} is the same for 
  both populations, the test of the null hypothesis (17) versus any of the three 
  alternatives (18)-(20) can be based on the Student t-statistic using the 
  log-transformed observations.

  To show this, first, let's re-write the hypotheses (17)-(20) as follows.  The 
  null hypothesis (17) is equivalent to:
  \deqn{H_0: \frac{\theta_1}{\theta_2} = 1 \;\;\;\;\;\; (21)}
  The three possible alternative hypotheses are the upper one-sided alternative 
  (\code{alternative="greater"})
  \deqn{H_a: \frac{\theta_1}{\theta_2} > 1 \;\;\;\;\;\; (22)}
  the lower one-sided alternative (\code{alternative="less"})
  \deqn{H_a: \frac{\theta_1}{\theta_2} < 1 \;\;\;\;\;\; (23)}
  and the two-sided alternative (\code{alternative="two.sided"})
  \deqn{H_a: \frac{\theta_1}{\theta_2} \ne 1 \;\;\;\;\;\; (24)}
  If coefficient of variation \eqn{\tau} is the same for both populations, then the 
  standard deviation of the log-transformed observations \eqn{\sigma} is also the 
  same for both populations (see Equation (7) above).  Hence, by Equation (8), the 
  ratio of the means can be written as:
  \deqn{R = \frac{\theta_1}{\theta_2} = \frac{exp[\mu_1 + (\sigma^2/2)]}{exp[\mu_2 + (\sigma^2/2)]} = \frac{e^\mu_1}{e^\mu_2} = e^{\mu_1 - \mu_2} \;\;\;\;\;\; (25)}
  which only involves the difference
  \deqn{\mu_1 - \mu_2 \;\;\;\;\;\; (26)}
  Thus, for given values of \eqn{R} and \eqn{\tau}, the power of the test of the null 
  hypothesis (21) against any of the alternatives (22)-(24) can be computed based on 
  the power of a two-sample t-test with
  \deqn{\frac{\delta}{\sigma} = \frac{log(R)}{\sqrt{log(\tau^2 + 1)}} \;\;\;\;\;\; (27)}
  (see the help file for \code{\link{tTestPower}}).  Note that for the function 
  \code{tTestLnormAltPower}, \eqn{R} corresponds to the argument \code{ratio.of.means}, 
  and \eqn{\tau} corresponds to the argument \code{cv}.
}
\value{
  a numeric vector powers.
}
\references{
  van Belle, G., and D.C. Martin. (1993).  Sample Size as a Function of Coefficient 
  of Variation and Ratio of Means.  \emph{The American Statistician} \bold{47}(3), 
  165--167.

  Also see the list of references in the help file for \code{\link{tTestPower}}.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The \link[stats:Normal]{normal distribution} and 
  \link[stats:Lognormal]{lognormal distribution} are probably the two most 
  frequently used distributions to model environmental data.  Often, you need to 
  determine whether a population mean is significantly different from a specified 
  standard (e.g., an MCL or ACL, USEPA, 1989b, Section 6), or whether two different 
  means are significantly different from each other (e.g., USEPA 2009, Chapter 16).  
  When you have lognormally-distributed data, you have to be careful about making 
  statements regarding inference for the mean.  For the two-sample case with 
  assumed equal coefficients of variation, you can perform the 
  \link[stats:t.test]{Student's t-test} on the log-transformed observations.  
  For the one-sample case, you can perform a hypothesis test by constructing a 
  confidence interval for the mean using \code{\link{elnormAlt}}, or use 
  \link[=chenTTest]{Chen's t-test modified for skewed data}.

  In the course of designing a sampling program, an environmental scientist may wish 
  to determine the relationship between sample size, significance level, power, and 
  scaled difference if one of the objectives of the sampling program is to determine 
  whether a mean differs from a specified level or two means differ from each other.  
  The functions \code{tTestLnormAltPower}, \code{\link{tTestLnormAltN}}, 
  \code{\link{tTestLnormAltRatioOfMeans}}, and \code{\link{plotTTestLnormAltDesign}} 
  can be used to investigate these relationships for the case of 
  lognormally-distributed observations.
}
\seealso{
  \code{\link{tTestLnormAltN}}, \code{\link{tTestLnormAltRatioOfMeans}}, 
  \code{\link{plotTTestLnormAltDesign}}, \link{LognormalAlt}, 
  \code{\link{t.test}}, \link{Hypothesis Tests}. 
}
\examples{
  # Look at how the power of the one-sample test increases with increasing 
  # sample size:

  seq(5, 30, by = 5) 
  #[1]  5 10 15 20 25 30 

  power <- tTestLnormAltPower(n.or.n1 = seq(5, 30, by = 5), 
    ratio.of.means = 1.5, cv = 1) 

  round(power, 2) 
  #[1] 0.14 0.28 0.42 0.54 0.65 0.73

  #----------

  # Repeat the last example, but use the approximation to the power instead of the 
  # exact power.  Note how the approximation underestimates the true power for 
  # the smaller sample sizes:

  power <- tTestLnormAltPower(n.or.n1 = seq(5, 30, by = 5), 
    ratio.of.means = 1.5, cv = 1, approx = TRUE) 

  round(power, 2) 
  #[1] 0.09 0.25 0.40 0.53 0.64 0.73

  #==========

  # Look at how the power of the two-sample t-test increases with increasing 
  # ratio of means:

  power <- tTestLnormAltPower(n.or.n1 = 20, sample.type = "two", 
    ratio.of.means = c(1.1, 1.5, 2), cv = 1) 

  round(power, 2) 
  #[1] 0.06 0.32 0.73

  #----------

  # Look at how the power of the two-sample t-test increases with increasing 
  # values of Type I error:

  power <- tTestLnormAltPower(30, sample.type = "two", ratio.of.means = 1.5, 
    cv = 1, alpha = c(0.001, 0.01, 0.05, 0.1)) 

  round(power, 2) 
  #[1] 0.07 0.23 0.46 0.59

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
  # Assuming a lognormal distribution with a coefficient of variation of 2, 
  # determine the power associated with various sample sizes for this one-sample test. 
  # Based on these calculations, you need to take at least 6 soil samples to 
  # satisfy the requirements for the Type I and Type II errors.

  power <- tTestLnormAltPower(n.or.n1 = 2:8, ratio.of.means = 4, cv = 2, 
    alpha = 0.2, alternative = "greater") 

  names(power) <- paste("N=", 2:8, sep = "")

  round(power, 2) 
  # N=2  N=3  N=4  N=5  N=6  N=7  N=8 
  #0.65 0.80 0.88 0.93 0.96 0.97 0.98

  #----------

  # Repeat the last example, but use the approximate power calculation instead of 
  # the exact one.  Using the approximate power calculation, you need at least 
  # 7 soil samples instead of 6 (because the approximation underestimates the power).

  power <- tTestLnormAltPower(n.or.n1 = 2:8, ratio.of.means = 4, cv = 2, 
    alpha = 0.2, alternative = "greater", approx = TRUE) 

  names(power) <- paste("N=", 2:8, sep = "")

  round(power, 2)
  # N=2  N=3  N=4  N=5  N=6  N=7  N=8 
  #0.55 0.75 0.84 0.90 0.93 0.95 0.97

  #==========

  # Clean up
  #---------
  rm(power)
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }

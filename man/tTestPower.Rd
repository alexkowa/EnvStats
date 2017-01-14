\name{tTestPower}
\alias{tTestPower}
\alias{t-test power}
\alias{T-test power}
\alias{t-test Power}
\alias{Power t-test}
\alias{power t-test}
\title{
  Power of a One- or Two-Sample t-Test
}
\description{
  Compute the power of a one- or two-sample t-test, given the sample size, scaled 
  difference, and significance level.
}
\usage{
  tTestPower(n.or.n1, n2 = n.or.n1, delta.over.sigma = 0, alpha = 0.05, 
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
  \item{delta.over.sigma}{
  numeric vector specifying the ratio of the true difference \eqn{\delta}  
  (\eqn{\delta = \mu - \mu_0} for the one-sample case and 
  \eqn{\delta = \mu_1 - \mu_2} for the two-sample case) 
  to the population standard deviation (\eqn{\sigma}).  This is also called the 
  \dQuote{scaled difference}.  

  The default value is \code{delta.over.sigma=0}.
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
  for the difference between two means.  The default value is 
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
  \item{approx}{
  logical scalar indicating whether to compute the power based on an approximation to 
  the non-central t-distribution.  The default value is \code{FALSE}.
}
}
\details{
  If the arguments \code{n.or.n1}, \code{n2}, \code{delta.over.sigma}, and 
  \code{alpha} are not all the same length, they are replicated to be the same 
  length as the length of the longest argument.

  \bold{One-Sample Case} (\code{sample.type="one.sample"}) \cr
  Let \eqn{\underline{x} = x_1, x_2, \ldots, x_n} denote a vector of \eqn{n} 
  observations from a \link[stats:Normal]{normal distribution} with mean \eqn{\mu} 
  and standard deviation \eqn{\sigma}, and consider the null hypothesis:
  \deqn{H_0: \mu = \mu_0 \;\;\;\;\;\; (1)}
  The three possible alternative hypotheses are the upper one-sided alternative 
  (\code{alternative="greater"}):
  \deqn{H_a: \mu > \mu_0 \;\;\;\;\;\; (2)}
  the lower one-sided alternative (\code{alternative="less"})
  \deqn{H_a: \mu < \mu_0 \;\;\;\;\;\; (3)}
  and the two-sided alternative (\code{alternative="two.sided"})
  \deqn{H_a: \mu \ne \mu_0 \;\;\;\;\;\; (4)}
  The test of the null hypothesis (1) versus any of the three alternatives (2)-(4) is 
  based on the \link[stats:t.test]{Student t-statistic}:
  \deqn{t = \frac{\bar{x} - \mu_0}{s/\sqrt{n}} \;\;\;\;\;\; (5)}
  where 
  \deqn{\bar{x} = \frac{1}{n} \sum_{i=1}^n x_i \;\;\;\;\;\; (6)}
  \deqn{s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\;\;\; (7)}
  Under the null hypothesis (1), the t-statistic in (5) follows a 
  \link[stats:TDist]{Student's t-distribution} with \eqn{n-1} degrees of freedom 
  (Zar, 2010, Chapter 7; Johnson et al., 1995, pp.362-363).

  The formula for the power of the test depends on which alternative is being tested.  
  The two subsections below describe exact and approximate formulas for the power of 
  the one-sample t-test.  Note that none of the equations for the power of the t-test 
  requires knowledge of the values \eqn{\delta} (Equation (12) below) or \eqn{\sigma} 
  (the population standard deviation), only the ratio \eqn{\delta/\sigma}.  The 
  argument \code{delta.over.sigma} is this ratio, and it is referred to as the 
  \dQuote{scaled difference}.
  \cr

  \bold{\emph{Exact Power Calculations}} (\code{approx=FALSE}) \cr
  This subsection describes the exact formulas for the power of the one-sample 
  t-test.
  \cr
 
  \emph{Upper one-sided alternative} (\code{alternative="greater"}) \cr
  The standard Student's t-test rejects the null hypothesis (1) in favor of the 
  upper alternative hypothesis (2) at level-\eqn{\alpha} if
  \deqn{t \ge t_{\nu}(1 - \alpha) \;\;\;\;\;\; (8)}
  where 
  \deqn{\nu = n - 1 \;\;\;\;\;\; (9)}
  and \eqn{t_{\nu}(p)} denotes the \eqn{p}'th quantile of Student's t-distribution 
  with \eqn{\nu} degrees of freedom (Zar, 2010; Berthouex and Brown, 2002).  
  The power of this test, denoted by \eqn{1-\beta}, where \eqn{\beta} denotes the 
  probability of a Type II error, is given by:
  \deqn{1 - \beta = Pr[t_{\nu, \Delta} \ge t_{\nu}(1 - \alpha)] = 1 - G[t_{\nu}(1 - \alpha), \nu, \Delta] \;\;\;\;\;\; (10)}
  where
  \deqn{\Delta = \sqrt{n} (\frac{\delta}{\sigma}) \;\;\;\;\;\; (11)}
  \deqn{\delta = \mu - \mu_0 \;\;\;\;\;\ (12)}
  and \eqn{t_{\nu, \Delta}} denotes a 
  \link[stats:TDist]{non-central Student's t-random variable} with 
  \eqn{\nu} degrees of freedom and non-centrality parameter \eqn{\Delta}, and 
  \eqn{G(x, \nu, \Delta)} denotes the cumulative distribution function of this 
  random variable evaluated at \eqn{x} (Johnson et al., 1995, pp.508-510).
  \cr

  \emph{Lower one-sided alternative} (\code{alternative="less"}) \cr
  The standard Student's t-test rejects the null hypothesis (1) in favor of the 
  lower alternative hypothesis (3) at level-\eqn{\alpha} if
  \deqn{t \le t_{\nu}(\alpha) \;\;\;\;\;\; (13)}
  and the power of this test is given by:
  \deqn{1 - \beta = Pr[t_{\nu, \Delta} \le t_{\nu}(\alpha)] = G[t_{\nu}(\alpha), \nu, \Delta] \;\;\;\;\;\; (14)}
  \cr

  \emph{Two-sided alternative} (\code{alternative="two.sided"}) \cr
  The standard Student's t-test rejects the null hypothesis (1) in favor of the 
  two-sided alternative hypothesis (4) at level-\eqn{\alpha} if
  \deqn{|t| \ge t_{\nu}(1 - \alpha/2) \;\;\;\;\;\; (15)}
  and the power of this test is given by:
  \deqn{1 - \beta = Pr[t_{\nu, \Delta} \le t_{\nu}(\alpha/2)] + Pr[t_{\nu, \Delta} \ge t_{\nu}(1 - \alpha/2)]}
  \deqn{= G[t_{\nu}(\alpha/2), \nu, \Delta] + 1 - G[t_{\nu}(1 - \alpha/2), \nu, \Delta] \;\;\;\;\;\; (16)}
  The power of the t-test given in Equation (16) can also be expressed in terms of the 
  cumulative distribution function of the \link[stats:FDist]{non-central F-distribution} 
  as follows. Let \eqn{F_{\nu_1, \nu_2, \Delta}} denote a 
  \link[stats:FDist]{non-central F random variable} with \eqn{\nu_1} and 
  \eqn{\nu_2} degrees of freedom and non-centrality parameter \eqn{\Delta}, and let 
  \eqn{H(x, \nu_1, \nu_2, \Delta)} denote the cumulative distribution function of this 
  random variable evaluated at \eqn{x}. Also, let \eqn{F_{\nu_1, \nu_2}(p)} denote 
  the \eqn{p}'th quantile of the central F-distribution with \eqn{\nu_1} and 
  \eqn{\nu_2} degrees of freedom.  It can be shown that
  \deqn{(t_{\nu, \Delta})^2 \cong F_{1, \nu, \Delta^2} \;\;\;\;\;\; (17)}
  where \eqn{\cong} denotes \dQuote{equal in distribution}.  Thus, it follows that
  \deqn{[t_{\nu}(1 - \alpha/2)]^2 = F_{1, \nu}(1 - \alpha) \;\;\;\;\;\; (18)}
  so the formula for the power of the t-test given in Equation (16) can also be 
  written as:
  \deqn{1 - \beta = Pr\{(t_{\nu, \Delta})^2  \ge [t_{\nu}(1 - \alpha/2)]^2\}}
  \deqn{= Pr[F_{1, \nu, \Delta^2} \ge F_{1, \nu}(1 - \alpha)] = 1 - H[F_{1, \nu}(1-\alpha), 1, \nu, \Delta^2] \;\;\;\;\;\; (19)}
  \cr
 
  \bold{\emph{Approximate Power Calculations}} (\code{approx=TRUE}) \cr
  Zar (2010, pp.115--118) presents an approximation to the power for the t-test 
  given in Equations (10), (14), and (16) above.  His approximation to the power 
  can be derived by using the approximation
  \deqn{\sqrt{n} (\frac{\delta}{s}) \approx \sqrt{n} (\frac{\delta}{\sigma}) = \Delta \;\;\;\;\;\; (20)}
  where \eqn{\approx} denotes \dQuote{approximately equal to}.  Zar's approximation 
  can be summarized in terms of the cumulative distribution function of the 
  non-central t-distribution as follows:
  \deqn{G(x, \nu, \Delta) \approx G(x - \Delta, \nu, 0) = G(x - \Delta, \nu) \;\;\;\;\;\; (21)}
  where \eqn{G(x, \nu)} denotes the cumulative distribution function of the 
  central Student's t-distribution with \eqn{\nu} degrees of freedom evaluated at 
  \eqn{x}.

  The following three subsections explicitly derive the approximation to the power of 
  the t-test for each of the three alternative hypotheses.
  \cr
 
  \emph{Upper one-sided alternative} (\code{alternative="greater"}) \cr
  The power for the upper one-sided alternative (2) given in Equation (10) can be 
  approximated as:
  \deqn{1 - \beta = Pr[t \ge t_{\nu}(1 - \alpha)]}
  \deqn{= Pr[\frac{\bar{x} - \mu}{s/\sqrt{n}} \ge t_{\nu}(1 - \alpha) - \sqrt{n}\frac{\delta}{s}]}
  \deqn{\approx Pr[t_{\nu} \ge t_{\nu}(1 - \alpha) - \Delta]}
  \deqn{= 1 - Pr[t_{\nu} \le t_{\nu}(1 - \alpha) - \Delta]}
  \deqn{ = 1 - G[t_{\nu}(1-\alpha) - \Delta, \nu] \;\;\;\;\;\; (22)}
  where \eqn{t_{\nu}} denotes a central Student's t-random variable with \eqn{\nu} 
  degrees of freedom.
  \cr

  \emph{Lower one-sided alternative} (\code{alternative="less"}) \cr
  The power for the lower one-sided alternative (3) given in Equation (14) can be 
  approximated as:
  \deqn{1 - \beta = Pr[t \le t_{\nu}(\alpha)]}
  \deqn{= Pr[\frac{\bar{x} - \mu}{s/\sqrt{n}} \le t_{\nu}(\alpha) - \sqrt{n}\frac{\delta}{s}]}
  \deqn{\approx Pr[t_{\nu} \le t_{\nu}(\alpha) - \Delta]}
  \deqn{ = G[t_{\nu}(\alpha) - \Delta, \nu] \;\;\;\;\;\; (23)}
  \cr
 
  \emph{Two-sided alternative} (\code{alternative="two.sided"}) \cr
  The power for the two-sided alternative (4) given in Equation (16) can be 
  approximated as:
  \deqn{1 - \beta = Pr[t \le t_{\nu}(\alpha/2)] + Pr[t \ge t_{\nu}(1 - \alpha/2)]}
  \deqn{= Pr[\frac{\bar{x} - \mu}{s/\sqrt{n}} \le t_{\nu}(\alpha/2) - \sqrt{n}\frac{\delta}{s}] + Pr[\frac{\bar{x} - \mu}{s/\sqrt{n}} \ge t_{\nu}(1 - \alpha) - \sqrt{n}\frac{\delta}{s}]}
  \deqn{\approx Pr[t_{\nu} \le t_{\nu}(\alpha/2) - \Delta] + Pr[t_{\nu} \ge t_{\nu}(1 - \alpha/2) - \Delta]}
  \deqn{= G[t_{\nu}(\alpha/2) - \Delta, \nu] + 1 - G[t_{\nu}(1-\alpha/2) - \Delta, \nu] \;\;\;\;\;\; (24)}
  \cr
 
  \bold{Two-Sample Case} (\code{sample.type="two.sample"}) \cr
  Let \eqn{\underline{x}_1 = x_{11}, x_{12}, \ldots, x_{1n_1}} denote a vector of 
  \eqn{n_1} observations from a \link[stats:Normal]{normal distribution} with mean 
  \eqn{\mu_1} and standard deviation \eqn{\sigma}, and let 
  \eqn{\underline{x}_2 = x_{21}, x_{22}, \ldots, x_{2n_2}} denote a vector of 
  \eqn{n_2} observations from a normal distribution with mean \eqn{\mu_2} and 
  standard deviation \eqn{\sigma}, and consider the null hypothesis:
  \deqn{H_0: \mu_1 = \mu_2 \;\;\;\;\;\; (25)}
  The three possible alternative hypotheses are the upper one-sided alternative 
  (\code{alternative="greater"}):
  \deqn{H_a: \mu_1 > \mu_2 \;\;\;\;\;\; (26)}
  the lower one-sided alternative (\code{alternative="less"})
  \deqn{H_a: \mu_1 < \mu_2 \;\;\;\;\;\; (27)}
  and the two-sided alternative (\code{alternative="two.sided"})
  \deqn{H_a: \mu_1 \ne \mu_2 \;\;\;\;\;\; (28)}
  The test of the null hypothesis (25) versus any of the three alternatives (26)-(28) 
  is based on the \link[stats:t.test]{Student t-statistic}:
  \deqn{t = \frac{\bar{x}_1 - \bar{x}_2}{s_p\sqrt{\frac{1}{n_1} + \frac{1}{n_2}}} \;\;\;\;\;\; (29)}
  where 
  \deqn{\bar{x}_1 = \frac{1}{n_1} \sum_{i=1}^{n_1} x_{1i} \;\;\;\;\;\; (30)}
  \deqn{\bar{x}_2 = \frac{1}{n_2} \sum_{i=1}^{n_2} x_{2i} \;\;\;\;\;\; (31)}
  \deqn{s_p^2 = \frac{(n_1 - 1) s_1^2 + (n_2 - 1) s_2^2}{n_1 + n_2 - 2} \;\;\;\;\;\; (32)}
  \deqn{s_1^2 = \frac{1}{n_1 - 1} \sum_{i=1}^{n_1} (x_{1i} - \bar{x}_1)^2 \;\;\;\;\;\; (33)}
  \deqn{s_2^2 = \frac{1}{n_2 - 1} \sum_{i=1}^{n_2} (x_{2i} - \bar{x}_2)^2 \;\;\;\;\;\; (34)}
  Under the null hypothesis (25), the t-statistic in (29) follows a 
  \link[stats:TDist]{Student's t-distribution} with \eqn{n_1 + n_2 - 2} degrees of 
  freedom (Zar, 2010, Chapter 8; Johnson et al., 1995, pp.508--510, 
  Helsel and Hirsch, 1992, pp.124--128).

  The formulas for the power of the two-sample t-test are precisely the same as those 
  for the one-sample case, with the following modifications:
  \deqn{\nu = n_1 + n_2 - 2 \;\;\;\;\;\; (35)}
  \deqn{\Delta = \sqrt{\frac{n_1 n_2}{n_1 + n_2}} (\frac{\delta}{\sigma}) \;\;\;\;\;\; (36)}
  \deqn{\delta = \mu_1 - \mu_2 \;\;\;\;\;\; (37)}

  Note that none of the equations for the power of the t-test requires knowledge of 
  the values \eqn{\delta} or \eqn{\sigma} (the population standard deviation for 
  both populations), only the ratio \eqn{\delta/\sigma}.  The argument 
  \code{delta.over.sigma} is this ratio, and it is referred to as the 
  \dQuote{scaled difference}.
}
\value{
  a numeric vector powers.
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). 
  \emph{Statistics for Environmental Engineers}.  Second Edition.   
  Lewis Publishers, Boca Raton, FL.

  Helsel, D.R., and R.M. Hirsch. (1992). 
  \emph{Statistical Methods in Water Resources Research}. 
  Elsevier, New York, NY, Chapter 7.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995).  \emph{Continuous Univariate 
  Distributions, Volume 2}.  Second Edition.  John Wiley and Sons, New York, 
  Chapters 28, 31

  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton, FL.

  USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}. 
  EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition. 
  Prentice-Hall, Upper Saddle River, NJ.
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
  In this case, assuming normally distributed data, you can perform the 
  Student's t-test.

  In the course of designing a sampling program, an environmental scientist may wish 
  to determine the relationship between sample size, significance level, power, and 
  scaled difference if one of the objectives of the sampling program is to determine 
  whether a mean differs from a specified level or two means differ from each other.  
  The functions \code{tTestPower}, \code{\link{tTestN}}, 
  \code{\link{tTestScaledMdd}}, and \code{\link{plotTTestDesign}} can be used to 
  investigate these relationships for the case of normally-distributed observations.
}
\seealso{
  \code{\link{tTestN}}, \code{\link{tTestScaledMdd}}, \code{\link{tTestAlpha}}, 
  \code{\link{plotTTestDesign}}, \link[stats]{Normal}, 
  \code{\link{t.test}}, \link{Hypothesis Tests}. 
}
\examples{
  # Look at how the power of the one-sample t-test increases with 
  # increasing sample size:

  seq(5, 30, by = 5) 
  #[1] 5 10 15 20 25 30 

  power <- tTestPower(n.or.n1 = seq(5, 30, by = 5), delta.over.sigma = 0.5) 

  round(power, 2) 
  #[1] 0.14 0.29 0.44 0.56 0.67 0.75

  #----------

  # Repeat the last example, but use the approximation.
  # Note how the approximation underestimates the power 
  # for the smaller sample sizes.
  #----------------------------------------------------

  power <- tTestPower(n.or.n1 = seq(5, 30, by = 5), delta.over.sigma = 0.5, 
    approx = TRUE) 

  round(power, 2) 
  #[1] 0.10 0.26 0.42 0.56 0.67 0.75

  #----------

  # Look at how the power of the two-sample t-test increases with increasing 
  # scaled difference:

  seq(0.5, 2, by = 0.5) 
  #[1] 0.5 1.0 1.5 2.0 

  power <- tTestPower(10, sample.type = "two.sample", 
    delta.over.sigma = seq(0.5, 2, by = 0.5)) 

  round(power, 2) 
  #[1] 0.19 0.56 0.89 0.99

  #----------

  # Look at how the power of the two-sample t-test increases with increasing values 
  # of Type I error:

  power <- tTestPower(20, sample.type = "two.sample", delta.over.sigma = 0.5, 
    alpha = c(0.001, 0.01, 0.05, 0.1)) 

  round(power, 2) 
  #[1] 0.03 0.14 0.34 0.46

  #==========

  # Modifying the example on pages 21-4 to 21-5 of USEPA (2009), determine how 
  # adding another four months of observations to increase the sample size from 
  # 4 to 8 for any one particular compliance well will affect the power of a 
  # one-sample t-test that compares the mean for the well with the MCL of 
  # 7 ppb.  Use alpha = 0.01, assume an upper one-sided alternative 
  # (i.e., compliance well mean larger than 7 ppb), and assume a scaled 
  # difference of 2.  (The data are stored in EPA.09.Ex.21.1.aldicarb.df.) 
  # Note that the power changes from 49% to 98% by increasing the sample size 
  # from 4 to 8.

  tTestPower(n.or.n1 = c(4, 8), delta.over.sigma = 2, alpha = 0.01, 
    sample.type = "one.sample", alternative = "greater")
  #[1] 0.4865800 0.9835401

  #==========

  # Clean up
  #---------
  rm(power)
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }

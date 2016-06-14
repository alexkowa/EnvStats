\name{linearTrendTestPower}
\alias{linearTrendTestPower}
\title{
  Power of a t-Test for Linear Trend
}
\description{
  Compute the power of a parametric test for linear trend, given the sample size or 
  predictor variable values, scaled slope, and significance level.
}
\usage{
  linearTrendTestPower(n, x = lapply(n, seq), slope.over.sigma = 0, alpha = 0.05, 
    alternative = "two.sided", approx = FALSE)
}
\arguments{
  \item{n}{
  numeric vector of sample sizes.  All values of \code{n} must be positive integers 
  larger than 2.  This argument is ignored when \code{x} is supplied.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are not allowed.
}
  \item{x}{
  numeric vector of predictor variable values, or a list in which each component is 
  a numeric vector of predictor variable values.  Usually, the predictor variable is 
  time (e.g., days, months, quarters, etc.).  The default value is 
  \code{x=lapply(n,seq)}, which yields a list in which the i'th component is the 
  seqence of integers from 1 to the i'th value of the vector \code{n}.  If \code{x} 
  is a numeric vector, it must contain at least three elements, two of which must be 
  unique.  If \code{x} is a list of numeric vectors, each component of \code{x} 
  must contain at least three elements, two of which must be unique.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are not allowed.
}
  \item{slope.over.sigma}{
  numeric vector specifying the ratio of the true slope to the standard deviation of 
  the error terms (\eqn{\sigma}).  This is also called the "scaled slope".  The 
  default value is \code{slope.over.sigma=0}.
}
  \item{alpha}{
  numeric vector of numbers between 0 and 1 indicating the Type I error level 
  associated with the hypothesis test.  The default value is \code{alpha=0.05}.
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
  If the argument \code{x} is a vector, it is converted into a list with one 
  component.  If the arguments \code{n}, \code{x}, \code{slope.over.sigma}, and 
  \code{alpha} are not all the same length, they are replicated to be the same 
  length as the length of the longest argument.

  \bold{Basic Model} \cr
  Consider the simple linear regression model
  \deqn{Y = \beta_0 + \beta_1 X + \epsilon \;\;\;\;\;\; (1)}
  where \eqn{X} denotes the predictor variable (observed without error), 
  \eqn{\beta_0} denotes the intercept, \eqn{\beta_1} denotes the slope, and the 
  error term \eqn{\epsilon} is assumed to be a random variable from a normal 
  distribution with mean 0 and standard deviation \eqn{\sigma}.  Let
  \deqn{(\underline{x}, \underline{y}) = (x_1, y_1), (x_2, y_2), \ldots, (x_n, y_n) \;\;\;\;\;\; (2)}
  denote \eqn{n} independent observed \eqn{(X,Y)} pairs from the model (1).

  Often in environmental data analysis, we are interested in determining whether there 
  is a trend in some indicator variable over time.  In this case, the predictor 
  variable \eqn{X} is time (e.g., day, month, quarter, year, etc.), and the \eqn{n} 
  values of the response variable \eqn{Y} represent measurements taken over time.  
  The slope then represents the change in the average of the response variable per 
  one unit of time.

  When the argument \code{x} is a numeric vector, it represents the 
  \eqn{n} values of the predictor variable.  When the argument \code{x} is a 
  list, each component of \code{x} is a numeric vector that represents a set values 
  of the predictor variable (and the number of elements may vary by component).  
  By default, the argument \code{x} is a list for which the i'th component is simply 
  the integers from 1 to the value of the i'th element of the argument \code{n}, 
  representing, for example, Day 1, Day2, ..., Day \code{n[i]}.

  In the discussion that follows, be sure not to confuse the intercept and slope 
  coefficients \eqn{\beta_0} and \eqn{\beta_1} with the Type II error of the 
  hypothesis test, which is denoted by \eqn{\beta}.
  \cr

  \bold{Estimation of Coefficients and Confidence Interval for Slope} \cr
  The standard least-squares estimators of the slope and intercept are given by:
  \deqn{\hat{\beta}_1 = \frac{S_{xy}}{S_{xx}} \;\;\;\;\;\; (3)}
  \deqn{\hat{\beta}_0 = \bar{y} - \hat{\beta}_1 \bar{x} \;\;\;\;\;\; (4)}
  where
  \deqn{S_{xy} = \sum_{i=1}^n (x_i - \bar{x})(y_i - \bar{y}) \;\;\;\;\;\; (5)}
  \deqn{S_{xx} = \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\;\;\; (6)}
  \deqn{\bar{x} = \frac{1}{n} \sum_{i=1}^n x_i \;\;\;\;\;\; (7)}
  \deqn{\bar{y} = \frac{1}{n} \sum_{i=1}^n y_i \;\;\;\;\;\; (8)}
  (Draper and Smith, 1998, p.25; Zar, 2010, p.332-334; Berthoux and Brown, 2002, p.297; 
  Helsel and Hirsch, p.226).  The estimator of slope in Equation (3) has a normal 
  distribution with mean equal to the true slope, and variance given by:
  \deqn{Var(\hat{\beta}_1) = \sigma_{\hat{\beta}_1}^2 = \frac{\sigma^2}{S_{xx}} \;\;\;\;\;\; (9)}
  (Draper and Smith, 1998, p.35; Zar, 2010, p.341; Berthoux and Brown, 2002, p.299; 
  Helsel and Hirsch, 1992, p.227). Thus, a \eqn{(1-\alpha)100\%} two-sided confidence 
  interval for the slope is given by:
  \deqn{[ \hat{\beta}_1 - t_{n-2}(1-\alpha/2) \hat{\sigma}_{\hat{\beta}_1}, \;\; \hat{\beta}_1 + t_{n-2}(1-\alpha/2) \hat{\sigma}_{\hat{\beta}_1} ] \;\;\;\;\;\; (10)}
  where
  \deqn{\hat{\sigma}_{\hat{\beta}_1} = \frac{\hat{\sigma}}{\sqrt{S_{xx}}} \;\;\;\;\;\; (11)}
  \deqn{\hat{\sigma}^2 = s^2 = \frac{1}{n-2} \sum_{i=1}^n (y_i - \hat{y}_i)^2 \;\;\;\;\;\; (12)}
  \deqn{\hat{y}_i = \hat{\beta}_0 + \hat{\beta}_1 x_i \;\;\;\;\;\; (13)}
  and \eqn{t_{\nu}(p)} denotes the \eqn{p}'th quantile of 
  \link[stats:TDist]{Student's t-distribution} with \eqn{\nu} degrees of freedom 
  (Draper and Smith, 1998, p.36; Zar, 2010, p.343; Berthoux and Brown, 2002, p.300; 
  Helsel and Hirsch, 1992, p.240).
  \cr

  \bold{Testing for a Non-Zero Slope} \cr
  Consider the null hypothesis of a zero slope coefficient:
  \deqn{H_0: \beta_1 = 0 \;\;\;\;\;\; (14)}
  The three possible alternative hypotheses are the upper one-sided alternative 
  (\code{alternative="greater"}):
  \deqn{H_a: \beta_1 > 0 \;\;\;\;\;\; (15)}
  the lower one-sided alternative (\code{alternative="less"})
  \deqn{H_a: \beta_1 < 0 \;\;\;\;\;\; (16)}
  and the two-sided alternative (\code{alternative="two.sided"})
  \deqn{H_a: \beta_1 \ne 0 \;\;\;\;\;\; (17)}
  The test of the null hypothesis (14) versus any of the three alternatives (15)-(17) is 
  based on the Student t-statistic:
  \deqn{t = \frac{\hat{\beta}_1}{\hat{\sigma}_{\hat{\beta}_1}} =  \frac{\hat{\beta}_1}{s/\sqrt{S_{xx}}} \;\;\;\;\;\; (18)}
  Under the null hypothesis (14), the t-statistic in (18) follows a 
  \link[stats:TDist]{Student's t-distribution} with \eqn{n-2} degrees of freedom 
  (Draper and Smith, 1998, p.36; Zar, 2010, p.341; 
  Helsel and Hirsch, 1992, pp.238-239).

  The formula for the power of the test of a zero slope depends on which alternative 
  is being tested.  
  The two subsections below describe exact and approximate formulas for the power of 
  the test.  Note that none of the equations for the power of the t-test 
  requires knowledge of the values \eqn{\beta_1} or \eqn{\sigma} 
  (the population standard deviation of the error terms), only the ratio 
  \eqn{\beta_1/\sigma}.  The argument \code{slope.over.sigma} is this ratio, and it is 
  referred to as the \dQuote{scaled slope}.
  \cr

  \bold{\emph{Exact Power Calculations}} (\code{approx=FALSE}) \cr
  This subsection describes the exact formulas for the power of the t-test for a 
  zero slope.
  \cr
 
  \emph{Upper one-sided alternative} (\code{alternative="greater"}) \cr
  The standard Student's t-test rejects the null hypothesis (1) in favor of the 
  upper alternative hypothesis (2) at level-\eqn{\alpha} if
  \deqn{t \ge t_{\nu}(1 - \alpha) \;\;\;\;\;\; (19)}
  where 
  \deqn{\nu = n - 2 \;\;\;\;\;\; (20)}
  and, as noted previously, \eqn{t_{\nu}(p)} denotes the \eqn{p}'th quantile of 
  Student's t-distribution with \eqn{\nu} degrees of freedom.    
  The power of this test, denoted by \eqn{1-\beta}, where \eqn{\beta} denotes the 
  probability of a Type II error, is given by:
  \deqn{1 - \beta = Pr[t_{\nu, \Delta} \ge t_{\nu}(1 - \alpha)] = 1 - G[t_{\nu}(1 - \alpha), \nu, \Delta] \;\;\;\;\;\; (21)}
  where
  \deqn{\Delta = \sqrt{S_{xx}} \frac{\beta_1}{\sigma} \;\;\;\;\;\; (22)}
  and \eqn{t_{\nu, \Delta}} denotes a 
  \link[stats:TDist]{non-central Student's t-random variable} with 
  \eqn{\nu} degrees of freedom and non-centrality parameter \eqn{\Delta}, and 
  \eqn{G(x, \nu, \Delta)} denotes the cumulative distribution function of this 
  random variable evaluated at \eqn{x} (Johnson et al., 1995, pp.508-510).  
  Note that when the predictor variable \eqn{X} represents equally-spaced measures 
  of time (e.g., days, months, quarters, etc.) and 
  \deqn{x_i = i, \;\; i = 1, 2, \ldots, n \;\;\;\;\;\; (23)}
  then the non-centrality parameter in Equation (22) becomes:
  \deqn{\Delta = \sqrt{\frac{(n-1)n(n+1)}{12}} \frac{\beta_1}{\sigma} \;\;\;\;\;\; (24)}
  \cr

  \emph{Lower one-sided alternative} (\code{alternative="less"}) \cr
  The standard Student's t-test rejects the null hypothesis (1) in favor of the 
  lower alternative hypothesis (3) at level-\eqn{\alpha} if
  \deqn{t \le t_{\nu}(\alpha) \;\;\;\;\;\; (25)}
  and the power of this test is given by:
  \deqn{1 - \beta = Pr[t_{\nu, \Delta} \le t_{\nu}(\alpha)] = G[t_{\nu}(\alpha), \nu, \Delta] \;\;\;\;\;\; (26)}
  \cr

  \emph{Two-sided alternative} (\code{alternative="two.sided"}) \cr
  The standard Student's t-test rejects the null hypothesis (14) in favor of the 
  two-sided alternative hypothesis (17) at level-\eqn{\alpha} if
  \deqn{|t| \ge t_{\nu}(1 - \alpha/2) \;\;\;\;\;\; (27)}
  and the power of this test is given by:
  \deqn{1 - \beta = Pr[t_{\nu, \Delta} \le t_{\nu}(\alpha/2)] + Pr[t_{\nu, \Delta} \ge t_{\nu}(1 - \alpha/2)]}
  \deqn{= G[t_{\nu}(\alpha/2), \nu, \Delta] + 1 - G[t_{\nu}(1 - \alpha/2), \nu, \Delta] \;\;\;\;\;\; (28)}
  The power of the t-test given in Equation (28) can also be expressed in terms of the 
  cumulative distribution function of the \link[stats:FDist]{non-central F-distribution} 
  as follows. Let \eqn{F_{\nu_1, \nu_2, \Delta}} denote a 
  \link[stats:FDist]{non-central F random variable} with \eqn{\nu_1} and 
  \eqn{\nu_2} degrees of freedom and non-centrality parameter \eqn{\Delta}, and let 
  \eqn{H(x, \nu_1, \nu_2, \Delta)} denote the cumulative distribution function of this 
  random variable evaluated at \eqn{x}. Also, let \eqn{F_{\nu_1, \nu_2}(p)} denote 
  the \eqn{p}'th quantile of the central F-distribution with \eqn{\nu_1} and 
  \eqn{\nu_2} degrees of freedom.  It can be shown that
  \deqn{(t_{\nu, \Delta})^2 \cong F_{1, \nu, \Delta^2} \;\;\;\;\;\; (29)}
  where \eqn{\cong} denotes \dQuote{equal in distribution}.  Thus, it follows that
  \deqn{[t_{\nu}(1 - \alpha/2)]^2 = F_{1, \nu}(1 - \alpha) \;\;\;\;\;\; (30)}
  so the formula for the power of the t-test given in Equation (28) can also be 
  written as:
  \deqn{1 - \beta = Pr\{(t_{\nu, \Delta})^2  \ge [t_{\nu}(1 - \alpha/2)]^2\}}
  \deqn{= Pr[F_{1, \nu, \Delta^2} \ge F_{1, \nu}(1 - \alpha)] = 1 - H[F_{1, \nu}(1-\alpha), 1, \nu, \Delta^2] \;\;\;\;\;\; (31)}
  \cr
 
  \bold{\emph{Approximate Power Calculations}} (\code{approx=TRUE}) \cr
  Zar (2010, pp.115--118) presents an approximation to the power for the t-test 
  given in Equations (21), (26), and (28) above.  His approximation to the power 
  can be derived by using the approximation
  \deqn{\sqrt{S_{xx}} \frac{\beta_1}{s} \approx \sqrt{SS_{xx}} \frac{\beta_1}{\sigma} = \Delta \;\;\;\;\;\; (32)}
  where \eqn{\approx} denotes \dQuote{approximately equal to}.  Zar's approximation 
  can be summarized in terms of the cumulative distribution function of the 
  non-central t-distribution as follows:
  \deqn{G(x, \nu, \Delta) \approx G(x - \Delta, \nu, 0) = G(x - \Delta, \nu) \;\;\;\;\;\; (33)}
  where \eqn{G(x, \nu)} denotes the cumulative distribution function of the 
  central Student's t-distribution with \eqn{\nu} degrees of freedom evaluated at 
  \eqn{x}.

  The following three subsections explicitly derive the approximation to the power of 
  the t-test for each of the three alternative hypotheses.
  \cr
 
  \emph{Upper one-sided alternative} (\code{alternative="greater"}) \cr
  The power for the upper one-sided alternative (15) given in Equation (21) can be 
  approximated as:
  \deqn{1 - \beta = Pr[t \ge t_{\nu}(1 - \alpha)]}
  \deqn{= Pr[\frac{\hat{\beta}_1}{s/\sqrt{S_{xx}}} \ge t_{\nu}(1 - \alpha) - \sqrt{S_{xx}}\frac{\beta_1}{s}]}
  \deqn{\approx Pr[t_{\nu} \ge t_{\nu}(1 - \alpha) - \Delta]}
  \deqn{= 1 - Pr[t_{\nu} \le t_{\nu}(1 - \alpha) - \Delta]}
  \deqn{ = 1 - G[t_{\nu}(1-\alpha) - \Delta, \nu] \;\;\;\;\;\; (34)}
  where \eqn{t_{\nu}} denotes a central Student's t-random variable with \eqn{\nu} 
  degrees of freedom.
  \cr

  \emph{Lower one-sided alternative} (\code{alternative="less"}) \cr
  The power for the lower one-sided alternative (16) given in Equation (26) can be 
  approximated as:
  \deqn{1 - \beta = Pr[t \le t_{\nu}(\alpha)]}
  \deqn{= Pr[\frac{\hat{\beta}_1}{s/\sqrt{S_{xx}}} \le t_{\nu}(\alpha) - \sqrt{S_{xx}}\frac{\beta_1}{s}]}
  \deqn{\approx Pr[t_{\nu} \le t_{\nu}(\alpha) - \Delta]}
  \deqn{ = G[t_{\nu}(\alpha) - \Delta, \nu] \;\;\;\;\;\; (35)}
  \cr
 
  \emph{Two-sided alternative} (\code{alternative="two.sided"}) \cr
  The power for the two-sided alternative (17) given in Equation (28) can be 
  approximated as:
  \deqn{1 - \beta = Pr[t \le t_{\nu}(\alpha/2)] + Pr[t \ge t_{\nu}(1 - \alpha/2)]}
  \deqn{= Pr[\frac{\hat{\beta}_1}{s/\sqrt{S_{xx}}} \le t_{\nu}(\alpha/2) - \sqrt{SS_{xx}}\frac{\beta_1}{s}] + Pr[\frac{\hat{\beta}_1}{s/\sqrt{S_{xx}}} \ge t_{\nu}(1 - \alpha) - \sqrt{SS_{xx}}\frac{\beta_1}{s}]}
  \deqn{\approx Pr[t_{\nu} \le t_{\nu}(\alpha/2) - \Delta] + Pr[t_{\nu} \ge t_{\nu}(1 - \alpha/2) - \Delta]}
  \deqn{= G[t_{\nu}(\alpha/2) - \Delta, \nu] + 1 - G[t_{\nu}(1-\alpha/2) - \Delta, \nu] \;\;\;\;\;\; (36)}
}
\value{
  a numeric vector powers.
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). 
  \emph{Statistics for Environmental Engineers}.  Second Edition.   
  Lewis Publishers, Boca Raton, FL.

  Draper, N., and H. Smith. (1998).  \emph{Applied Regression Analysis}.  
  Third Edition.  John Wiley and Sons, New York, Chapter 1.

  Helsel, D.R., and R.M. Hirsch. (1992). 
  \emph{Statistical Methods in Water Resources Research}. 
  Elsevier, New York, NY, Chapter 9.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995).  \emph{Continuous Univariate 
  Distributions, Volume 2}.  Second Edition.  John Wiley and Sons, New York, 
  Chapters 28, 31

  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton, FL.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition. 
  Prentice-Hall, Upper Saddle River, NJ.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Often in environmental data analysis, we are interested in determining whether 
  there is a trend in some indicator variable over time.  In this case, the predictor 
  variable \eqn{X} is time (e.g., day, month, quarter, year, etc.), and the \eqn{n} 
  values of the response variable represent measurements taken over time.  The slope 
  then represents the change in the average of the response variable per one unit of 
  time.

  You can use the parametric model (1) to model your data, then use the \R function 
  \code{\link{lm}} to fit the regression coefficients and the \code{\link{summary.lm}} 
  function to perform a test for the significance of the slope coefficient.  The 
  function \code{linearTrendTestPower} computes the power of this t-test, given a 
  fixed value of the sample size, scaled slope, and significance level.

  You can also use \link[=kendallTrendTest]{Kendall's nonparametric test for trend} 
  if you don't want to assume the error terms are normally distributed.  When the 
  errors are truly normally distributed, the asymptotic relative efficiency of 
  Kendall's test for trend versus the parametric t-test for a zero slope is 0.98, 
  and Kendall's test can be more powerful than the parametric t-test when the errors 
  are not normally distributed.  Thus the function \code{linearTrendTestPower} can 
  also be used to estimate the power of Kendall's test for trend.

  In the course of designing a sampling program, an environmental scientist may wish 
  to determine the relationship between sample size, significance level, power, and 
  scaled slope if one of the objectives of the sampling program is to determine 
  whether a trend is occurring.  The functions \code{linearTrendTestPower}, 
  \code{\link{linearTrendTestN}}, \code{\link{linearTrendTestScaledMds}}, and \cr
  \code{\link{plotLinearTrendTestDesign}} can be used to investigate these 
  relationships.
}
\seealso{
  \code{\link{linearTrendTestN}}, \code{\link{linearTrendTestScaledMds}}, 
  \code{\link{plotLinearTrendTestDesign}}, \code{\link{lm}}, \cr
  \code{\link{summary.lm}}, \code{\link{kendallTrendTest}}, 
  \link{Power and Sample Size}, \link{Normal}, \code{\link{t.test}}.
}
\examples{
  # Look at how the power of the t-test for zero slope increases with increasing 
  # sample size:

  seq(5, 30, by = 5) 
  #[1] 5 10 15 20 25 30 

  power <- linearTrendTestPower(n = seq(5, 30, by = 5), slope.over.sigma = 0.1) 

  round(power, 2) 
  #[1] 0.06 0.13 0.34 0.68 0.93 1.00

  #----------

  # Repeat the last example, but compute the approximate power instead of the 
  # exact:

  power <- linearTrendTestPower(n = seq(5, 30, by = 5), slope.over.sigma = 0.1, 
    approx = TRUE) 

  round(power, 2) 
  #[1] 0.05 0.11 0.32 0.68 0.93 0.99

  #----------

  # Look at how the power of the t-test for zero slope increases with increasing 
  # scaled slope:

  seq(0.05, 0.2, by = 0.05) 
  #[1] 0.05 0.10 0.15 0.20 

  power <- linearTrendTestPower(15, slope.over.sigma = seq(0.05, 0.2, by = 0.05)) 

  round(power, 2) 
  #[1] 0.12 0.34 0.64 0.87

  #----------

  # Look at how the power of the t-test for zero slope increases with increasing 
  # values of Type I error:

  power <- linearTrendTestPower(20, slope.over.sigma = 0.1, 
    alpha = c(0.001, 0.01, 0.05, 0.1)) 

  round(power, 2) 
  #[1] 0.14 0.41 0.68 0.80

  #----------

  # Show that for a simple regression model, you get a greater power of detecting 
  # a non-zero slope if you take all the observations at two endpoints, rather than 
  # spreading the observations evenly between two endpoints. 
  # (Note: This design usually cannot work with environmental monitoring data taken 
  # over time since usually observations taken close together in time are not 
  # independent.)

  linearTrendTestPower(x = 1:10, slope.over.sigma = 0.1) 
  #[1] 0.1265976


  linearTrendTestPower(x = c(rep(1, 5), rep(10, 5)), slope.over.sigma = 0.1) 
  #[1] 0.2413823

  #==========

  # Clean up
  #---------
  rm(power)
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }

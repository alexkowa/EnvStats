\name{chenTTest}
\alias{chenTTest}
\title{
  Chen's Modified One-Sided t-test for Skewed Distributions
}
\description{
  For a skewed distribution, estimate the mean, standard deviation, and skew; 
  test the null hypothesis that the mean is equal to a user-specified value vs. 
  a one-sided alternative; and create a one-sided confidence interval for the mean.
}
\usage{
  chenTTest(x, y = NULL, alternative = "greater", mu = 0, paired = !is.null(y), 
    conf.level = 0.95, ci.method = "z")
}
\arguments{
  \item{x}{
  numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), and 
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{y}{
  optional numeric vector of observations that are paired with the observations in 
  \code{x}.  The length of \code{y} must be the same as the length of \code{x}.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.  This argument is ignored if 
  \code{paired=FALSE}, and must be supplied if \code{paired=TRUE}.  The default value 
  is \code{y=NULL}.
}
  \item{alternative}{
  character string indicating the kind of alternative hypothesis.  The possible values 
  are \code{"greater"} (the default) and \code{"less"}. The value \code{"greater"} 
  should be used for positively-skewed distributions, and the value \code{"less"} 
  should be used for negatively-skewed distributions.
}
  \item{mu}{
  numeric scalar indicating the hypothesized value of the mean.  The default value is 
  \code{mu=0}.
}
  \item{paired}{
  character string indicating whether to perform a paired or one-sample t-test.  The 
  possible values are \code{paired=FALSE} (the default; indicates a one-sample t-test) 
  and \code{paired=TRUE}.
}
  \item{conf.level}{
  numeric scalar between 0 and 1 indicating the confidence level associated with the 
  confidence interval for the population mean.  The default value is \cr
  \code{conf.level=0.95}.
}
  \item{ci.method}{
  character string indicating which critical value to use to construct the confidence 
  interval for the mean.  The possible values are \code{"z"} (the default), 
  \code{"t"}, and \code{"Avg. of z and t"}.  See the DETAILS section below for more 
  information.
}
}
\details{
  \bold{One-Sample Case} (\code{paired=FALSE}) \cr
  Let \eqn{\underline{x} = (x_1, x_2, \ldots, x_n)} be a vector of \eqn{n} independent 
  and identically distributed (i.i.d.) observations from some distribution with mean 
  \eqn{\mu} and standard deviation \eqn{\sigma}.
  \cr

  \emph{Background: The Conventional Student's t-Test} \cr
  Assume that the \eqn{n} observations come from a normal (Gaussian) distribution, and 
  consider the test of the null hypothesis:
  \deqn{H_0: \mu = \mu_0 \;\;\;\;\;\; (1)}
  The three possible alternative hypotheses are the upper one-sided alternative 
  (\code{alternative="greater"}):
  \deqn{H_a: \mu > \mu_0 \;\;\;\;\;\; (2)}
  the lower one-sided alternative (\code{alternative="less"}):
  \deqn{H_a: \mu < \mu_0 \;\;\;\;\;\; (3)}
  and the two-sided alternative:
  \deqn{H_a: \mu \ne \mu_0 \;\;\;\;\;\; (4)}  
  The test of the null hypothesis (1) versus any of the three alternatives (2)-(4) 
  is usually based on the Student t-statistic:
  \deqn{t = \frac{\bar{x} - \mu_0}{s/\sqrt{n}} \;\;\;\;\;\; (5)}
  where
  \deqn{\bar{x} = \frac{1}{n}\sum_{i=1}^n x_i \;\;\;\;\;\; (6)}
  \deqn{s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\;\;\; (7)}
  (see the \R help file for \code{\link{t.test}}).  Under the null hypothesis (1), 
  the t-statistic in (5) follows a \link[stats:TDist]{Student's t-distribution} with 
  \eqn{n-1} degrees of freedom (Zar, 2010, p.99; Johnson et al., 1995, pp.362-363).  
  The t-statistic is fairly robust to departures from normality in terms of 
  maintaining Type I error and power, provided that the sample size is sufficiently 
  large.
  \cr

  \emph{Chen's Modified t-Test for Skewed Distributions} \cr
  In the case when the underlying distribution of the \eqn{n} observations is 
  positively skewed and the sample size is small, the sampling distribution of the 
  t-statistic under the null hypothesis (1) does not follow a Student's t-distribution, 
  but is instead negatively skewed.  For the test against the upper alternative in (2) 
  above, this leads to a Type I error smaller than the one assumed and a loss of power 
  (Chen, 1995b, p.767).

  Similarly, in the case when the underlying distribution of the \eqn{n} observations 
  is negatively skewed and the sample size is small, the sampling distribution of the 
  t-statistic is positively skewed.  For the test against the lower alternative in (3) 
  above, this also leads to a Type I error smaller than the one assumed and a loss of 
  power.

  In order to overcome these problems, Chen (1995b) proposed the following modified 
  t-statistic that takes into account the skew of the underlying distribution:
  \deqn{t_2 = t + a(1 + 2t^2) + 4a^2(t + 2t^3) \;\;\;\;\;\; (8)}
  where
  \deqn{a = \frac{\sqrt{\hat{\beta}_1}}{6n} \;\;\;\;\;\; (9)}
  \deqn{\hat{\beta}_1 = \frac{\hat{\mu}_3}{\hat{\sigma}^3} \;\;\;\;\;\; (10)}
  \deqn{\hat{\mu}_3 = \frac{n}{(n-1)(n-2)} \sum_{i=1}^n (x_i - \bar{x})^3 \;\;\;\;\;\; (11)}
  \deqn{\hat{\sigma}^3 = s^3 = [\frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2]^{3/2} \;\;\;\;\;\; (12)}
  Note that the quantity \eqn{\sqrt{\hat{\beta}_1}} in (9) is an estimate of 
  the skew of the underlying distribution and is based on unbiased estimators of 
  central moments (see the help file for \code{\link{skewness}}).

  For a positively-skewed distribution, Chen's modified t-test rejects the null 
  hypothesis (1) in favor of the upper one-sided alternative (2) if the t-statistic in 
  (8) is too large.  For a negatively-skewed distribution, Chen's modified t-test 
  rejects the null hypothesis (1) in favor of the lower one-sided alternative (3) if 
  the t-statistic in (8) is too small.

  Chen's modified t-test is \bold{not} applicable to testing the two-sided alternative 
  (4).  It should also \bold{not} be used to test the upper one-sided alternative (2) 
  based on negatively-skewed data, nor should it be used to test the lower one-sided 
  alternative (3) based on positively-skewed data.
  \cr

  \emph{Determination of Critical Values and p-Values} \cr
  Chen (1995b) performed a simulation study in which the modified t-statistic in (8) 
  was compared to a critical value based on the normal distribution (z-value), 
  a critical value based on Student's t-distribution (t-value), and the average of the 
  critical z-value and t-value.  Based on the simulation study, Chen (1995b) suggests 
  using either the z-value or average of the z-value and t-value when \eqn{n} 
  (the sample size) is small (e.g., \eqn{n \le 10}) or \eqn{\alpha} (the Type I error) 
  is small (e.g. \eqn{\alpha \le 0.01}), and using either the t-value or the average 
  of the z-value and t-value when \eqn{n \ge 20} or \eqn{\alpha \ge 0.05}.

  The function \code{chenTTest} returns three different p-values:  one based on the 
  normal distribution, one based on Student's t-distribution, and one based on the 
  average of these two p-values.  This last p-value should roughly correspond to a 
  p-value based on the distribution of the average of a normal and Student's t 
  random variable.
  \cr

  \emph{Computing Confidence Intervals} \cr
  The function \code{chenTTest} computes a one-sided confidence interval for the true 
  mean \eqn{\mu} based on finding all possible values of \eqn{\mu} for which the null 
  hypothesis (1) will not be rejected, with the confidence level determined by the 
  argument \code{conf.level}.  The argument \code{ci.method} determines which p-value 
  is used in the algorithm to determine the bounds on \eqn{\mu}.  When 
  \code{ci.method="z"}, the p-value is based on the normal distribution, when 
  \code{ci.method="t"}, the p-value is based on Student's t-distribution, and when 
  \code{ci.method="Avg. of z and t"} the p-value is based on the average of the 
  p-values based on the normal and Student's t-distribution.
  \cr

  \bold{Paired-Sample Case} (\code{paired=TRUE}) \cr
  When the argument \code{paired=TRUE}, the arguments \code{x} and \code{y} are assumed 
  to have the same length, and the \eqn{n} differences
  \deqn{d_i = x_i - y_i, \;\; i = 1, 2, \ldots, n}
  are assumed to be i.i.d. observations from some distribution with mean \eqn{\mu} 
  and standard deviation \eqn{\sigma}.  Chen's modified t-test can then be applied 
  to the differences.
}
\value{
  a list of class \code{"htestEnvStats"} containing the results of the hypothesis test.  See 
  the help file for \code{\link{htestEnvStats.object}} for details.
}
\references{
  Chen, L. (1995b).  Testing the Mean of Skewed Distributions.  
  \emph{Journal of the American Statistical Association} \bold{90}(430), 767--772.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995).  \emph{Continuous Univariate 
  Distributions, Volume 2}.  Second Edition.  John Wiley and Sons, New York, 
  Chapters 28, 31.

  Land, C.E. (1971).  Confidence Intervals for Linear Functions of the Normal Mean and 
  Variance.  \emph{The Annals of Mathematical Statistics} \bold{42}(4), 1187--1205.

  Millard, S.P., and N.K. Neerchal. (2001).  
  \emph{Environmental Statistics with S-PLUS}.  CRC Press, Boca Raton, FL, pp.402--404.

  Singh, A., N. Armbya, and A. Singh. (2010b). 
  \emph{ProUCL Version 4.1.00 Technical Guide (Draft)}. EPA/600/R-07/041, May 2010.  
  Office of Research and Development, U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (1996c).  \emph{Soil Screening Guidance: Technical Background Document}.  
  EPA/540/R-95/128, PB96963502. Office of Emergency and Remedial Response, U.S. 
  Environmental Protection Agency, Washington, D.C., May, 1996.

  USEPA. (2002d).  \emph{Estimation of the Exposure Point Concentration Term Using a 
  Gamma Distribution}.  
  EPA/600/R-02/084. October 2002. Technology Support Center for Monitoring and 
  Site Characterization, Office of Research and Development, Office of Solid Waste and 
  Emergency Response, U.S. Environmental Protection Agency, Washington, D.C.

  Zar, J.H. (2010).  \emph{Biostatistical Analysis}.  Fifth Edition.  
  Prentice-Hall, Upper Saddle River, NJ.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The presentation of Chen's (1995b) method in USEPA (2002d) and Singh et
  al. (2010b, p. 52) is incorrect for two reasons: it is based on an intermediate 
  formula instead of the actual statistic that Chen proposes, and it uses the 
  intermediate formula to compute an \emph{upper} confidence limit for the mean when 
  the sample data are positively skewed.  As explained above, for the case of 
  positively skewed data, Chen's method is appropriate to test the upper one-sided 
  alternative hypothesis that the population mean is greater than some specified 
  value, and a one-sided upper alternative corresponds to creating a one-sided 
  \emph{lower} confidence limit, not an upper confidence limit (see, for example, 
  Millard and Neerchal, 2001, p. 371).

  A frequent question in environmental statistics is \dQuote{Is the concentration of 
  chemical X greater than Y units?}  For example, in groundwater assessment 
  (compliance) monitoring at hazardous and solid waste sites, the concentration of 
  a chemical in the groundwater at a downgradient may be compared to a 
  groundwater protection standard (GWPS).  If the concentration is \dQuote{above} 
  the GWPS, then the site enters corrective action monitoring.  As another example, 
  soil screening at a Superfund site involves comparing the concentration of a 
  chemical in the soil with a pre-determined soil screening level (SSL).  If the 
  concentration is \dQuote{above} the SSL, then further investigation and possible 
  remedial action is required.  Determining what it means for the chemical 
  concentration to be \dQuote{above} a GWPS or an SSL is a policy decision:  the 
  average of the distribution of the chemical concentration must be above the GWPS or 
  SSL, or the median must be above the GWPS or SSL, or the 95'th percentile must be 
  above the GWPS or SSL, or something else.  Often, the first interpretation is used.

  The regulatory guidance document \emph{Soil Screening Guidance:  Technical 
  Background Document} (USEPA, 1996c, Part 4) recommends using Chen's t-test as one 
  possible method to compare chemical concentrations in soil samples to a soil 
  screening level (SSL).  The document notes that the distribution of chemical 
  concentrations will almost always be positively-skewed, but not necessarily fit a 
  lognormal distribution well (USEPA, 1996c, pp.107, 117-119).  It also notes that 
  using a confidence interval based on Land's (1971) method is extremely sensitive 
  to the assumption of a lognormal distribution, while Chen's test is robust with 
  respect to maintaining Type I and Type II errors for a variety of positively-skewed 
  distributions (USEPA, 1996c, pp.99, 117-119, 123-125).
 
  Hypothesis tests you can use to perform tests of location include:  
  \link[stats:t.test]{Student's t-test}, 
  \link[=oneSamplePermutationTest]{Fisher's randomization test}, 
  \link[stats:wilcox.test]{the Wilcoxon signed rank test}, 
  Chen's modified t-test, 
  \link[=signTest]{the sign test}, and a test based on a 
  bootstrap confidence interval.  
  For a discussion comparing the performance of these tests, see 
  Millard and Neerchal (2001, pp.408--409).
}
\seealso{
  \code{\link{t.test}}, \code{\link{elnorm}}, \code{\link{elnormAlt}}.
}
\examples{
  # The guidance document "Calculating Upper Confidence Limits for 
  # Exposure Point Concentrations at Hazardous Waste Sites" 
  # (USEPA, 2002d, Exhibit 9, p. 16) contains an example of 60 observations 
  # from an exposure unit.  Here we will use Chen's modified t-test to test 
  # the null hypothesis that the average concentration is less than 30 mg/L 
  # versus the alternative that it is greater than 30 mg/L.
  # In EnvStats these data are stored in the vector EPA.02d.Ex.9.mg.per.L.vec.

  sort(EPA.02d.Ex.9.mg.per.L.vec)
  # [1]  16  17  17  17  18  18  20  20  20  21  21  21  21  21  21  22
  #[17]  22  22  23  23  23  23  24  24  24  25  25  25  25  25  25  26
  #[33]  26  26  26  27  27  28  28  28  28  29  29  30  30  31  32  32
  #[49]  32  33  33  35  35  97  98 105 107 111 117 119

  dev.new()
  hist(EPA.02d.Ex.9.mg.per.L.vec, col = "cyan", xlab = "Concentration (mg/L)")

  # The Shapiro-Wilk goodness-of-fit test rejects the null hypothesis of a 
  # normal, lognormal, and gamma distribution:

  gofTest(EPA.02d.Ex.9.mg.per.L.vec)$p.value
  #[1] 2.496781e-12

  gofTest(EPA.02d.Ex.9.mg.per.L.vec, dist = "lnorm")$p.value
  #[1] 3.349035e-09

  gofTest(EPA.02d.Ex.9.mg.per.L.vec, dist = "gamma")$p.value
  #[1] 1.564341e-10


  # Use Chen's modified t-test to test the null hypothesis that
  # the average concentration is less than 30 mg/L versus the 
  # alternative that it is greater than 30 mg/L.

  chenTTest(EPA.02d.Ex.9.mg.per.L.vec, mu = 30)

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 mean = 30
  #
  #Alternative Hypothesis:          True mean is greater than 30
  #
  #Test Name:                       One-sample t-Test
  #                                 Modified for
  #                                 Positively-Skewed Distributions
  #                                 (Chen, 1995)
  #
  #Estimated Parameter(s):          mean = 34.566667
  #                                 sd   = 27.330598
  #                                 skew =  2.365778
  #
  #Data:                            EPA.02d.Ex.9.mg.per.L.vec
  #
  #Sample Size:                     60
  #
  #Test Statistic:                  t = 1.574075
  #
  #Test Statistic Parameter:        df = 59
  #
  #P-values:                        z               = 0.05773508
  #                                 t               = 0.06040889
  #                                 Avg. of z and t = 0.05907199
  #
  #Confidence Interval for:         mean
  #
  #Confidence Interval Method:      Based on z
  #
  #Confidence Interval Type:        Lower
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 29.82
  #                                 UCL =   Inf

  # The estimated mean, standard deviation, and skew are 35, 27, and 2.4, 
  # respectively.  The p-value is 0.06, and the lower 95% confidence interval 
  # is [29.8, Inf).  Depending on what you use for your Type I error rate, you 
  # may or may not want to reject the null hypothesis.
}
\keyword{ htestEnvStats }
\keyword{ models }

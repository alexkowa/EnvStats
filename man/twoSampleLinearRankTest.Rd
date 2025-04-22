\name{twoSampleLinearRankTest}
\alias{twoSampleLinearRankTest}
\title{
  Two-Sample Linear Rank Test to Detect a Difference Between Two Distributions
}
\description{
  Two-sample linear rank test to detect a difference (usually a shift) between two
  distributions.  The \link[=wilcox.test]{Wilcoxon Rank Sum test} is a special case of
  a linear rank test.  The function \cr
  \code{twoSampleLinearRankTest} is part of
  \pkg{EnvStats} mainly because this help file gives the necessary background to
  explain two-sample linear rank tests for censored data (see \cr
  \code{\link{twoSampleLinearRankTestCensored}}).
}
\usage{
  twoSampleLinearRankTest(x, y, location.shift.null = 0, scale.shift.null = 1,
    alternative = "two.sided", test = "wilcoxon", shift.type = "location")
}
\arguments{
  \item{x}{
  numeric vector of values for the first sample.
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf},
  \code{-Inf}) values are allowed but will be removed.
}
  \item{y}{
  numeric vector of values for the second sample.
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf},
  \code{-Inf}) values are allowed but will be removed.
}
  \item{location.shift.null}{
  numeric scalar indicating the hypothesized value of \eqn{\Delta}, the location
  shift between the two distributions, under the null hypothesis.  The default value is
  \code{location.shift.null=0}.  This argument is ignored if \code{shift.type="scale"}.
}
  \item{scale.shift.null}{
  numeric scalar indicating the hypothesized value of \eqn{\tau}, the scale shift
  between the two distributions, under the null hypothesis.  The default value is \cr
  \code{scale.shift.null=1}.  This argument is ignored if \code{shift.type="location"}.
}
  \item{alternative}{
  character string indicating the kind of alternative hypothesis.  The possible values
  are \code{"two.sided"} (the default), \code{"less"}, and \code{"greater"}.  See the
  DETAILS section below for more information.
}
  \item{test}{
  character string indicating which linear rank test to use.  The possible values are:
  \code{"wilcoxon"} (the default), \code{"normal.scores"}, \code{"moods.median"}, and
  \code{"savage.scores"}.
}
  \item{shift.type}{
  character string indicating which kind of shift is being tested.  The possible values
  are \code{"location"} (the default) and \code{"scale"}.
}
}
\details{
  The function \code{twoSampleLinearRankTest} allows you to compare two samples using
  a locally most powerful rank test (LMPRT) to determine whether the two samples come
  from the same distribution.  The sections below explain the concepts of location and
  scale shifts, linear rank tests, and LMPRT's.
  \cr

  \bold{Definitions of Location and Scale Shifts} \cr
  Let \eqn{X} denote a random variable representing measurements from group 1 with
  cumulative distribution function (cdf):
  \deqn{F_1(t) = Pr(X \le t) \;\;\;\;\;\; (1)}
  and let \eqn{x_1, x_2, \ldots, x_m} denote \eqn{m} independent observations from this
  distribution.  Let \eqn{Y} denote a random variable from group 2 with cdf:
  \deqn{F_2(t) = Pr(Y \le t) \;\;\;\;\;\; (2)}
  and let \eqn{y_1, y_2, \ldots, y_n} denote \eqn{n} independent observations from this
  distribution.  Set \eqn{N = m + n}.
  \cr

  \emph{General Hypotheses to Test Differences Between Two Populations} \cr
  A very general hypothesis to test whether two distributions are the same is
  given by:
  \deqn{H_0: F_1(t) = F_2(t), -\infty < t < \infty \;\;\;\;\;\; (3)}
  versus the two-sided alternative hypothesis:
  \deqn{H_a: F_1(t) \ne F_2(t) \;\;\;\;\;\; (4)}
  with strict inequality for at least one value of \eqn{t}.
  The two possible one-sided hypotheses would be:
  \deqn{H_0: F_1(t) \ge F_2(t) \;\;\;\;\;\; (5)}
  versus the alternative hypothesis:
  \deqn{H_a: F_1(t) < F_2(t) \;\;\;\;\;\; (6)}
  and
  \deqn{H_0: F_1(t) \le F_2(t) \;\;\;\;\;\; (7)}
  versus the alternative hypothesis:
  \deqn{H_a: F_1(t) > F_2(t) \;\;\;\;\;\; (8)}

  A similar set of hypotheses to test whether the two distributions are the same are
  given by (Conover, 1980, p. 216):
  \deqn{H_0: Pr(X < Y) = 1/2 \;\;\;\;\;\; (9)}
  versus the two-sided alternative hypothesis:
  \deqn{H_a: Pr(X < Y) \ne 1/2 \;\;\;\;\;\; (10)}
  or
  \deqn{H_0: Pr(X < Y) \ge 1/2 \;\;\;\;\;\; (11)}
  versus the alternative hypothesis:
  \deqn{H_a: Pr(X < Y) < 1/2 \;\;\;\;\;\; (12)}
  or
  \deqn{H_0: Pr(X < Y) \le 1/2 \;\;\;\;\;\; (13)}
  versus the alternative hypothesis:
  \deqn{H_a: Pr(X < Y) > 1/2 \;\;\;\;\;\; (14)}

  Note that this second set of hypotheses (9)--(14) is \bold{not} equivalent to the
  set of hypotheses (3)--(8).  For example, if \eqn{X} takes on the values 1 and 4
  with probability 1/2 for each, and \eqn{Y} only takes on values in the interval
  (1, 4) with strict inequality at the enpoints (e.g., \eqn{Y} takes on the values
  2 and 3 with probability 1/2 for each), then the null hypothesis (9) is
  true but the null hypothesis (3) is not true.  However, the null hypothesis (3)
  implies the null hypothesis (9), (5) implies (11), and (7) implies (13).
  \cr

  \emph{Location Shift} \cr
  A special case of the alternative hypotheses (4), (6), and (8) above is the
  \bold{\emph{location shift}} alternative:
  \deqn{H_a: F_1(t) = F_2(t - \Delta) \;\;\;\;\;\; (15)}
  where \eqn{\Delta} denotes the shift between the two groups.  (Note: some references
  refer to (15) above as a shift in the median, but in fact this kind of shift
  represents a shift in every single quantile, not just the median.)
  If \eqn{\Delta} is positive, this means that observations in group 1 tend to be
  larger than observations in group 2, and if \eqn{\Delta} is negative, observations
  in group 1 tend to be smaller than observations in group 2.

  The alternative hypothesis (15) is called a \bold{\emph{location shift}}: the only
  difference between the two distributions is a difference in location (e.g., the
  standard deviation is assumed to be the same for both distributions).  A location
  shift is not applicable to distributions that are bounded below or above by some
  constant, such as a lognormal distribution.  For lognormal distributions, the
  location shift could refer to a shift in location of the distribution of the
  log-transformed observations.

  For a location shift, the null hypotheses (3) can be generalized as:
  \deqn{H_0: F_1(t) = F_2(t - \Delta_0), -\infty < t < \infty \;\;\;\;\;\; (16)}
  where \eqn{\Delta_0} denotes the null shift between the two groups.  Almost always,
  however, the null shift is taken to be 0 and we will assume this for the rest of this
  help file.

  Alternatively, the null and alternative hypotheses can be written as
  \deqn{H_0: \Delta = 0 \;\;\;\;\;\; (17)}
  versus the alternative hypothesis
  \deqn{H_a: \Delta > 0 \;\;\;\;\;\; (18)}
  The other one-sided alternative hypothesis (\eqn{\Delta < 0}) and two-sided
  alternative hypothesis (\eqn{\Delta \ne 0}) could be considered as well.

  The general hypotheses (3)-(14) are \emph{not} location shift hypotheses
  (e.g., the standard deviation does not have to be the same for both distributions),
  but they do allow for distributions that are bounded below or above by a constant
  (e.g., lognormal distributions).
  \cr

  \emph{Scale Shift} \cr
  A special kind of scale shift replaces the alternative hypothesis (15) with the
  alternative hypothesis:
  \deqn{H_a: F_1(t) = F_2(t/\tau) \;\;\;\;\;\; (19)}
  where \eqn{\tau} denotes the shift in scale between the two groups.  Alternatively,
  the null and alternative hypotheses for this scale shift can be written as
  \deqn{H_0: \tau = 1 \;\;\;\;\;\; (20)}
  versus the alternative hypothesis
  \deqn{H_a: \tau > 1 \;\;\;\;\;\; (21)}
  The other one-sided alternative hypothesis (\eqn{t < 1}) and two-sided alternative
  hypothesis (\eqn{t \ne 1}) could be considered as well.

  This kind of scale shift often involves a shift in both location and scale.  For
  example, suppose the underlying distribution for both groups is
  \link[=Exponential]{exponential}, with parameter \code{rate=}\eqn{\lambda}.  Then
  the mean and standard deviation of the reference group is \eqn{1/\lambda}, while
  the mean and standard deviation of the treatment group is \eqn{\tau/\lambda}.  In
  this case, the alternative hypothesis (21) implies the more general alternative
  hypothesis (8).
  \cr

  \bold{Linear Rank Tests} \cr
  The usual nonparametric test to test the null hypothesis of the same distribution
  for both groups versus the location-shift alternative (18) is the
  \link[=wilcox.test]{Wilcoxon Rank Sum test}
  (Gilbert, 1987, pp.247-250; Helsel and Hirsch, 1992, pp.118-123;
  Hollander and Wolfe, 1999).  Note that the Mann-Whitney U test is equivalent to the
  Wilcoxon Rank Sum test (Hollander and Wolfe, 1999; Conover, 1980, p.215,
  Zar, 2010).  Hereafter, this test will be abbreviated as the MWW test.  The MWW test
  is performed by combining the \eqn{m} \eqn{X} observations with the \eqn{n} \eqn{Y}
  observations and ranking them from smallest to largest, and then computing the
  statistic
  \deqn{W = \sum_{i=1}^m R_i \;\;\;\;\;\; (22)}
  where \eqn{R_1, R_2, \ldots, R_m} denote the ranks of the \eqn{X} observations when
  the \eqn{X} and \eqn{Y} observations are combined ranked.  The null
  hypothesis (5), (11), or (17) is rejected in favor of the alternative hypothesis
  (6), (12)  or (18) if the value of \eqn{W} is too large.  For small sample sizes,
  the exact distribution of \eqn{W} under the null hypothesis is fairly easy to
  compute and may be found in tables (e.g., Hollander and Wolfe, 1999;
  Conover, 1980, pp.448-452).  For larger sample sizes, a normal approximation is
  usually used (Hollander and Wolfe, 1999; Conover, 1980, p.217).  For the
  \R function \code{\link{wilcox.test}}, an exact p-value is computed if the
  samples contain less than 50 finite values and there are no ties.

  \bold{It is important to note that the MWW test is actually testing the more general
  hypotheses (9)-(14) (Conover, 1980, p.216; Divine et al., 2013), even though it
  is often presented as only applying to location shifts.}

  The MWW W-statistic in Equation (22) is an example of a
  \bold{\emph{linear rank statistic}} (Hettmansperger, 1984, p.147; Prentice, 1985),
  which is any statistic that can be written in the form:
  \deqn{L = \sum_{i=1}^m a(R_i) \;\;\;\;\;\; (23)}
  where \eqn{a()} denotes a score function.  Statistics of this form are also called
  \bold{\emph{general scores statistics}} (Hettmansperger, 1984, p.147).  The MWW test
  uses the identity score function:
  \deqn{a(R_i) = R_i \;\;\;\;\;\; (24)}
  Any test based on a linear rank statistic is called a \bold{\emph{linear rank test}}.
  Under the null hypothesis (3), (9), (17), or (20), the distribution of the linear
  rank statistic \eqn{L} does not depend on the form of the underlying distribution of
  the \eqn{X} and \eqn{Y} observations.  Hence, tests based on \eqn{L} are
  nonparametric (also called distribution-free).  If the null hypothesis is not true,
  however, the distribution of \eqn{L} will depend not only on the distributions of the
  \eqn{X} and \eqn{Y} observations, but also upon the form the score function
  \eqn{a()}.
  \cr

  \bold{Locally Most Powerful Linear Rank Tests} \cr
  The decision of what scores to use may be based on considering the power of the test.
  A locally most powerful rank test (LMPRT) of the null hypothesis (17) versus the
  alternative (18) maximizes the slope of the power (as a function of \eqn{\Delta}) in
  the neighborhood where \eqn{\Delta=0}.  A LMPRT of the null hypothesis (20) versus
  the alternative (21) maximizes the slope of the power (as a function of \eqn{\tau})
  in the neighborhood where \eqn{\tau=1}.  That is, LMPRT's are the best linear rank
  test you can use for detecting small shifts in location or scale.

  Table 1 below shows the score functions associated with the LMPRT's for various
  assumed underlying distributions (Hettmansperger, 1984, Chapter 3;
  Millard and Deverel, 1988, p.2090).  A test based on the identity score function of
  Equation (24) is equivalent to a test based on the score shown in Table 1 associated
  with the logistic distribution, thus the MWW test is the LMPRT for detecting a
  location shift when the underlying observations follow the logistic distribution.
  When the underlying distribution is normal or lognormal, the LMPRT for a location
  shift uses the \dQuote{Normal scores} shown in Table 1.  When the underlying
  distribution is exponential, the LMPRT for detecting a scale shift is based on the
  \dQuote{Savage scores} shown in Table 1.

  \emph{Table 1.  Scores of LMPRT's for Various Distributions}\cr
  \tabular{llll}{
  \bold{Distribution} \tab \bold{Score} \eqn{a(R_i)}  \tab \bold{Shift Type} \tab \bold{Test Name} \cr
  Logistic            \tab \eqn{[2/(N+1)]R_i - 1}     \tab Location          \tab Wilcoxon Rank Sum \cr
                      \tab                            \tab                   \tab                   \cr
  Normal or           \tab \eqn{\Phi^{-1}[R_i/(N+1)]}* \tab Location          \tab Van der Waerden or \cr
  Lognormal (log-scale) \tab                          \tab                   \tab Normal scores     \cr
                      \tab                            \tab                   \tab                   \cr
  Double Exponential  \tab \eqn{sign[R_i - (N+1)/2]}  \tab Location          \tab Mood's Median     \cr
                      \tab                            \tab                   \tab                   \cr
  Exponential or      \tab \eqn{\sum_{j=1}^{R_i} (N-j+1)^{-1}} \tab Scale    \tab Savage scores     \cr
  Extreme Value       \tab                            \tab                   \tab
  }

  * Denotes an approximation to the true score.  The symbol \eqn{\Phi} denotes the
  cumulative distribution function of the standard normal distribution, and \eqn{sign}
  denotes the \code{\link{sign}} function.

  A large sample normal approximation to the distribution of the linear rank statistic
  \eqn{L} for arbitrary score functions is given by Hettmansperger (1984, p.148).
  Under the null hypothesis (17) or (20), the mean and variance of \eqn{L} are given by:
  \deqn{E(L) = \mu_L = \frac{m}{N} \sum_{i=1}^N a_i = m \bar{a} \;\;\;\;\;\; (24)}
  \deqn{Var(L) = \sigma_L^2 = \frac{mn}{N(N-1)} \sum_{i=1}^N (a_i - \bar{a})^2 \;\;\;\;\;\; (25)}
  Hettmansperger (1984, Chapter 3) shows that under the null hypothesis of no
  difference between the two groups, the statistic
  \deqn{z = \frac{L - \mu_L}{\sigma_L} \;\;\;\;\;\; (26)}
  is approximately distributed as a standard normal random variable for
  \dQuote{large} sample sizes.  This statistic will tend to be large if the
  observations in group 1 tend to be larger than the observations in group 2.
}
\value{
  a list of class \code{"htestEnvStats"} containing the results of the hypothesis test.
  See the help file for \code{\link{htestEnvStats.object}} for details.
}
\references{
  Conover, W.J. (1980).  \emph{Practical Nonparametric Statistics}.  Second Edition.
  John Wiley and Sons, New York, Chapter 4.

  Divine, G., H.J. Norton, R. Hunt, and J. Dinemann. (2013).  A Review of Analysis
  and Sample Size Calculation Considerations for Wilcoxon Tests.  \emph{Anesthesia
  & Analgesia} \bold{117}, 699--710.

  Hettmansperger, T.P. (1984).  \emph{Statistical Inference Based on Ranks}.
  John Wiley and Sons, New York, 323pp.

  Hollander, M., and D.A. Wolfe. (1999). \emph{Nonparametric Statistical Methods,
  Second Edition}.  John Wiley and Sons, New York.

  Millard, S.P., and S.J. Deverel. (1988).  Nonparametric Statistical Methods for
  Comparing Two Sites Based on Data With Multiple Nondetect Limits.
  \emph{Water Resources Research}, \bold{24}(12), 2087--2098.

  Millard, S.P., and N.K. Neerchal. (2001).  \emph{Environmental Statistics with
  S-PLUS}.  CRC Press, Boca Raton, FL, pp.432--435.

  Prentice, R.L. (1985).  Linear Rank Tests.  In Kotz, S., and N.L. Johnson, eds.
  \emph{Encyclopedia of Statistical Science}.  John Wiley and Sons, New York.
  Volume 5, pp.51--58.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
  U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition.
  Prentice-Hall, Upper Saddle River, NJ.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The \link[=wilcox.test]{Wilcoxon Rank Sum test}, also known as the Mann-Whitney U
  test, is the standard nonparametric test used to test for differences between two
  groups (e.g., Zar, 2010; USEPA, 2009, pp.16-14 to 16-20).  Other possible
  nonparametric tests include linear rank tests based on scores other than the ranks,
  including the \dQuote{normal scores} test and the \dQuote{Savage scores} tests.
  The normal scores test is actually slightly more powerful than the Wilcoxon Rank Sum
  test for detecting small shifts in location if the underlying distribution is normal
  or lognormal.  In general, however, there will be little difference between these
  two tests.

  The results of calling the function \code{twoSampleLinearRankTest} with the
  argument \code{test="wilcoxon"} will match those of calling the built-in
  \R function \code{\link{wilcox.test}} with the arguments \code{exact=FALSE} and
  \code{correct=FALSE}.  In general, it is better to use the built-in function
  \code{\link{wilcox.test}} for performing the Wilcoxon Rank Sum test, since this
  function can compute exact (rather than approximate) p-values.
}
\seealso{
  \code{\link{wilcox.test}}, \code{\link{twoSampleLinearRankTestCensored}},
  \code{\link{htestEnvStats.object}}.
}
\examples{
  # Generate 15 observations from a normal distribution with parameters
  # mean=3 and sd=1.  Call these the observations from the reference group.
  # Generate 10 observations from a normal distribution with parameters
  # mean=3.5 and sd=1.  Call these the observations from the treatment group.
  # Compare the results of calling wilcox.test to those of calling
  # twoSampleLinearRankTest with test="normal.scores".
  # (The call to set.seed allows you to reproduce this example.)

  set.seed(346)
  x <- rnorm(15, mean = 3)
  y <- rnorm(10, mean = 3.5)

  wilcox.test(x, y)

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 location shift = 0
  #
  #Alternative Hypothesis:          True location shift is not equal to 0
  #
  #Test Name:                       Wilcoxon rank sum test
  #
  #Data:                            x and y
  #
  #Test Statistic:                  W = 32
  #
  #P-value:                         0.0162759


  twoSampleLinearRankTest(x, y, test = "normal.scores")

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 Fy(t) = Fx(t)
  #
  #Alternative Hypothesis:          Fy(t) != Fx(t) for at least one t
  #
  #Test Name:                       Two-Sample Linear Rank Test:
  #                                 Normal Scores Test
  #                                 Based on Normal Approximation
  #
  #Data:                            x = x
  #                                 y = y
  #
  #Sample Sizes:                    nx = 15
  #                                 ny = 10
  #
  #Test Statistic:                  z = -2.431099
  #
  #P-value:                         0.01505308

  #----------

  # Clean up
  #---------
  rm(x, y)

  #==========

  # Following Example 6.6 on pages 6.22-6.26 of USEPA (1994b), perform the
  # Wilcoxon Rank Sum test for the TcCB data (stored in EPA.94b.tccb.df).
  # There are m=47 observations from the reference area and n=77 observations
  # from the cleanup unit.  Then compare the results using the other available
  # linear rank tests.  Note that Mood's median test yields a p-value less
  # than 0.10, while the other tests yield non-significant p-values.
  # In this case, Mood's median test is picking up the residual contamination
  # in the cleanup unit. (See the example in the help file for quantileTest.)

  names(EPA.94b.tccb.df)
  #[1] "TcCB.orig" "TcCB"      "Censored"  "Area"

  summary(EPA.94b.tccb.df$Area)
  #  Cleanup Reference
  #       77        47

  with(EPA.94b.tccb.df,
    twoSampleLinearRankTest(TcCB[Area=="Cleanup"], TcCB[Area=="Reference"]))

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 Fy(t) = Fx(t)
  #
  #Alternative Hypothesis:          Fy(t) != Fx(t) for at least one t
  #
  #Test Name:                       Two-Sample Linear Rank Test:
  #                                 Wilcoxon Rank Sum Test
  #                                 Based on Normal Approximation
  #
  #Data:                            x = TcCB[Area == "Cleanup"]
  #                                 y = TcCB[Area == "Reference"]
  #
  #Sample Sizes:                    nx = 77
  #                                 ny = 47
  #
  #Test Statistic:                  z = -1.171872
  #
  #P-value:                         0.2412485

  with(EPA.94b.tccb.df,
    twoSampleLinearRankTest(TcCB[Area=="Cleanup"],
      TcCB[Area=="Reference"], test="normal.scores"))$p.value
  #[1] 0.3399484

  with(EPA.94b.tccb.df,
    twoSampleLinearRankTest(TcCB[Area=="Cleanup"],
      TcCB[Area=="Reference"], test="moods.median"))$p.value
  #[1] 0.09707393

  with(EPA.94b.tccb.df,
    twoSampleLinearRankTest(TcCB[Area=="Cleanup"],
      TcCB[Area=="Reference"], test="savage.scores"))$p.value
  #[1] 0.2884351
}
\keyword{htestEnvStats}
\keyword{nonparametric}
\keyword{regression}

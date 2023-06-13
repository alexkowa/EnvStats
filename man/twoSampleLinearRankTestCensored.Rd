\name{twoSampleLinearRankTestCensored}
\alias{twoSampleLinearRankTestCensored}
\title{
  Two-Sample Linear Rank Test to Detect a Difference Between Two Distributions Based on Censored Data
}
\description{
  Two-sample linear rank test to detect a difference (usually a shift) between two
  distributions based on censored data.
}
\usage{
  twoSampleLinearRankTestCensored(x, x.censored, y, y.censored,
    censoring.side = "left", location.shift.null = 0, scale.shift.null = 1,
    alternative = "two.sided", test = "logrank", variance = "hypergeometric",
    surv.est = "prentice", shift.type = "location")
}
\arguments{
  \item{x}{
  numeric vector of values for the first sample.
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf},
  \code{-Inf}) values are allowed but will be removed.
}
  \item{x.censored}{
  numeric or logical vector indicating which values of \code{x} are censored.
  This must be the same length as \code{x}.  If the mode of \code{x.censored} is
  \code{"logical"}, \code{TRUE} values correspond to elements of \code{x} that are
  censored, and \code{FALSE} values correspond to elements of \code{x} that are not
  censored.  If the mode of \code{x.censored} is \code{"numeric"}, it must contain only
  \code{1}'s and \code{0}'s; \code{1} corresponds to \code{TRUE} and \code{0}
  corresponds to \code{FALSE}.  Missing (\code{NA}) values are allowed but will be
  removed.
}
  \item{y}{
  numeric vector of values for the second sample.
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf},
  \code{-Inf}) values are allowed but will be removed.
}
  \item{y.censored}{
  numeric or logical vector indicating which values of \code{y} are censored.
  This must be the same length as \code{y}.  If the mode of \code{y.censored} is
  \code{"logical"}, \code{TRUE} values correspond to elements of \code{y} that are
  censored, and \code{FALSE} values correspond to elements of \code{y} that are not
  censored.  If the mode of \code{y.censored} is \code{"numeric"}, it must contain only
  \code{1}'s and \code{0}'s; \code{1} corresponds to \code{TRUE} and \code{0}
  corresponds to \code{FALSE}.  Missing (\code{NA}) values are allowed but will be
  removed.
}
  \item{censoring.side}{
  character string indicating on which side the censoring occurs for the data in
  \code{x} and \code{y}.  The possible values are \code{"left"} (the default) and
  \code{"right"}.
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
  \code{"logrank"} (the default), \code{"tarone-ware"}, \code{"gehan"},
  \code{"peto-peto"}, \code{"normal.scores.1"}, \code{"normal.scores.2"},
  and \code{"generalized.sign"}.  See the DETAILS section below for more information.
}
  \item{variance}{
  character string indicating which kind of variance to compute for the test.  The
  possible values are: \code{"hypergeometric"} (the default), \code{"permutation"},
  and \code{"asymptotic"}.  See the DETAILS section below for more information.
}
  \item{surv.est}{
  character string indicating what method to use to estimate the survival function.
  The possible values are \code{"prentice"} (the default), \code{"kaplan-meier"}, \cr
  \code{"peto-peto"}, and \code{"altshuler"}.  When \code{test="logrank"} the
  argument \cr
  \code{surv.est} is automatically set to \code{"altshuler"} and cannot
  be changed by the user.
  See the DETAILS section below for more information.
}
  \item{shift.type}{
  character string indicating which kind of shift is being tested.  The possible values
  are \code{"location"} (the default) and \code{"scale"}.
}
}
\details{
  The function \code{twoSampleLinearRankTestCensored} allows you to compare two
  samples containing censored observations using a linear rank test to determine
  whether the two samples came from the same distribution.  The help file for
  \code{\link{twoSampleLinearRankTest}} explains linear rank tests for complete data
  (i.e., no censored observations are present), and here we assume you are
  familiar with that material  The sections below explain how linear
  rank tests can be extended to the case of censored data.
  \cr

  \bold{\emph{Notation}} \cr
  Several authors have proposed extensions of the MWW test to the case of censored
  data, mainly in the context of survival analysis (e.g., Breslow, 1970; Cox, 1972;
  Gehan, 1965; Mantel, 1966; Peto and Peto, 1972; Prentice, 1978).  Prentice (1978)
  showed how all of these proposed tests are extensions of a linear rank test to the
  case of censored observations.

  Survival analysis usually deals with right-censored data, whereas environmental data
  is rarely right-censored but often left-censored (some observations are reported as
  less than some detection limit).  Fortunately, all of the methods developed for
  right-censored data can be applied to left-censored data as well.  (See the
  sub-section \emph{Left-Censored Data below}.)

  In order to explain Prentice's (1978) generalization of linear rank tests to censored
  data, we will use the following notation that closely follows Prentice (1978),
  Prentice and Marek (1979), and Latta (1981).
  Let \eqn{X} denote a random variable representing measurements from group 1 with
  cumulative distribution function (cdf):
  \deqn{F_1(t) = Pr(X \le t) \;\;\;\;\;\; (1)}
  and let \eqn{x_1, x_2, \ldots, x_m} denote \eqn{m} independent observations from this
  distribution.  Let \eqn{Y} denote a random variable from group 2 with cdf:
  \deqn{F_2(t) = Pr(Y \le t) \;\;\;\;\;\; (2)}
  and let \eqn{y_1, y_2, \ldots, y_n} denote \eqn{n} independent observations from this
  distribution.  Set \eqn{N = m + n}, the total number of observations.
  Assume the data are right-censored so that some observations are only recorded as
  greater than some censoring level, with possibly several different censoring levels.
  Let \eqn{t_1, t_2, \ldots, t_k} denote the \eqn{k} ordered, unique, uncensored
  observations for the combined samples (in the context of survival data, \eqn{t} usually stands
  for \dQuote{time of death}).  For \eqn{i = 1, 2, \ldots, k}, let \eqn{d_{1i}}
  denote the number of observations from sample 1 (the \eqn{X} observations) that are
  equal to \eqn{t_i}, and let \eqn{d_{2i}} denote the observations from sample 2 (the
  \eqn{Y} observations) equal to this value.  Set
  \deqn{d_i = d_{1i} + d_{2i} \;\;\;\;\;\; (3)}
  the total number of observations equal to \eqn{t_i}.  If there are no tied
  uncensored observations, then \eqn{t_i = 1} for \eqn{i = 1, 2, \ldots, k},
  otherwise it is greater than 1 for at least on value of \eqn{i}.

  For \eqn{i = 1, 2, \ldots, k}, let \eqn{e_{1i}} denote the number of censored
  observations from sample 1 (the \eqn{X} observations) with censoring levels that
  fall into the interval \eqn{[t_i, t_{i+1})} where \eqn{t_{k+1} = \infty} by
  definition, and let \eqn{e_{2i}} denote the number of censored observations from
  sample 2 (the \eqn{Y} observations) with censoring levels that fall into this
  interval.  Set
  \deqn{e_i = e_{1i} + e_{2i} \;\;\;\;\;\; (4)}
  the total number of censoring levels that fall into this interval.

  Finally, set \eqn{n_{1i}} equal to the number of observations from sample 1
  (uncensored and censored) known to be greater than or equal to \eqn{t_i}, i.e.,
  that lie in the interval \eqn{[t_i, \infty)},
  set \eqn{n_{2i}} equal to the number of observations from sample 2
  (uncensored and censored) that lie in this interval, and set
  \deqn{n_i = n_{1i} + n_{2i} \;\;\;\;\;\; (5)}
  In survival analysis jargon, \eqn{n_{1i}} denotes the number of people from
  sample 1 who are \dQuote{at risk} at time \eqn{t_i}, that is, these people are
  known to still be alive at this time.  Similarly, \eqn{n_{2i}} denotes the number
  of people from sample 2 who are at risk at time \eqn{t_i}, and \eqn{n_i} denotes
  the total number of people at risk at time \eqn{t_i}.
  \cr

  \bold{\emph{Score Statistics for Multiply Censored Data}} \cr
  Prentice's (1978) generalization of the two-sample score (linear rank) statistic is
  given by:
  \deqn{\nu = \sum_{i=1}^k (d_{1i} c_i + e_{1i} C_i) \;\;\;\;\;\; (6)}
  where \eqn{c_i} and \eqn{C_i} denote the scores associated with the uncensored and
  censored observations, respectively.  As for complete data, the form of the scores
  depends upon the assumed underlying distribution.  Table 1 below shows scores for
  various assumed distributions as presented in Prentice (1978) and Latta (1981)
  (also see Table 5 of Millard and Deverel, 1988, p.2091).
  The last column shows what these tests reduce to in the case of complete data
  (no censored observations).

  \emph{Table 1.  Scores Associated with Various Censored Data Rank Tests}\cr
  \tabular{lllll}{
                      \tab \bold{Uncensored}           \tab \bold{Censored}                                         \tab                     \tab \bold{Uncensored} \cr
  \bold{Distribution} \tab \bold{Score} (\eqn{c_i})    \tab \bold{Score} (\eqn{C_i})                                \tab \bold{Test Name}    \tab \bold{Analogue}   \cr
  Logistic            \tab \eqn{2\hat{F}_i - 1}        \tab \eqn{\hat{F}_i}                                         \tab Peto-Peto           \tab Wilcoxon Rank Sum \cr
                      \tab                             \tab                                                         \tab                     \tab                   \cr
      "               \tab \eqn{i - n_i}               \tab \eqn{i}                                                 \tab Gehan or Breslow    \tab        "          \cr
                      \tab                             \tab                                                         \tab                     \tab                   \cr
      "               \tab \eqn{i - \sqrt{n_i}}        \tab \eqn{i}                                                 \tab Tarone-Ware         \tab        "          \cr
                      \tab                             \tab                                                         \tab                     \tab                   \cr
                      \tab                             \tab                                                         \tab                     \tab                   \cr
  Normal,             \tab \eqn{\Phi^{-1}(\hat{F}_i)}  \tab \eqn{\phi(c_i)/\hat{S}_i}                               \tab Normal Scores 1     \tab Normal Scores     \cr
  Lognormal           \tab                             \tab                                                         \tab                     \tab                   \cr
      "               \tab        "                    \tab \eqn{\frac{n_i C_{i-1} - c_i}{n_i-1}}                   \tab Normal Scores 2     \tab        "          \cr
                      \tab                             \tab                                                         \tab                     \tab                   \cr
                      \tab                             \tab                                                         \tab                     \tab                   \cr
  Double              \tab \eqn{sign(\hat{F}_i - 0.5)} \tab \eqn{\frac{\hat{F}_i}{1-\hat{F}_i}, if \hat{F_i} < 0.5} \tab Generalized         \tab Mood's Median     \cr
  Exponential         \tab                             \tab \eqn{1, if \hat{F}_i \ge 0.5}                           \tab Sign                \tab                   \cr
                      \tab                             \tab                                                         \tab                     \tab                   \cr
                      \tab                             \tab                                                         \tab                     \tab                   \cr
  Exponential,        \tab \eqn{-log(\tilde{S}_i) - 1} \tab \eqn{-log(\tilde{S}_i)}                                 \tab Logrank             \tab Savage Scores     \cr
  Extreme Value       \tab                             \tab                                                         \tab                     \tab
  }

  In Table 1 above, \eqn{\Phi} denotes the cumulative distribution function of the
  standard normal distribution, \eqn{\phi} denotes the probability density function
  of the standard normal distribution, and \eqn{sign} denotes the \code{\link{sign}}
  function.  Also, the quantities \eqn{\hat{F}_i} and \eqn{\hat{S}_i}
  denote the estimates of the cumulative distribution function (cdf) and survival
  function, respectively, at time \eqn{t_i} for the combined sample.  The estimated
  cdf is related to the estimated survival function by:
  \deqn{\hat{F}_i = 1 - \hat{S}_i \;\;\;\;\;\; (7)}
  The quantity \eqn{\tilde{S}_i} denotes the Altshuler (1970) estimate of the
  survival function at time \eqn{t_i} for the combined sample (see below).

  The argument \code{surv.est} determines what method to use estimate the survival
  function.  When \code{surv.est="prentice"} (the default), the survival function is
  estimated as:
  \deqn{\hat{S}_{i,P} = \prod_{j=1}^{i} \frac{n_j - d_j + 1}{n_j + 1} \;\;\;\;\;\; (8)}
  (Prentice, 1978).  When \code{surv.est="kaplan-meier"}, the survival function is
  estimated as:
  \deqn{\hat{S}_{i,KM} = \prod_{j=1}^{i} \frac{n_j - d_j}{n_j} \;\;\;\;\;\; (9)}
  (Kaplan and Meier, 1958), and when \code{surv.est="peto-peto"}, the survival
  function is estimated as:
  \deqn{\hat{S}_{i,PP} = \frac{1}{2} (\hat{S}_{i,KM} + \hat{S}_{i-1, KM}) \;\;\;\;\;\; (10)}
  where \eqn{\hat{S}_{0,KM} = 0} (Peto and Peto, 1972).  All three of these estimators
  of the survival function should produce very similar results.  When
  \code{surv.est="altshuler"}, the survival function is estimated as:
  \deqn{\tilde{S}_i = exp(-\sum_{j=1}^i \frac{d_j}{n_j}) \;\;\;\;\;\; (11)}
  (Altshuler, 1970).  The scores for the logrank test use this estimator of survival.

  Lee and Wang (2003, p. 116) present a slightly different version of the Peto-Peto
  test. They use the Peto-Peto estimate of the survival function for \eqn{c_i}, but
  use the Kaplan-Meier estimate of the survival function for \eqn{C_i}.

  The scores for the \dQuote{Normal Scores 1} test shown in Table 1 above are based on
  the approximation (30) of Prentice (1978).  The scores for the
  \dQuote{Normal Scores 2} test are based on equation (7) of Prentice and Marek (1979).
  For the \dQuote{Normal Scores 2} test, the following rules are used to construct the
  scores for the censored observations:  \eqn{C_0 = 0}, and
  \eqn{C_k = 0} if \eqn{n_k = 1}.

  \bold{\emph{The Distribution of the Score Statistic}} \cr
  Under the null hypothesis that the two distributions are the same, the expected
  value of the score statistic \eqn{\nu} in Equation (6) is 0.  The variance of
  \eqn{\nu} can be computed in at least three different ways.  If the censoring
  mechanism is the same for both groups, the \bold{\emph{permutation variance}} is
  appropriate (\code{variance="permutation"}) and its estimate is given by:
  \deqn{\hat{\sigma}_{\nu}^2 = \frac{mn}{N(N-1)} \sum_{i=1}^k (d_i c_i^2 + e_i C_i^2) \;\;\;\;\;\; (12)}
  Often, however, it is not clear whether this assumption is valid, and
  both Prentice (1978) and Prentice and Marek (1979) caution against
  using the permuation variance (Prentice and Marek, 1979, state it can lead to
  inflated estimates of variance).

  If the censoring mechanisms for the two groups are not necessarily the same, a more
  general estimator of the variance is based on a conditional permutation approach.  In
  this case, the statistic \eqn{\nu} in Equation (6) is re-written as:
  \deqn{\nu = \sum_{i=1}^k w_i [d_{1i} - d_i \frac{n_{1i}}{n_i}] \;\;\;\;\;\; (13)}
  where
  \deqn{w_i = c_i - C_i \;\;\;\;\;\; (14)}
  \eqn{c_i} and \eqn{C_i} are given above in Table 1,
  and the conditional permutation or \bold{\emph{hypergeometric estimate}}
  (\code{variance="hypergeometric"}) is given by:
  \deqn{\hat{\sigma}_{\nu}^2 = \sum_{i=1}^k d_i w_i^2 (\frac{n_{1i}}{n_i}) (1 - \frac{n_{1i}}{n_i}) (\frac{n_i - d_i}{n_i - 1}) \;\;\;\;\;\; (15)}
  (Prentice and Marek, 1979; Latta, 1981; Millard and Deverel, 1988).  Note that
  Equation (13) can be thought of as the sum of weighed values of observed minus
  expected observations.

  Prentice (1978) derived an asymptotic estimator of the variance of the score
  statistic \eqn{\nu} given in Equation (6) above based on the log likelihood of the
  rank vector (\code{variance="asymptotic"}).  This estimator is the same as the
  hypergeometric variance estimator for the logrank and Gehan tests (assuming no
  tied uncensored observations), but for the Peto-Peto test, this estimator is
  given by:
  \deqn{\hat{\sigma}_{\nu}^2 = \sum_{i=1}^k \{ \hat{S}_i (1 - a_i) b_i - (a_i - \hat{S}_i) b_i [\hat{S}_i b_i + 2 \sum_{j=i+1}^k \hat{S}_j b_j ]\} \;\;\;\;\;\; (16)}
  where
  \deqn{a_i = \prod_{j=1}^i \frac{n_j + 1}{n_j + 2} \;\;\;\;\;\; (17)}
  \deqn{b_i = 2 d_{1i} + e_{1i} \;\;\;\;\;\; (18)}
  (Prentice, 1978; Latta, 1981; Millard and Deverel, 1988).  Note that equation (14)
  of Millard and Deverel (1988) contains a typographical error.
  \cr

  \bold{\emph{The Treatment of Ties}} \cr
  If the hypergeometric estimator of variance is being used, no modifications need to
  be made for ties; Equations (13)-(15) already account for ties.  For the case of the
  permutation or asymptotic variance estimators, Equations (6), (12), and (16) all
  assume no ties in the uncensored observations.  If ties exist in the uncensored
  observations, Prentice (1978) suggests computing the scores shown in Table 1
  above as if there were no ties, and then assigning average scores to the
  tied observations.  (This modification also applies to the quantities
  \eqn{a_i} and \eqn{\hat{S}_i} in Equation (16) above.)  For this algorithm, the
  statistic in Equation (6) is not in general the same as the one in Equation (13).
  \cr

  \bold{\emph{Computing a Test Statistic}} \cr
  Under the null hypothesis that the two distributions are the same, the statistic
  \deqn{z = \frac{\nu}{\hat{\sigma}_{\nu}} \;\;\;\;\;\; (19)}
  is approximately distributed as a standard normal random variable for \dQuote{large}
  sample sizes.  \bold{This statistic will tend to be large if the observations in
  group 1 (the \eqn{X} observations) tend to be larger than the observations in
  group 2 (the \eqn{Y} observations)}.
  \cr

  \bold{\emph{Left-Censored Data}} \cr
  Most of the time, if censored observations occur in environmental data, they are
  left-censored (e.g., observations are reported as less than one or more detection
  limits).  For the two-sample test of differences between groups, the methods that
  apply to right-censored data are easily adapted to left-censored data:  simply
  multiply the observations by -1, compute the z-statistic shown in Equation
  (20), then reverse the sign of this statistic before computing the p-value.
}
\value{
  a list of class \code{"htestCensored"} containing the results of the hypothesis test.
  See the help file for \code{\link{htestCensored.object}} for details.
}
\references{
  Altshuler, B. (1970).  Theory for the Measurement of Competing Risks in Animal
  Experiments.  \emph{Mathematical Biosciences} \bold{6}, 1--11.

  Breslow, N.E. (1970).  A Generalized Kruskal-Wallis Test for Comparing K Samples
  Subject to Unequal Patterns of Censorship.  \emph{Biometrika} \bold{57}, 579--594.

  Conover, W.J. (1980).  \emph{Practical Nonparametric Statistics}.  Second Edition.
  John Wiley and Sons, New York, Chapter 4.

  Cox, D.R. (1972).  Regression Models and Life Tables (with Discussion).
  \emph{Journal of the Royal Statistical Society of London, Series B} \bold{34},
  187--220.

  Divine, G., H.J. Norton, R. Hunt, and J. Dinemann. (2013).  A Review of Analysis
  and Sample Size Calculation Considerations for Wilcoxon Tests.  \emph{Anesthesia
  & Analgesia} \bold{117}, 699--710.

  Fleming, T.R., and D.P. Harrington. (1981).  A Class of Hypothesis Tests for One and
  Two Sample Censored Survival Data.
  \emph{Communications in Statistics -- Theory and Methods} \bold{A10}(8), 763--794.

  Fleming, T.R., and D.P. Harrington. (1991).
  \emph{Counting Processes & Survival Analysis}.  John Wiley and Sons, New York,
  Chapter 7.

  Gehan, E.A. (1965).  A Generalized Wilcoxon Test for Comparing Arbitrarily
  Singly-Censored Samples.  \emph{Biometrika} \bold{52}, 203--223.

  Harrington, D.P., and T.R. Fleming. (1982).  A Class of Rank Test Procedures for
  Censored Survival Data.  \emph{Biometrika} \bold{69}(3), 553--566.

  Heller, G., and E. S. Venkatraman. (1996).  Resampling Procedures to Compare Two
  Survival Distributions in the Presence of Right-Censored Data.
  \emph{Biometrics} \bold{52}, 1204--1213.

  Hettmansperger, T.P. (1984).  \emph{Statistical Inference Based on Ranks}.
  John Wiley and Sons, New York, 323pp.

  Hollander, M., and D.A. Wolfe. (1999). \emph{Nonparametric Statistical Methods,
  Second Edition}.  John Wiley and Sons, New York.

  Kaplan, E.L., and P. Meier. (1958).  Nonparametric Estimation From Incomplete
  Observations.  \emph{Journal of the American Statistical Association} \bold{53},
  457--481.

  Latta, R.B. (1981).  A Monte Carlo Study of Some Two-Sample Rank Tests with Censored
  Data.  \emph{Journal of the American Statistical Association} \bold{76}(375),
  713--719.

  Mantel, N. (1966).  Evaluation of Survival Data and Two New Rank Order Statistics
  Arising in its Consideration.  \emph{Cancer Chemotherapy Reports} \bold{50}, 163-170.

  Millard, S.P., and S.J. Deverel. (1988).  Nonparametric Statistical Methods for
  Comparing Two Sites Based on Data With Multiple Nondetect Limits.
  \emph{Water Resources Research}, \bold{24}(12), 2087--2098.

  Millard, S.P., and N.K. Neerchal. (2001).  \emph{Environmental Statistics with
  S-PLUS}.  CRC Press, Boca Raton, FL, pp.432--435.

  Peto, R., and J. Peto. (1972).  Asymptotically Efficient Rank Invariant Test
  Procedures (with Discussion).
  \emph{Journal of the Royal Statistical Society of London, Series A} \bold{135},
  185--206.

  Prentice, R.L. (1978).  Linear Rank Tests with Right Censored Data.
  \emph{Biometrika} \bold{65}, 167--179.

  Prentice, R.L. (1985).  Linear Rank Tests.  In Kotz, S., and N.L. Johnson, eds.
  \emph{Encyclopedia of Statistical Science}.  John Wiley and Sons, New York.
  Volume 5, pp.51--58.

  Prentice, R.L., and P. Marek. (1979).  A Qualitative Discrepancy Between Censored
  Data Rank Tests.  \emph{Biometrics} \bold{35}, 861--867.

  Tarone, R.E., and J. Ware. (1977).  On Distribution-Free Tests for Equality of
  Survival Distributions.  \emph{Biometrika} \bold{64}(1), 156--160.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
  U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  All of the tests computed by \code{twoSampleLinearRankTestCensored}
  (logrank, Tarone-Ware, Gehan, Peto-Peto, normal scores, and generalized sign)
  are based on a
  statistic that is essentially the sum over all uncensored time points of the
  weighted difference between the observed and expected number of observations at each
  time point (see Equation (15) above).  The tests differ in how they weight the
  differences between the observed and expected number of observations.

  Prentice and Marek (1979) point out that the Gehan test uses weights that depend on
  the censoring rates within each group and can lead to non-significant outcomes in
  the case of heavy censoring when in fact a very large difference between the two
  groups exists.

  Latta (1981) performed a Monte Carlo simulation to study the power of the Gehan,
  logrank, and Peto-Peto tests using all three different estimators of variance
  (permutation, hypergeometric, and asymptotic).  He used lognormal, Weibull, and
  exponential distributions to generate the observations, and studied two different
  cases of censoring: uniform censoring for both samples vs. no censoring in the first
  sample and uniform censoring in the second sample.  Latta (1981) used sample sizes
  of 10 and 50 (both the equal and unequal cases were studied).  Latta (1981) found
  that all three tests maintained the nominal Type I error level (\eqn{\alpha}-level)
  in the case of equal sample sizes and equal censoring.  Also, the Peto-Peto test
  based on the asymptotic variance appeared to maintain the nominal \eqn{\alpha}-level
  in all situations, but the other tests were slightly biased in the case of unequal
  sample sizes and/or unequal censoring.  In particular, tests based on the
  hypergeometric variance are slightly biased for unequal sample sizes.  Latta (1981)
  concludes that if there is no censoring or light censoring, any of the tests may be
  used (but the hypergeometric variance should not be used if the sample sizes are
  very different).  In the case of heavy censoring where sample sizes are far apart
  and/or the censoring is very different between samples, the Peto-Peto test based on
  the asymptotic variance should be used.

  Millard and Deverel (1988) also performed a Monte Carlo simulation similar to
  Latta's (1981) study.  They only used the lognormal distribution to generate
  observations, but also looked at the normal scores test and two ad-hoc modifications
  of the MWW test.  They found the \dQuote{Normal Scores 2} test shown in Table 1
  above to be the best behaved test in terms of maintaining the nominal
  \eqn{\alpha}-level, but the other tests behaved almost as well.  As Latta (1981)
  found, when sample sizes and censoring are very different between the two groups,
  the nominal \eqn{\alpha}-level of most of the tests is slightly biased.  In the
  cases where the nominal \eqn{\alpha}-level was maintained, the Peto-Peto test based
  on the asymptotic variance appeared to be as powerful or more powerful than the
  normal scores tests.

  Neither of the Monte Carlo studies performed by Latta (1981) and Millard and Deverel
  (1988) looked at the behavior of the two-sample linear rank tests in the presence of
  several tied uncensored observations (because both studies generated observations
  from continuous distributions).  Note that the results shown in Table 9 of
  Millard and Deverel (1988, p.2097) are not all correct because they did not
  allow for tied uncensored values.  The last example in the EXAMPLES section below
  shows the correct values that should appear in that table.

  Heller and Venkatraman (1996) performed a Monte Carlo simulation study to compare
  the behaviors of the Peto-Peto test (using the Prentice, 1978, estimator of survival;
  they call this the Prentice-Wilcoxon test)
  and logrank test under varying censoring conditions with sample sizes of 20 and 50
  per group based on using the following methods to compute p-values:
  the asymptotic standard normal approximation,
  a permutation test approach (this is \bold{NOT} the same as the permutation variance),
  and a bootstrap approach.  Observed times were generated from Weibull and lognormal
  survival time distributions with independent uniform censoring.  They found that
  for the Peto-Peto test, "the asymptotic test procedure was the most accurate;
  resampling procedures did not improve upon its accuracy."  For the logrank test,
  with sample sizes of 20 per group, the usual test based on the asymptotic standard
  normal approximation tended to have a very slightly higher Type I error rate than
  assumed (however, for an assumed Type I error rate of 0.05, the largest Type I error
  rate observed was less than 0.065), whereas the permuation and bootstrap tests
  performed better; with sample sizes of 50 per group there was no difference in
  test performance.

  Fleming and Harrington (1981) introduced a family of tests (sometimes called G-rho
  tests) that contain the logrank and Peto-Peto tests as special cases.  A single
  parameter \eqn{\rho} (rho) controls the weights given to the uncensored and
  censored observations.  Positive values of \eqn{\rho} produce tests more sensitive
  to early differences in the survival function, that is, differences in the cdf at
  small values.  Negative values of \eqn{\rho} produce tests more sensitive to late
  differences in the survival function, that is, differences in the cdf at large
  values.

  The function \code{\link[survival:survdiff]{survdiff}} in the \R package
  \pkg{survival} implements the G-rho family of tests suggested by Flemming and
  Harrington (1981).  Calling \code{survdiff} with \code{rho=0} (the default) yields
  the logrank test.  Calling \code{survdiff} with \code{rho=1} yields the Peto-Peto
  test based on the Kaplan-Meier estimate of survival.  The function \code{survdiff}
  always uses the hypergeometric estimate of variance and the Kaplan-Meier estimate of
  survival, but it uses the \dQuote{left-continuous} version of the Kaplan-Meier
  estimate.  The left-continuous K-M estimate of survival is defined as
  follows:  at each death (unique uncensored observation), the estimated survival is
  equal to the estimated survival based on the ordinary K-M estimate at the prior
  death time (or 1 for the first death).
}
\seealso{
  \code{\link{twoSampleLinearRankTest}}, \code{\link[survival:survdiff]{survdiff}},
  \code{\link{wilcox.test}}, \code{\link{htestCensored.object}}.
}
\examples{
  # The last part of the EXAMPLES section in the help file for
  # cdfCompareCensored compares the empirical distribution of copper and zinc
  # between two sites:  Alluvial Fan and Basin-Trough (Millard and Deverel, 1988).
  # The data for this example are stored in Millard.Deverel.88.df.  Perform a
  # test to determine if there is a significant difference between these two
  # sites (perform a separate test for the copper and the zinc).

  Millard.Deverel.88.df
  #    Cu.orig Cu Cu.censored Zn.orig  Zn Zn.censored         Zone Location
  #1       < 1  1        TRUE     <10  10        TRUE Alluvial.Fan        1
  #2       < 1  1        TRUE       9   9       FALSE Alluvial.Fan        2
  #3         3  3       FALSE      NA  NA       FALSE Alluvial.Fan        3
  #.
  #.
  #.
  #116       5  5       FALSE      50  50       FALSE Basin.Trough       48
  #117      14 14       FALSE      90  90       FALSE Basin.Trough       49
  #118       4  4       FALSE      20  20       FALSE Basin.Trough       50


  #------------------------------
  # First look at the copper data
  #------------------------------

  Cu.AF <- with(Millard.Deverel.88.df,
    Cu[Zone == "Alluvial.Fan"])

  Cu.AF.cen <- with(Millard.Deverel.88.df,
    Cu.censored[Zone == "Alluvial.Fan"])

  Cu.BT <- with(Millard.Deverel.88.df,
    Cu[Zone == "Basin.Trough"])

  Cu.BT.cen <- with(Millard.Deverel.88.df,
    Cu.censored[Zone == "Basin.Trough"])

  # Note the large number of tied observations in the copper data
  #--------------------------------------------------------------

  table(Cu.AF[!Cu.AF.cen])
  # 1  2  3  4  5  7  8  9 10 11 12 16 20
  # 5 21  6  3  3  3  1  1  1  1  1  1  1

  table(Cu.BT[!Cu.BT.cen])
  # 1  2  3  4  5  6  8  9 12 14 15 17 23
  # 7  4  8  5  1  2  1  2  1  1  1  1  1


  # Logrank test with hypergeometric variance:
  #-------------------------------------------
  twoSampleLinearRankTestCensored(x = Cu.AF, x.censored = Cu.AF.cen,
    y = Cu.BT, y.censored = Cu.BT.cen)

  #Results of Hypothesis Test
  #Based on Censored Data
  #--------------------------
  #
  #Null Hypothesis:                 Fy(t) = Fx(t)
  #
  #Alternative Hypothesis:          Fy(t) != Fx(t) for at least one t
  #
  #Test Name:                       Two-Sample Linear Rank Test:
  #                                 Logrank Test
  #                                 with Hypergeometric Variance
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              x =  1  5 10 20
  #                                 y =  1  2  5 10 15
  #
  #Data:                            x = Cu.AF
  #                                 y = Cu.BT
  #
  #Censoring Variable:              x = Cu.AF.cen
  #                                 y = Cu.BT.cen
  #
  #Number NA/NaN/Inf's Removed:     x = 3
  #                                 y = 1
  #
  #Sample Sizes:                    nx = 65
  #                                 ny = 49
  #
  #Percent Censored:                x = 26.2%
  #                                 y = 28.6%
  #
  #Test Statistics:                 nu     = -1.8791355
  #                                 var.nu = 13.6533490
  #                                 z      = -0.5085557
  #
  #P-value:                         0.6110637


  # Compare the p-values produced by the Normal Scores 2 test
  # using the hypergeomtric vs. permutation variance estimates.
  # Note how much larger the estimated variance is based on
  # the permuation variance estimate:
  #-----------------------------------------------------------

  twoSampleLinearRankTestCensored(x = Cu.AF, x.censored = Cu.AF.cen,
    y = Cu.BT, y.censored = Cu.BT.cen,
    test = "normal.scores.2")$p.value
  #[1] 0.2008913

  twoSampleLinearRankTestCensored(x = Cu.AF, x.censored = Cu.AF.cen,
    y = Cu.BT, y.censored = Cu.BT.cen,
    test = "normal.scores.2", variance = "permutation")$p.value
  #[1] [1] 0.657001


  #--------------------------
  # Now look at the zinc data
  #--------------------------

  Zn.AF <- with(Millard.Deverel.88.df,
    Zn[Zone == "Alluvial.Fan"])

  Zn.AF.cen <- with(Millard.Deverel.88.df,
    Zn.censored[Zone == "Alluvial.Fan"])

  Zn.BT <- with(Millard.Deverel.88.df,
    Zn[Zone == "Basin.Trough"])

  Zn.BT.cen <- with(Millard.Deverel.88.df,
    Zn.censored[Zone == "Basin.Trough"])

  # Note the moderate number of tied observations in the zinc data,
  # and the "outlier" of 620 in the Alluvial Fan data.
  #---------------------------------------------------------------

  table(Zn.AF[!Zn.AF.cen])
  #  5   7   8   9  10  11  12  17  18  19  20  23  29  30  33  40  50 620
  #  1   1   1   1  20   2   1   1   1   1  14   1   1   1   1   1   1   1

  table(Zn.BT[!Zn.BT.cen])
  # 3  4  5  6  8 10 11 12 13 14 15 17 20 25 30 40 50 60 70 90
  # 2  2  2  1  1  5  1  2  1  1  1  2 11  1  4  3  2  2  1  1


  # Logrank test with hypergeometric variance:
  #-------------------------------------------
  twoSampleLinearRankTestCensored(x = Zn.AF, x.censored = Zn.AF.cen,
    y = Zn.BT, y.censored = Zn.BT.cen)

  #Results of Hypothesis Test
  #Based on Censored Data
  #--------------------------
  #
  #Null Hypothesis:                 Fy(t) = Fx(t)
  #
  #Alternative Hypothesis:          Fy(t) != Fx(t) for at least one t
  #
  #Test Name:                       Two-Sample Linear Rank Test:
  #                                 Logrank Test
  #                                 with Hypergeometric Variance
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              x =  3 10
  #                                 y =  3 10
  #
  #Data:                            x = Zn.AF
  #                                 y = Zn.BT
  #
  #Censoring Variable:              x = Zn.AF.cen
  #                                 y = Zn.BT.cen
  #
  #Number NA/NaN/Inf's Removed:     x = 1
  #                                 y = 0
  #
  #Sample Sizes:                    nx = 67
  #                                 ny = 50
  #
  #Percent Censored:                x = 23.9%
  #                                 y =  8.0%
  #
  #Test Statistics:                 nu     = -6.992999
  #                                 var.nu = 17.203227
  #                                 z      = -1.686004
  #
  #P-value:                         0.09179512

  #----------

  # Compare the p-values produced by the Logrank, Gehan, Peto-Peto,
  # and Tarone-Ware tests using the hypergeometric variance.
  #-----------------------------------------------------------

  twoSampleLinearRankTestCensored(x = Zn.AF, x.censored = Zn.AF.cen,
    y = Zn.BT, y.censored = Zn.BT.cen,
    test = "logrank")$p.value
  #[1] 0.09179512

  twoSampleLinearRankTestCensored(x = Zn.AF, x.censored = Zn.AF.cen,
    y = Zn.BT, y.censored = Zn.BT.cen,
    test = "gehan")$p.value
  #[1] 0.0185445

  twoSampleLinearRankTestCensored(x = Zn.AF, x.censored = Zn.AF.cen,
    y = Zn.BT, y.censored = Zn.BT.cen,
    test = "peto-peto")$p.value
  #[1] 0.009704529

  twoSampleLinearRankTestCensored(x = Zn.AF, x.censored = Zn.AF.cen,
    y = Zn.BT, y.censored = Zn.BT.cen,
    test = "tarone-ware")$p.value
  #[1] 0.03457803

  #----------

  # Clean up
  #---------

  rm(Cu.AF, Cu.AF.cen, Cu.BT, Cu.BT.cen,
     Zn.AF, Zn.AF.cen, Zn.BT, Zn.BT.cen)


  #==========

  # Example 16.5 on pages 16-22 to 16.23 of USEPA (2009) shows how to perform
  # the Tarone-Ware two sample linear rank test based on censored data using
  # observations on tetrachloroethylene (PCE) (ppb) collected at one background
  # and one compliance well.  The data for this example are stored in
  # EPA.09.Ex.16.5.PCE.df.

  EPA.09.Ex.16.5.PCE.df

  #    Well.type PCE.Orig.ppb PCE.ppb Censored
  #1  Background           <4     4.0     TRUE
  #2  Background          1.5     1.5    FALSE
  #3  Background           <2     2.0     TRUE
  #4  Background          8.7     8.7    FALSE
  #5  Background          5.1     5.1    FALSE
  #6  Background           <5     5.0     TRUE
  #7  Compliance          6.4     6.4    FALSE
  #8  Compliance         10.9    10.9    FALSE
  #9  Compliance            7     7.0    FALSE
  #10 Compliance         14.3    14.3    FALSE
  #11 Compliance          1.9     1.9    FALSE
  #12 Compliance           10    10.0    FALSE
  #13 Compliance          6.8     6.8    FALSE
  #14 Compliance           <5     5.0     TRUE

  with(EPA.09.Ex.16.5.PCE.df,
    twoSampleLinearRankTestCensored(
      x = PCE.ppb[Well.type == "Compliance"],
      x.censored = Censored[Well.type == "Compliance"],
      y = PCE.ppb[Well.type == "Background"],
      y.censored = Censored[Well.type == "Background"],
      test = "tarone-ware", alternative = "greater"))

  #Results of Hypothesis Test
  #Based on Censored Data
  #--------------------------
  #
  #Null Hypothesis:                 Fy(t) = Fx(t)
  #
  #Alternative Hypothesis:          Fy(t) > Fx(t) for at least one t
  #
  #Test Name:                       Two-Sample Linear Rank Test:
  #                                 Tarone-Ware Test
  #                                 with Hypergeometric Variance
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              x = 5
  #                                 y = 2 4 5
  #
  #Data:                            x = PCE.ppb[Well.type == "Compliance"]
  #                                 y = PCE.ppb[Well.type == "Background"]
  #
  #Censoring Variable:              x = Censored[Well.type == "Compliance"]
  #                                 y = Censored[Well.type == "Background"]
  #
  #Sample Sizes:                    nx = 8
  #                                 ny = 6
  #
  #Percent Censored:                x = 12.5%
  #                                 y = 50.0%
  #
  #Test Statistics:                 nu     =  8.458912
  #                                 var.nu = 20.912407
  #                                 z      =  1.849748
  #
  #P-value:                         0.03217495

  # Compare the p-value for the Tarone-Ware test with p-values from
  # the logrank, Gehan, and Peto-Peto tests
  #-----------------------------------------------------------------

  with(EPA.09.Ex.16.5.PCE.df,
    twoSampleLinearRankTestCensored(
      x = PCE.ppb[Well.type == "Compliance"],
      x.censored = Censored[Well.type == "Compliance"],
      y = PCE.ppb[Well.type == "Background"],
      y.censored = Censored[Well.type == "Background"],
      test = "tarone-ware", alternative = "greater"))$p.value
  #[1] 0.03217495


  with(EPA.09.Ex.16.5.PCE.df,
    twoSampleLinearRankTestCensored(
      x = PCE.ppb[Well.type == "Compliance"],
      x.censored = Censored[Well.type == "Compliance"],
      y = PCE.ppb[Well.type == "Background"],
      y.censored = Censored[Well.type == "Background"],
      test = "logrank", alternative = "greater"))$p.value
  #[1] 0.02752793


  with(EPA.09.Ex.16.5.PCE.df,
    twoSampleLinearRankTestCensored(
      x = PCE.ppb[Well.type == "Compliance"],
      x.censored = Censored[Well.type == "Compliance"],
      y = PCE.ppb[Well.type == "Background"],
      y.censored = Censored[Well.type == "Background"],
      test = "gehan", alternative = "greater"))$p.value
  #[1] 0.03656224

  with(EPA.09.Ex.16.5.PCE.df,
    twoSampleLinearRankTestCensored(
      x = PCE.ppb[Well.type == "Compliance"],
      x.censored = Censored[Well.type == "Compliance"],
      y = PCE.ppb[Well.type == "Background"],
      y.censored = Censored[Well.type == "Background"],
      test = "peto-peto", alternative = "greater"))$p.value
  #[1] 0.03127296

  #==========

  # The results shown in Table 9 of Millard and Deverel (1988, p.2097) are correct
  # only for the hypergeometric variance and the modified MWW tests; the other
  # results were computed as if there were no ties.  Re-compute the correct
  # z-statistics and p-values for the copper and zinc data.

  test <- c(rep(c("gehan", "logrank", "peto-peto"), 2), "peto-peto",
    "normal.scores.1", "normal.scores.2", "normal.scores.2")

  variance <- c(rep("permutation", 3), rep("hypergeometric", 3),
    "asymptotic", rep("permutation", 2), "hypergeometric")

  stats.mat <- matrix(as.numeric(NA), ncol = 4, nrow = 10)

  for(i in 1:10) {
    dum.list <- with(Millard.Deverel.88.df,
        twoSampleLinearRankTestCensored(
          x = Cu[Zone == "Basin.Trough"],
          x.censored = Cu.censored[Zone == "Basin.Trough"],
          y = Cu[Zone == "Alluvial.Fan"],
          y.censored = Cu.censored[Zone == "Alluvial.Fan"],
          test = test[i], variance = variance[i]))
    stats.mat[i, 1:2] <- c(dum.list$statistic["z"], dum.list$p.value)

    dum.list <- with(Millard.Deverel.88.df,
        twoSampleLinearRankTestCensored(
          x = Zn[Zone == "Basin.Trough"],
          x.censored = Zn.censored[Zone == "Basin.Trough"],
          y = Zn[Zone == "Alluvial.Fan"],
          y.censored = Zn.censored[Zone == "Alluvial.Fan"],
          test = test[i], variance = variance[i]))
    stats.mat[i, 3:4] <- c(dum.list$statistic["z"], dum.list$p.value)
  }

  dimnames(stats.mat) <- list(paste(test, variance, sep = "."),
    c("Cu.Z", "Cu.p.value", "Zn.Z", "Zn.p.value"))

  round(stats.mat, 2)
  #                               Cu.Z Cu.p.value Zn.Z Zn.p.value
  #gehan.permutation              0.87       0.38 2.49       0.01
  #logrank.permutation            0.79       0.43 1.75       0.08
  #peto-peto.permutation          0.92       0.36 2.42       0.02
  #gehan.hypergeometric           0.71       0.48 2.35       0.02
  #logrank.hypergeometric         0.51       0.61 1.69       0.09
  #peto-peto.hypergeometric       1.03       0.30 2.59       0.01
  #peto-peto.asymptotic           0.90       0.37 2.37       0.02
  #normal.scores.1.permutation    0.94       0.34 2.37       0.02
  #normal.scores.2.permutation    0.98       0.33 2.39       0.02
  #normal.scores.2.hypergeometric 1.28       0.20 2.48       0.01

  #----------

  # Clean up
  #---------
  rm(test, variance, stats.mat, i, dum.list)
}
\keyword{htest}
\keyword{nonparametric}
\keyword{regression}

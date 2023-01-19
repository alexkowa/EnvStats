\name{distChoose}
\alias{distChoose}
\alias{distChoose.default}
\alias{distChoose.formula}
\title{
  Choose Best Fitting Distribution Based on Goodness-of-Fit Tests
}
\description{
  Perform a series of goodness-of-fit tests from a (possibly user-specified)
  set of candidate probability distributions to determine which
  probability distribution provides the best fit for a data set.
}
\usage{
distChoose(y, ...)

\method{distChoose}{formula}(y, data = NULL, subset,
  na.action = na.pass, ...)

\method{distChoose}{default}(y, alpha = 0.05, method = "sw",
    choices = c("norm", "gamma", "lnorm"), est.arg.list = NULL,
    warn = TRUE, keep.data = TRUE, data.name = NULL,
    parent.of.data = NULL, subset.expression = NULL, ...)
}
\arguments{
  \item{y}{
  an object containing data for the goodness-of-fit tests.  In the default
  method, the argument \code{y} must be numeric vector of observations.
  In the formula method, \code{y} must be a formula of the form \code{y ~ 1}.
  Missing (\code{NA}), undefined (\code{NaN}), and
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be
  removed.
}
  \item{data}{
  specifies an optional data frame, list or environment (or object coercible
  by \code{as.data.frame} to a data frame) containing the variables in the
  model.  If not found in \code{data}, the variables are taken from
  \code{environment(formula)}, typically the environment from which
  \code{distChoose} is called.
}
  \item{subset}{
  specifies an optional vector specifying a subset of observations to be used.
}
  \item{na.action}{
  specifies a function which indicates what should happen when the data contain \code{NA}s.
  The default is \code{\link{na.pass}}.
}
  \item{alpha}{
  numeric scalar between 0 and 1 specifying the Type I error associated with each
  goodness-of-fit test.  When \code{method="proucl"} the only allowed values for
  \code{alpha} are 0.01, 0.05, and 0.1.  The default value is \code{alpha=0.05}.
  }
  \item{method}{
  character string defining which method to use.  Possible values are:
  \itemize{
  \item \code{"sw"}. Shapiro-Wilk; the default.
  \item \code{"sf"}. Shapiro-Francia.
  \item \code{"ppcc"}. Probability Plot Correlation Coefficient.
  \item \code{"proucl"}.  ProUCL.
  }

  See the DETAILS section below.
}
  \item{choices}{
  a character vector denoting the distribution abbreviations of the candidate
  distributions.  See the help file for \code{\link{Distribution.df}} for a list
  of distributions and their abbreviations.
  The default value is \code{choices=c("norm", "gamma", "lnorm")},
  indicating the \link{Normal}, \link{Gamma},  and \link{Lognormal} distributions.

  This argument is ignored when \code{method="proucl"}.
}
  \item{est.arg.list}{
  a list containing one or more lists of arguments to be passed to the
  function(s) estimating the distribution parameters.  The name(s) of
  the components of the list must be equal to or a subset of the values of the
  argument \code{choices}.  For example, if
  \code{choices=c("norm", "gamma", "lnorm")}, setting \cr
  \code{est.arg.list=list(gamma = list(method="bcmle"))} indicates
  using the bias-corrected maximum-likelihood estimators of shape and scale
  for the  gamma distribution (see the help file for \code{\link{egamma}}).
  See the help file \link{Estimating Distribution Parameters} for a list of
  estimating functions.
  The default value is \code{est.arg.list=NULL} so that all default values for the
  estimating functions are used.

  When testing for some form of normality (i.e., \link{Normal}, \link{Lognormal},
  \link[=Lognormal3]{Three-Parameter Lognormal},
  \link[=ZeroModifiedNormal]{Zero-Modified Normal}, or
  \link[=ZeroModifiedLognormal]{Zero-Modified Lognormal (Delta)}),
  the estimated parameters are provided in the output merely for
  information, and the choice of the method of estimation has no effect
  on the goodness-of-fit test statistics or p-values.

  This argument is ignored when \code{method="proucl"}.
}
  \item{warn}{
  logical scalar indicating whether to print a warning message when
  observations with \code{NA}s, \code{NaN}s, or \code{Inf}s in
  \code{y} are removed.  The default value is \code{TRUE}.
}
  \item{keep.data}{
  logical scalar indicating whether to return the original data.  The
  default value is \code{keep.data=TRUE}.
}
  \item{data.name}{
  optional character string indicating the name of the data used for argument \code{y}.
}
  \item{parent.of.data}{
  character string indicating the source of the data used for the
  goodness-of-fit test.
}
  \item{subset.expression}{
  character string indicating the expression used to subset the data.
}
  \item{\dots}{
  additional arguments affecting the goodness-of-fit test.
}
}
\details{
  The function \code{distChoose} returns a list with information on the goodness-of-fit
  tests for various distributions and which distribution appears to best fit the
  data based on the p-values from the goodness-of-fit tests.  This function was written in
  order to compare ProUCL's way of choosing the best-fitting distribution (USEPA, 2015) with
  other ways of choosing the best-fitting distribution.

  \bold{Method Based on Shapiro-Wilk, Shapiro-Francia, or Probability Plot Correlation Test} \cr
  (\code{method="sw"}, \code{method="sf"}, or \code{method="ppcc"})

  For each value of the argument \code{choices}, the function \code{distChoose}
  runs the goodness-of-fit test using the data in \code{y} assuming that particular
  distribution.  For example, if \cr
  \code{choices=c("norm", "gamma", "lnorm")},
  indicating the \link{Normal}, \link{Gamma}, and \link{Lognormal} distributions, and
  \code{method="sw"}, then the usual Shapiro-Wilk test is performed for the \link{Normal}
  and \link{Lognormal} distributions, and the extension of the Shapiro-Wilk test is performed
  for the \link{Gamma} distribution (see the section
  \emph{Testing Goodness-of-Fit for Any Continuous Distribution} in the help
  file for \code{\link{gofTest}} for an explanation of the latter).  The distribution associated
  with the largest p-value is the chosen distribution.  In the case when all p-values are
  less than the value of the argument \code{alpha}, the distribution \dQuote{Nonparametric} is chosen.  \cr

  \bold{Method Based on ProUCL Algorithm} (\code{method="proucl"})

  When \code{method="proucl"}, the function \code{distChoose} uses the
  algorithm that ProUCL (USEPA, 2015) uses to determine the best fitting
  distribution.  The candidate distributions are the
  \link{Normal}, \link{Gamma}, and \link{Lognormal} distributions.  The algorithm
  used by ProUCL is as follows:

  \enumerate{
  \item Perform the Shapiro-Wilk and Lilliefors goodness-of-fit tests for the
        \link{Normal} distribution, i.e., call the function \code{\link{gofTest}} with
        \code{distribution = "norm", test="sw"} and \cr
        \code{distribution = "norm", test="lillie"}.
        If either or both of the associated p-values are greater than or equal to the user-supplied value
        of \code{alpha}, then choose the \link{Normal} distribution.  Otherwise, proceed to the next step.

  \item Perform the \dQuote{ProUCL Anderson-Darling} and \dQuote{ProUCL Kolmogorov-Smirnov} goodness-of-fit
        tests for the \link{Gamma} distribution,
        i.e., call the function \code{\link{gofTest}} with \cr
        \code{distribution="gamma", test="proucl.ad.gamma"} and \cr
        \code{distribution="gamma", test="proucl.ks.gamma"}.
        If either or both of the associated p-values are greater than or equal to the user-supplied value
        of \code{alpha}, then choose the \link{Gamma} distribution.  Otherwise, proceed to the next step.

  \item Perform the Shapiro-Wilk and Lilliefors goodness-of-fit tests for the
        \link{Lognormal} distribution, i.e., call the function \code{\link{gofTest}} with
        \code{distribution="lnorm", test="sw"} and \cr
        \code{distribution="lnorm", test="lillie"}.
        If either or both of the associated p-values are greater than or equal to the user-supplied value
        of \code{alpha}, then choose the \link{Lognormal} distribution.  Otherwise, proceed to the next step.

  \item If none of the goodness-of-fit tests above yields a p-value greater than or equal to the user-supplied value
        of \code{alpha}, then choose the \dQuote{Nonparametric} distribution.
  }
}
\value{
  a list of class \code{"distChoose"} containing the results of the goodness-of-fit tests.
  Objects of class \code{"distChoose"} have a special printing method.
  See the help files for \code{\link{distChoose.object}} for details.
}
\references{
  Birnbaum, Z.W., and F.H. Tingey. (1951).
  One-Sided Confidence Contours for Probability Distribution Functions.
  \emph{Annals of Mathematical Statistics} \bold{22}, 592-596.

  Blom, G. (1958). \emph{Statistical Estimates and Transformed Beta Variables}.
  John Wiley and Sons, New York.

  Conover, W.J. (1980). \emph{Practical Nonparametric Statistics}. Second Edition.
  John Wiley and Sons, New York.

  Dallal, G.E., and L. Wilkinson. (1986).
  An Analytic Approximation to the Distribution of Lilliefor's Test for Normality.
  \emph{The American Statistician} \bold{40}, 294-296.

  D'Agostino, R.B. (1970). Transformation to Normality of the Null Distribution of \eqn{g1}.
  \emph{Biometrika} \bold{57}, 679-681.

  D'Agostino, R.B. (1971). An Omnibus Test of Normality for Moderate and Large Size Samples.
  \emph{Biometrika} \bold{58}, 341-348.

  D'Agostino, R.B. (1986b). Tests for the Normal Distribution. In: D'Agostino, R.B., and M.A. Stephens, eds.
  \emph{Goodness-of Fit Techniques}. Marcel Dekker, New York.

  D'Agostino, R.B., and E.S. Pearson (1973). Tests for Departures from Normality.
  Empirical Results for the Distributions of \eqn{b2} and \eqn{\sqrt{b1}}.
  \emph{Biometrika} \bold{60}(3), 613-622.

  D'Agostino, R.B., and G.L. Tietjen (1973). Approaches to the Null Distribution of \eqn{\sqrt{b1}}.
  \emph{Biometrika} \bold{60}(1), 169-173.

  Fisher, R.A. (1950). \emph{Statistical Methods for Research Workers}. 11'th Edition.
  Hafner Publishing Company, New York, pp.99-100.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009).
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.
  John Wiley & Sons, Hoboken.

  Kendall, M.G., and A. Stuart. (1991).
  \emph{The Advanced Theory of Statistics, Volume 2: Inference and Relationship}.
  Fifth Edition. Oxford University Press, New York.

  Kim, P.J., and R.I. Jennrich. (1973).
  Tables of the Exact Sampling Distribution of the Two Sample Kolmogorov-Smirnov Criterion.
  In Harter, H.L., and D.B. Owen, eds. \emph{Selected Tables in Mathematical Statistics, Vol. 1}.
  American Mathematical Society, Providence, Rhode Island, pp.79-170.

  Kolmogorov, A.N. (1933). Sulla determinazione empirica di una legge di distribuzione.
  \emph{Giornale dell' Istituto Italiano degle Attuari} \bold{4}, 83-91.

  Marsaglia, G., W.W. Tsang, and J. Wang. (2003). Evaluating Kolmogorov's distribution.
  \emph{Journal of Statistical Software}, \bold{8}(18).
  \doi{10.18637/jss.v008.i18}.

  Moore, D.S. (1986). Tests of Chi-Squared Type. In D'Agostino, R.B., and M.A. Stephens, eds.
  \emph{Goodness-of Fit Techniques}. Marcel Dekker, New York, pp.63-95.

  Pomeranz, J. (1973).
  Exact Cumulative Distribution of the Kolmogorov-Smirnov Statistic for Small Samples (Algorithm 487).
  \emph{Collected Algorithms from ACM} ??, ???-???.

  Royston, J.P. (1992a). Approximating the Shapiro-Wilk W-Test for Non-Normality.
  \emph{Statistics and Computing} \bold{2}, 117-119.

  Royston, J.P. (1992b).
  Estimation, Reference Ranges and Goodness of Fit for the Three-Parameter Log-Normal Distribution.
  \emph{Statistics in Medicine} \bold{11}, 897-912.

  Royston, J.P. (1992c).
  A Pocket-Calculator Algorithm for the Shapiro-Francia Test of Non-Normality: An Application to Medicine.
  \emph{Statistics in Medicine} \bold{12}, 181-184.

  Royston, P. (1993). A Toolkit for Testing for Non-Normality in Complete and Censored Samples.
  \emph{The Statistician} \bold{42}, 37-43.

  Ryan, T., and B. Joiner. (1973). \emph{Normal Probability Plots and Tests for Normality}.
  Technical Report, Pennsylvannia State University, Department of Statistics.

  Shapiro, S.S., and R.S. Francia. (1972). An Approximate Analysis of Variance Test for Normality.
  \emph{Journal of the American Statistical Association} \bold{67}(337), 215-219.

  Shapiro, S.S., and M.B. Wilk. (1965). An Analysis of Variance Test for Normality (Complete Samples).
  \emph{Biometrika} \bold{52}, 591-611.

  Smirnov, N.V. (1939).
  Estimate of Deviation Between Empirical Distribution Functions in Two Independent Samples.
  \emph{Bulletin Moscow University} \bold{2}(2), 3-16.

  Smirnov, N.V. (1948). Table for Estimating the Goodness of Fit of Empirical Distributions.
  \emph{Annals of Mathematical Statistics} \bold{19}, 279-281.

  Stephens, M.A. (1970).
  Use of the Kolmogorov-Smirnov, Cramer-von Mises and Related Statistics Without Extensive Tables.
  \emph{Journal of the Royal Statistical Society, Series B}, \bold{32}, 115-122.

  Stephens, M.A. (1986a). Tests Based on EDF Statistics. In D'Agostino, R. B., and M.A. Stevens, eds.
  \emph{Goodness-of-Fit Techniques}. Marcel Dekker, New York.

  USEPA. (2015).  \emph{ProUCL Version 5.1.002 Technical Guide}.  EPA/600/R-07/041, October 2015.
  Office of Research and Development. U.S. Environmental Protection Agency, Washington, D.C.

  Verrill, S., and R.A. Johnson. (1987).
  The Asymptotic Equivalence of Some Modified Shapiro-Wilk Statistics -- Complete and Censored Sample Cases.
  \emph{The Annals of Statistics} \bold{15}(1), 413-419.

  Verrill, S., and R.A. Johnson. (1988).
  Tables and Large-Sample Distribution Theory for Censored-Data Correlation Statistics for Testing Normality.
  \emph{Journal of the American Statistical Association} \bold{83}, 1192-1197.

  Weisberg, S., and C. Bingham. (1975).
  An Approximate Analysis of Variance Test for Non-Normality Suitable for Machine Calculation.
  \emph{Technometrics} \bold{17}, 133-134.

  Wilk, M.B., and S.S. Shapiro. (1968). The Joint Assessment of Normality of Several Independent
  Samples. \emph{Technometrics}, \bold{10}(4), 825-839.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition.
  Prentice-Hall, Upper Saddle River, NJ.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  In practice, almost any goodness-of-fit test will \emph{not} reject the null hypothesis
  if the number of observations is relatively small.  Conversely, almost any goodness-of-fit
  test \emph{will} reject the null hypothesis if the number of observations is very large,
  since \dQuote{real} data are never distributed according to any theoretical distribution
  (Conover, 1980, p.367).  For most cases, however, the distribution of \dQuote{real} data
  is close enough to some theoretical distribution that fairly accurate results may be
  provided by assuming that particular theoretical distribution.  One way to asses the
  goodness of the fit is to use goodness-of-fit tests.  Another way is to look at
  quantile-quantile (Q-Q) plots (see \code{\link{qqPlot}}).
}
\seealso{
  \code{\link{gofTest}}, \code{\link{distChoose.object}}, \code{\link{print.distChoose}}.
}
\examples{
  # Generate 20 observations from a gamma distribution with
  # parameters shape = 2 and scale = 3 and:
  #
  # 1) Call distChoose using the Shapiro-Wilk method.
  #
  # 2) Call distChoose using the Shapiro-Wilk method and specify
  #    the bias-corrected method of estimating shape for the Gamma
  #    distribution.
  #
  # 3) Compare the results in 2) above with the results using the
  #    ProUCL method.
  #
  # Notes:  The call to set.seed lets you reproduce this example.
  #
  #         The ProUCL method chooses the Normal distribution, whereas the
  #         Shapiro-Wilk method chooses the Gamma distribution.

  set.seed(47)
  dat <- rgamma(20, shape = 2, scale = 3)


  # 1) Call distChoose using the Shapiro-Wilk method.
  #--------------------------------------------------

  distChoose(dat)

  #Results of Choosing Distribution
  #--------------------------------
  #
  #Candidate Distributions:         Normal
  #                                 Gamma
  #                                 Lognormal
  #
  #Choice Method:                   Shapiro-Wilk
  #
  #Type I Error per Test:           0.05
  #
  #Decision:                        Gamma
  #
  #Estimated Parameter(s):          shape = 1.909462
  #                                 scale = 4.056819
  #
  #Estimation Method:               MLE
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Test Results:
  #
  #  Normal
  #    Test Statistic:              W = 0.9097488
  #    P-value:                     0.06303695
  #
  #  Gamma
  #    Test Statistic:              W = 0.9834958
  #    P-value:                     0.970903
  #
  #  Lognormal
  #    Test Statistic:              W = 0.9185006
  #    P-value:                     0.09271768

  #--------------------

  # 2) Call distChoose using the Shapiro-Wilk method and specify
  #    the bias-corrected method of estimating shape for the Gamma
  #    distribution.
  #---------------------------------------------------------------

  distChoose(dat, method = "sw",
    est.arg.list = list(gamma = list(method = "bcmle")))

  #Results of Choosing Distribution
  #--------------------------------
  #
  #Candidate Distributions:         Normal
  #                                 Gamma
  #                                 Lognormal
  #
  #Choice Method:                   Shapiro-Wilk
  #
  #Type I Error per Test:           0.05
  #
  #Decision:                        Gamma
  #
  #Estimated Parameter(s):          shape = 1.656376
  #                                 scale = 4.676680
  #
  #Estimation Method:               Bias-Corrected MLE
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Test Results:
  #
  #  Normal
  #    Test Statistic:              W = 0.9097488
  #    P-value:                     0.06303695
  #
  #  Gamma
  #    Test Statistic:              W = 0.9834346
  #    P-value:                     0.9704046
  #
  #  Lognormal
  #    Test Statistic:              W = 0.9185006
  #    P-value:                     0.09271768

  #--------------------

  # 3) Compare the results in 2) above with the results using the
  #    ProUCL method.
  #---------------------------------------------------------------

  distChoose(dat, method = "proucl")

  #Results of Choosing Distribution
  #--------------------------------
  #
  #Candidate Distributions:         Normal
  #                                 Gamma
  #                                 Lognormal
  #
  #Choice Method:                   ProUCL
  #
  #Type I Error per Test:           0.05
  #
  #Decision:                        Normal
  #
  #Estimated Parameter(s):          mean = 7.746340
  #                                 sd   = 5.432175
  #
  #Estimation Method:               mvue
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Test Results:
  #
  #  Normal
  #    Shapiro-Wilk GOF
  #      Test Statistic:            W = 0.9097488
  #      P-value:                   0.06303695
  #    Lilliefors (Kolmogorov-Smirnov) GOF
  #      Test Statistic:            D = 0.1547851
  #      P-value:                   0.238092
  #
  #  Gamma
  #    ProUCL Anderson-Darling Gamma GOF
  #      Test Statistic:            A = 0.1853826
  #      P-value:                   >= 0.10
  #    ProUCL Kolmogorov-Smirnov Gamma GOF
  #      Test Statistic:            D = 0.0988692
  #      P-value:                   >= 0.10
  #
  #  Lognormal
  #    Shapiro-Wilk GOF
  #      Test Statistic:            W = 0.9185006
  #      P-value:                   0.09271768
  #    Lilliefors (Kolmogorov-Smirnov) GOF
  #      Test Statistic:            D = 0.149317
  #      P-value:                   0.2869177

  #--------------------

  # Clean up
  #---------

  rm(dat)

  #====================================================================

  # Example 10-2 of USEPA (2009, page 10-14) gives an example of
  # using the Shapiro-Wilk test to test the assumption of normality
  # for nickel concentrations (ppb) in groundwater collected over
  # 4 years.  The data for this example are stored in
  # EPA.09.Ex.10.1.nickel.df.

  EPA.09.Ex.10.1.nickel.df
  #   Month   Well Nickel.ppb
  #1      1 Well.1       58.8
  #2      3 Well.1        1.0
  #3      6 Well.1      262.0
  #4      8 Well.1       56.0
  #5     10 Well.1        8.7
  #6      1 Well.2       19.0
  #7      3 Well.2       81.5
  #8      6 Well.2      331.0
  #9      8 Well.2       14.0
  #10    10 Well.2       64.4
  #11     1 Well.3       39.0
  #12     3 Well.3      151.0
  #13     6 Well.3       27.0
  #14     8 Well.3       21.4
  #15    10 Well.3      578.0
  #16     1 Well.4        3.1
  #17     3 Well.4      942.0
  #18     6 Well.4       85.6
  #19     8 Well.4       10.0
  #20    10 Well.4      637.0

  # Use distChoose with the probability plot correlation method,
  # and for the lognormal distribution specify the
  # mean and CV parameterization:
  #------------------------------------------------------------

  distChoose(Nickel.ppb ~ 1, data = EPA.09.Ex.10.1.nickel.df,
    choices = c("norm", "gamma", "lnormAlt"), method = "ppcc")

  #Results of Choosing Distribution
  #--------------------------------
  #
  #Candidate Distributions:         Normal
  #                                 Gamma
  #                                 Lognormal
  #
  #Choice Method:                   PPCC
  #
  #Type I Error per Test:           0.05
  #
  #Decision:                        Lognormal
  #
  #Estimated Parameter(s):          mean = 213.415628
  #                                 cv   =   2.809377
  #
  #Estimation Method:               mvue
  #
  #Data:                            Nickel.ppb
  #
  #Data Source:                     EPA.09.Ex.10.1.nickel.df
  #
  #Sample Size:                     20
  #
  #Test Results:
  #
  #  Normal
  #    Test Statistic:              r = 0.8199825
  #    P-value:                     5.753418e-05
  #
  #  Gamma
  #    Test Statistic:              r = 0.9749044
  #    P-value:                     0.317334
  #
  #  Lognormal
  #    Test Statistic:              r = 0.9912528
  #    P-value:                     0.9187852

  #--------------------

  # Repeat the above example using the ProUCL method.
  #--------------------------------------------------

  distChoose(Nickel.ppb ~ 1, data = EPA.09.Ex.10.1.nickel.df,
    method = "proucl")

  #Results of Choosing Distribution
  #--------------------------------
  #
  #Candidate Distributions:         Normal
  #                                 Gamma
  #                                 Lognormal
  #
  #Choice Method:                   ProUCL
  #
  #Type I Error per Test:           0.05
  #
  #Decision:                        Gamma
  #
  #Estimated Parameter(s):          shape =   0.5198727
  #                                 scale = 326.0894272
  #
  #Estimation Method:               MLE
  #
  #Data:                            Nickel.ppb
  #
  #Data Source:                     EPA.09.Ex.10.1.nickel.df
  #
  #Sample Size:                     20
  #
  #Test Results:
  #
  #  Normal
  #    Shapiro-Wilk GOF
  #      Test Statistic:            W = 0.6788888
  #      P-value:                   2.17927e-05
  #    Lilliefors (Kolmogorov-Smirnov) GOF
  #      Test Statistic:            D = 0.3267052
  #      P-value:                   5.032807e-06
  #
  #  Gamma
  #    ProUCL Anderson-Darling Gamma GOF
  #      Test Statistic:            A = 0.5076725
  #      P-value:                   >= 0.10
  #    ProUCL Kolmogorov-Smirnov Gamma GOF
  #      Test Statistic:            D = 0.1842904
  #      P-value:                   >= 0.10
  #
  #  Lognormal
  #    Shapiro-Wilk GOF
  #      Test Statistic:            W = 0.978946
  #      P-value:                   0.9197735
  #    Lilliefors (Kolmogorov-Smirnov) GOF
  #      Test Statistic:            D = 0.08405167
  #      P-value:                   0.9699648

  #====================================================================

  \dontrun{
  # 1) Simulate 1000 trials where for each trial you:
  #    a) Generate 20 observations from a Gamma distribution with
  #       parameters mean = 10 and CV = 1.
  #    b) Use distChoose with the Shapiro-Wilk method.
  #    c) Use distChoose with the ProUCL method.
  #
  #  2) Compare the proportion of times the
  #     Normal vs. Gamma vs. Lognormal vs. Nonparametric distribution
  #     is chosen for b) and c) above.
  #------------------------------------------------------------------

  set.seed(58)
  N <- 1000

  Choose.fac <- factor(rep("", N), levels = c("Normal", "Gamma", "Lognormal", "Nonparametric"))
  Choose.df <- data.frame(SW = Choose.fac, ProUCL = Choose.fac)

  for(i in 1:N) {
    dat <- rgammaAlt(20, mean = 10, cv = 1)
    Choose.df[i, "SW"]     <- distChoose(dat, method = "sw")$decision
    Choose.df[i, "ProUCL"] <- distChoose(dat, method = "proucl")$decision
  }

  summaryStats(Choose.df, digits = 0)

  #              ProUCL(N) ProUCL(Pct) SW(N) SW(Pct)
  #Normal              443          44    41       4
  #Gamma               546          55   733      73
  #Lognormal             9           1   215      22
  #Nonparametric         2           0    11       1
  #Combined           1000         100  1000     100


  #--------------------


  # Repeat above example for the Lognormal Distribution with mean=10 and CV = 1.
  #-----------------------------------------------------------------------------

  set.seed(297)
  N <- 1000

  Choose.fac <- factor(rep("", N), levels = c("Normal", "Gamma", "Lognormal", "Nonparametric"))
  Choose.df <- data.frame(SW = Choose.fac, ProUCL = Choose.fac)

  for(i in 1:N) {
    dat <- rlnormAlt(20, mean = 10, cv = 1)
    Choose.df[i, "SW"]     <- distChoose(dat, method = "sw")$decision
    Choose.df[i, "ProUCL"] <- distChoose(dat, method = "proucl")$decision
  }

  summaryStats(Choose.df, digits = 0)

  #              ProUCL(N) ProUCL(Pct) SW(N) SW(Pct)
  #Normal              313          31    15       2
  #Gamma               556          56   254      25
  #Lognormal           121          12   706      71
  #Nonparametric        10           1    25       2
  #Combined           1000         100  1000     100


  #--------------------


  # Clean up
  #---------

  rm(N, Choose.fac, Choose.df, i, dat)
  }

}
\keyword{ htest }
\keyword{ models }

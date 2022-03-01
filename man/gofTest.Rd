\name{gofTest}
\alias{gofTest}
\alias{gofTest.default}
\alias{gofTest.formula}
\title{
  Goodness-of-Fit Test
}
\description{
  Perform a goodness-of-fit test to determine whether a data set
  appears to come from a specified probability distribution or if two
  data sets appear to come from the same distribution.
}
\usage{
gofTest(y, ...)

\method{gofTest}{formula}(y, data = NULL, subset,
  na.action = na.pass, ...)

\method{gofTest}{default}(y, x = NULL,
  test = ifelse(is.null(x), "sw", "ks"),
  distribution = "norm", est.arg.list = NULL,
  alternative = "two.sided", n.classes = NULL,
  cut.points = NULL, param.list = NULL,
  estimate.params = ifelse(is.null(param.list), TRUE, FALSE),
  n.param.est = NULL, correct = NULL, digits = .Options$digits,
  exact = NULL, ws.method = "normal scores", warn = TRUE, keep.data = TRUE,
  data.name = NULL, data.name.x = NULL, parent.of.data = NULL,
  subset.expression = NULL, ...)
}
\arguments{
  \item{y}{
  an object containing data for the goodness-of-fit test.  In the default
  method, the argument \code{y} must be numeric vector of observations.
  In the formula method, \code{y} must be a formula of the form \code{y ~ 1}
  or \code{y ~ x}.  The form \code{y ~ 1} indicates use the observations in
  the vector \code{y} for a one-sample goodness-of-fit test.  The form
  \code{y ~ x} is only relevant to the case of the two-sample
  Kolmogorov-Smirnov test (\code{test="ks"}) and indicates use the
  observations in the vector \code{y} as the second sample and use the
  observations in the vector \code{x} as the first sample.  Note that
  for the formula method, \code{x} and \code{y} must be the same length but
  this is not a requirement of the test and you can use vectors of different
  lengths via the default method.
  Missing (\code{NA}), undefined (\code{NaN}),
  and infinite (\code{Inf}, \code{-Inf}) values are allowed but will be
  removed.
}
  \item{data}{
  specifies an optional data frame, list or environment (or object coercible
  by \code{as.data.frame} to a data frame) containing the variables in the
  model.  If not found in \code{data}, the variables are taken from
  \code{environment(formula)}, typically the environment from which
  \code{gofTest} is called.
}
  \item{subset}{
  specifies an optional vector specifying a subset of observations to be used.
}
  \item{na.action}{
  specifies a function which indicates what should happen when the data contain \code{NA}s.
  The default is \code{\link{na.pass}}.
}
  \item{x}{
  numeric vector of values for the first sample in the case of a two-sample
  Kolmogorov-Smirnov goodness-of-fit test (\code{test="ks"}).
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf},
  \code{-Inf}) values are allowed but will be removed.
}
  \item{test}{
  character string defining which goodness-of-fit test to perform.  Possible values are:
  \itemize{
  \item \code{"sw"}. Shapiro-Wilk; the default when \code{x} is NOT supplied.
  \item \code{"sf"}. Shapiro-Francia.
  \item \code{"ppcc"}. Probability Plot Correlation Coefficient.
  \item \code{"ad"}.  Anderson-Darling.
  \item \code{"cmv"}. Cramer-von Mises.
  \item \code{"lillie"}. Lilliefor.
  \item \code{"skew"}. Zero-skew.
  \item \code{"chisq"}. Chi-squared.
  \item \code{"ks"}. Kolmogorov-Smirnov; the default when \code{x} IS supplied.
  \item \code{"ws"}. Wilk-Shapiro test for Uniform [0, 1] distribution.
  \item \code{"proucl.ad.gamma"}. Anderson-Darling test for a gamma distribution using
    ProUCL critical values.
  \item \code{"proucl.ks.gamma"}. Kolmogorov-Smirnov test for a gamma distribution using
    ProUCL critical values.
  }

  When the argument \code{x} is supplied, you must set \code{test="ks"}, which is what \code{gofTest}
  does by default.
}
  \item{distribution}{
  a character string denoting the distribution abbreviation.  See the help file for
  \code{\link{Distribution.df}} for a list of distributions and their abbreviations.
  The default value is \code{distribution="norm"} (\link{Normal} distribution).

  When \code{test="sw"}, \code{test="sf"}, or \code{test="ppcc"}, any continuous
  distribuiton is allowed (e.g., \code{"norm"} (normal), \code{"lnorm"} (lognormal),
  \code{"gamma"} (gamma), etc.), as well as mixed distributions involving the normal distribution
  (i.e., \code{"zmnorm"} (zero-modified normal), \code{"zmlnorm"} (zero-modified lognormal (delta)),
  and \cr
  \code{"zmlnormAlt"} (zero-modified lognormal with alternative parameterization)).

  When \code{test="ad"}, \code{test="cvm"}, \code{test="lillie"}, or \code{test="skew"},
  only the values \code{"norm"} (normal), \code{"lnorm"} (lognormal),
  \code{"lnormAlt"} (lognormal with alternative parameterization),
  \code{"zmnorm"} (zero-modified normal), \code{"zmlnorm"} (zero-modified lognormal (delta)), and \cr
  \code{"zmlnormAlt"} (zero-modified lognormal with alternative parameterization) are allowed.

  When \code{test="ks"}, any continuous distribution is allowed.

  When \code{test="chisq"}, any distribuiton is allowed.

  When \code{test="ws"}, this argument is ignored.

  When \code{test="proucl.ad.gamma"} or \code{test="proucl.ks.gamma"}, you must set
  \code{distribution="gamma"} or \code{distribution="gammaAlt"}.
}
  \item{est.arg.list}{
  a list of arguments to be passed to the function estimating the distribution parameters.
  For example, if \code{test="sw"} and \code{distribution="gamma"}, setting \cr
  \code{est.arg.list=list(method="bcmle")} indicates using the bias-corrected \cr
  maximum-likelihood
  estimators of shape and scale (see the help file for \code{\link{egamma}}).
  See the help file
  \link{Estimating Distribution Parameters} for a list of estimating functions.
  The default value is \code{est.arg.list=NULL} so that all default values for the
  estimating function are used.  This argument is ignored if \cr
  \code{estimate.params=FALSE}.


  When \code{test="sw"}, \code{test="sf"}, \code{test="ppcc"},
  \code{test="ad"}, \code{test="cvm"}, \code{test="lillie"}, or \code{test="skew"},
  and you are testing for some form of normality (i.e., \link{Normal}, \link{Lognormal},
  \link[=Lognormal3]{Three-Parameter Lognormal},
  \link[=ZeroModifiedNormal]{Zero-Modified Normal}, or
  \link[=ZeroModifiedLognormal]{Zero-Modified Lognormal (Delta)}),
  the estimated parameters are provided in the
  output merely for information, and the choice of the method of estimation has no effect
  on the goodness-of-fit test statistic or p-value.


  When \code{test="ks"}, \code{x} is not supplied, and
  \code{estimate.params=TRUE}, the estimated parameters are used to
  specify the null hypothesis of which distribution
  the data are assumed to come from.


  When \code{test="chisq"} and \code{estimate.params=TRUE},
  the estimated parameters are used to specify the null hypothesis of which distribution
  the data are assumed to come from.

  When \code{test="ws"}, \code{test="proucl.ad.gamma"}, or \code{test="proucl.ks.gamma"},
  this argument is ignored.
}
  \item{alternative}{
  for the case when \code{test="ks"}, \code{test="skew"}, or \code{test="ws"},
  character string specifying the alternative hypothesis.  When \code{test="ks"} or
  \code{test="skew"}, the possible values are \code{"two-sided"} (the default),
  \code{"greater"}, or \code{"less"}.  When \code{test="ws"}, the possible values are
  \code{"greater"} (the default), or \code{"less"}.  See the DETAILS section
  of the help file for \code{\link{ks.test}} for more explanation of the
  meaning of this argument.
}
  \item{n.classes}{
  for the case when \code{test="chisq"}, the number of cells into which the observations
  are to be allocated.  If the argument \code{cut.points} is supplied, then \code{n.classes}
  is set to \code{length(cut.points)-1}.  The default value is \cr
  \code{ceiling(2* (length(x)^(2/5)))} and is recommended by Moore (1986).
}
  \item{cut.points}{
  for the case when \code{test="chisq"}, a vector of cutpoints that defines the cells.
  The element \code{x[i]} is allocated to cell \code{j} if \cr
  \code{cut.points[j]} < \code{x[i]} \eqn{\le} \code{cut.points[j+1]}.
  If \code{x[i]} is less than or equal to the first cutpoint or
  greater than the last cutpoint, then \code{x[i]} is treated as missing.  If the
  hypothesized distribution is discrete, \code{cut.points} must be supplied.  The default
  value is \code{cut.points=NULL}, in which case the cutpoints are determined by
  \code{n.classes} equi-probable intervals.
}
  \item{param.list}{
  for the case when \code{test="ks"} and \code{x} is not supplied, or when
  \code{test="chisq"},
  a list with values for the parameters of the specified distribution.  See the help file
  for \code{\link{Distribution.df}} for the names and possible values of the parameters
  associated with each distribution.  The default value is \code{param.list=NULL}, which forces
  estimation of the distribution parameters.  This argument is ignored if
  \code{estimate.params=TRUE}.
}
  \item{estimate.params}{
  for the case when \code{test="ks"} and \code{x} is not supplied, or when
  \code{test="chisq"}, a logical scalar indicating whether to perform the goodness-of-fit test based on
  estimating the distribution parameters (\code{estimate.params=TRUE}) or using the
  user-supplied distribution parameters specified by \code{param.list} \cr
  (\code{estimate.params=FALSE}).  The default value of \code{estimate.params} is
  \code{TRUE} if \code{param.list=NULL}, otherwise it is \code{FALSE}.
}
  \item{n.param.est}{
  for the case when \code{test="ks"} and \code{x} is not supplied, or when
  \code{test="chisq"},
  an integer indicating the number of parameters estimated from the data.  \cr
  If \code{estimate.params=TRUE}, the default value is the number of parameters associated
  with the distribution specified by \code{distribution} (e.g., 2 for a normal distribution).
  If \code{estimate.params=FALSE}, the default value is \code{n.param.est=0}.
}
  \item{correct}{
  for the case when \code{test="chisq"}, a logical scalar indicating whether to use the
  continuity correction.  The default value is \code{correct=FALSE} unless \cr
  \code{n.classes=2}.
}
  \item{digits}{
  for the case when \code{test="ks"} and \code{x} is not supplied, or when
  \code{test="chisq"}, and \code{param.list} is supplied,
  a scalar indicating how many significant digits to print out for the parameters
  associated with the hypothesized distribution.  The default value is
  \code{.Options$digits}.
}
  \item{exact}{
  for the case when \code{test="ks"}, \code{exact=NULL} by default, but can be set to
  a logical scalar indicating whether an exact p-value should be computed.
  See the help file for \code{\link{ks.test}} for more information.
}
  \item{ws.method}{
  for the case when \code{test="ws"}, this argument specifies whether to perform the test
  based on normal scores (\code{ws.method="normal scores"}, the default) or
  chi-square scores (\code{ws.method="chi-square scores"}).  See the DETAILS section
  for more information.
}
  \item{warn}{
  logical scalar indicating whether to print a warning message when
  observations with \code{NA}s, \code{NaN}s, or \code{Inf}s in
  \code{y} or \code{x} are removed.  The default value is \code{warn=TRUE}.
}
  \item{keep.data}{
  logical scalar indicating whether to return the data used for the goodness-of-fit test.
  The default value is \code{keep.data=TRUE}.
}
  \item{data.name}{
  character string indicating the name of the data used for argument \code{y}.
}
  \item{data.name.x}{
  character string indicating the name of the data used for argument \code{x}.
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
  \itemize{

    \item \bold{Shapiro-Wilk Goodness-of-Fit Test} (\code{test="sw"}).

    The Shapiro-Wilk goodness-of-fit test (Shapiro and Wilk, 1965; Royston, 1992a)
    is one of the most commonly used goodness-of-fit tests for normality.
    You can use it to test the following hypothesized distributions:
    \link{Normal}, \link{Lognormal}, \link[=Lognormal3]{Three-Parameter Lognormal},
    \link[=ZeroModifiedNormal]{Zero-Modified Normal}, or
    \link[=ZeroModifiedLognormal]{Zero-Modified Lognormal (Delta)}.
    \bold{In addition, you can also use it to test the null hypothesis of any
    continuous distribution that is available} (see the help file for
    \code{\link{Distribution.df}}, and see explanation below).
    \cr

    \bold{Shapiro-Wilk W-Statistic and P-Value for Testing Normality} \cr
    Let \eqn{X} denote a random variable with cumulative distribution function (cdf)
    \eqn{F}.  Suppose we want to test the null hypothesis that \eqn{F} is the cdf of
    a \link[stats:Normal]{normal (Gaussian) distribution} with some arbitrary mean
    \eqn{\mu} and standard deviation \eqn{\sigma} against the alternative hypothesis
    that \eqn{F} is the cdf of some other distribution.  The table below shows the
    random variable for which \eqn{F} is the assumed cdf, given the value of the
    argument \code{distribution}.
    \tabular{lll}{
    \bold{Value of}     \tab                                         \tab \bold{Random Variable for} \cr
    \code{distribution} \tab \bold{Distribution Name}                \tab \bold{which} \eqn{F} \bold{is the cdf} \cr
    \code{"norm"}       \tab Normal                                  \tab \eqn{X} \cr
    \code{"lnorm"}      \tab Lognormal (Log-space)                   \tab \eqn{log(X)} \cr
    \code{"lnormAlt"}   \tab Lognormal (Untransformed)               \tab \eqn{log(X)} \cr
    \code{"lnorm3"}     \tab Three-Parameter Lognormal               \tab \eqn{log(X-\gamma)} \cr
    \code{"zmnorm"}     \tab Zero-Modified Normal                    \tab \eqn{X | X > 0} \cr
    \code{"zmlnorm"}    \tab Zero-Modified Lognormal (Log-space)     \tab \eqn{log(X) | X > 0} \cr
    \code{"zmlnormAlt"} \tab Zero-Modified Lognormal (Untransformed) \tab \eqn{log(X) | X > 0}
    }
    Note that for the three-parameter lognormal distribution, the symbol \eqn{\gamma}
    denotes the threshold parameter.

    Let \eqn{\underline{x} = (x_1, x_2, \ldots, x_n)} denote the vector of
    \eqn{n} \bold{\emph{ordered}} observations assumed to come from a normal
    distribution.
    \cr

    \emph{The Shapiro-Wilk W-Statistic} \cr
    Shapiro and Wilk (1965) introduced the following statistic to test
    the null hypothesis that \eqn{F} is the cdf of a normal distribution:
    \deqn{W = \frac{(\sum_{i=1}^n a_i x_i)^2}{\sum_{i=1}^n (x_i - \bar{x})^2} \;\;\;\;\;\; (1)}
    where the quantity \eqn{a_i} is the \eqn{i}'th element of the vector
    \eqn{\underline{a}} defined by:
    \deqn{\underline{a} = \frac{\underline{m}^T V^{-1}}{[\underline{m}^T V^{-1} V^{-1} \underline{m}]^{1/2}} \;\;\;\;\;\; (2)}
    where \eqn{T} denotes the transpose operator, and \eqn{\underline{m}} is the vector
    of expected values and \eqn{V} is the variance-covariance matrix of the order
    statistics of a random sample of size \eqn{n} from a standard normal distribution.
    That is, the values of \eqn{\underline{a}} are the expected values of the standard
    normal order statistics weighted by their variance-covariance matrix, and
    normalized so that
    \deqn{\underline{a}^T \underline{a} = 1 \;\;\;\;\;\; (3)}
    It can be shown that the coefficients \eqn{\underline{a}} are antisymmetric, that
    is,
    \deqn{a_i = -a_{n-i+1} \;\;\;\;\;\; (4)}
    and for odd \eqn{n},
    \deqn{a_{(n+1)/2} = 0 \;\;\;\;\;\; (5)}
    Now because
    \deqn{\bar{a} = \frac{1}{n} \sum_{i=1}^n a_i = 0 \;\;\;\;\;\ (6)}
    and
    \deqn{\sum_{i=1}^n (a_i - \bar{a})^2 = \sum_{i=1}^n a_i^2 = \underline{a}^T \underline{a} = 1 \;\;\;\;\;\; (7)}
    the \eqn{W}-statistic in Equation (1) is the same as the square of the sample
    product-moment correlation between the vectors \eqn{\underline{a}} and
    \eqn{\underline{x}}:
    \deqn{W = r(\underline{a}, \underline{x})^2 \;\;\;\;\;\; (8)}
    where
    \deqn{r(\underline{x}, \underline{y}) = \frac{\sum_{i=1}^n (x_i - \bar{x})(y_i - \bar{y})}{[\sum_{i=1}^n (x_i - \bar{x})^2 \sum_{i=1}^n (y_i - \bar{y})^2]^{1/2}} \;\;\;\;\;\;\; (9)}
    (see the \R help file for \code{\link{cor}}).

    The Shapiro-Wilk \eqn{W}-statistic is also simply the ratio of two estimators of
    variance, and can be rewritten as
    \deqn{W = \frac{\hat{\sigma}_{BLUE}^2}{\hat{\sigma}_{MVUE}^2} \;\;\;\;\;\; (10)}
    where the numerator is the square of the best linear unbiased estimate (BLUE) of
    the standard deviation, and the denominator is the minimum variance unbiased
    estimator (MVUE) of the variance:
    \deqn{\hat{\sigma}_{BLUE} = \frac{\sum_{i=1}^n a_i x_i}{\sqrt{n-1}} \;\;\;\;\;\; (11)}
    \deqn{\hat{\sigma}_{MVUE}^2 = \frac{\sum_{i=1}^n (x_i - \bar{x})^2}{n-1} \;\;\;\;\;\; (12)}
    Small values of \eqn{W} indicate the null hypothesis is probably not true.
    Shapiro and Wilk (1965) computed the values of the coefficients \eqn{\underline{a}}
    and the percentage points for \eqn{W} (based on smoothing the empirical null
    distribution of \eqn{W}) for sample sizes up to 50.  Computation of the
    \eqn{W}-statistic for larger sample sizes can be cumbersome, since computation of
    the coefficients \eqn{\underline{a}} requires storage of at least
    \eqn{n + [n(n+1)/2]} reals followed by \eqn{n \times n} matrix inversion
    (Royston, 1992a).
    \cr

    \emph{The Shapiro-Francia W'-Statistic} \cr
    Shapiro and Francia (1972) introduced a modification of the \eqn{W}-test that
    depends only on the expected values of the order statistics (\eqn{\underline{m}})
    and not on the variance-covariance matrix (\eqn{V}):
    \deqn{W' = \frac{(\sum_{i=1}^n b_i x_i)^2}{\sum_{i=1}^n (x_i - \bar{x})^2} \;\;\;\;\;\; (13)}
    where the quantity \eqn{b_i} is the \eqn{i}'th element of the vector
    \eqn{\underline{b}} defined by:
    \deqn{\underline{b} = \frac{\underline{m}}{[\underline{m}^T \underline{m}]^{1/2}} \;\;\;\;\;\; (14)}
    Several authors, including Ryan and Joiner (1973), Filliben (1975), and Weisberg
    and Bingham (1975), note that the \eqn{W'}-statistic is intuitively appealing
    because it is the squared Pearson correlation coefficient associated with a normal
    probability plot.  That is, it is the squared correlation between the ordered
    sample values \eqn{\underline{x}} and the expected normal order statistics
    \eqn{\underline{m}}:
    \deqn{W' = r(\underline{b}, \underline{x})^2  = r(\underline{m}, \underline{x})^2 \;\;\;\;\;\; (15)}
    Shapiro and Francia (1972) present a table of empirical percentage points for \eqn{W'}
    based on a Monte Carlo simulation.  It can be shown that the asymptotic null
    distributions of \eqn{W} and \eqn{W'} are identical, but convergence is very slow
    (Verrill and Johnson, 1988).
    \cr

    \emph{The Weisberg-Bingham Approximation to the W'-Statistic} \cr
    Weisberg and Bingham (1975) introduced an approximation of the Shapiro-Francia
    \eqn{W'}-statistic that is easier to compute.  They suggested using Blom scores
    (Blom, 1958, pp.68--75) to approximate the element of \eqn{\underline{m}}:
    \deqn{\tilde{W}' = \frac{(\sum_{i=1}^n c_i x_i)^2}{\sum_{i=1}^n (x_i - \bar{x})^2} \;\;\;\;\;\; (16)}
    where the quantity \eqn{c_i} is the \eqn{i}'th element of the vector
    \eqn{\underline{c}} defined by:
    \deqn{\underline{c} = \frac{\underline{\tilde{m}}}{[\underline{\tilde{m}}^T \underline{\tilde{m}}]^{1/2}} \;\;\;\;\;\; (17)}
    and
    \deqn{\tilde{m}_i = \Phi^{-1}[\frac{i - (3/8)}{n + (1/4)}] \;\;\;\;\;\;  (18)}
    and \eqn{\Phi} denotes the standard normal cdf.  That is, the values of the
    elements of \eqn{\underline{m}} in Equation (14) are replaced with their estimates
    based on the usual plotting positions for a normal distribution.
    \cr

    \emph{Royston's Approximation to the Shapiro-Wilk W-Test} \cr
    Royston (1992a) presents an approximation for the coefficients \eqn{\underline{a}}
    necessary to compute the Shapiro-Wilk \eqn{W}-statistic, and also a transformation
    of the \eqn{W}-statistic that has approximately a standard normal distribution
    under the null hypothesis.

    Noting that, up to a constant, the components of \eqn{\underline{b}} in
    Equation (14) and \eqn{\underline{c}} in Equation (17) differ from those of
    \eqn{\underline{a}} in Equation (2) mainly in the first and last two components,
    Royston (1992a) used the approximation \eqn{\underline{c}} as the basis for
    approximating \eqn{\underline{a}} using polynomial (quintic) regression analysis.
    For \eqn{4 \le n \le 1000}, the approximation gave the following equations for the
    last two (and hence first two) components of \eqn{\underline{a}}:
    \deqn{\tilde{a}_n = c_n + 0.221157 y - 0.147981 y^2 - 2.071190 y^3 + 4.434685 y^4 - 2.706056 y^5 \;\;\;\;\;\; (19)}
    \deqn{\tilde{a}_{n-1} = c_{n-1} + 0.042981 y - 0.293762 y^2 - 1.752461 y^3 + 5.682633 y^4 - 3.582633 y^5 \;\;\;\;\;\; (20)}
    where
    \deqn{y = \sqrt{n} \;\;\;\;\;\; (21)}
    The other components are computed as:
    \deqn{\tilde{a}_i = \frac{\tilde{m}_i}{\sqrt{\eta}} \;\;\;\;\;\; (22)}
    for \eqn{i = 2, \ldots , n-1} if \eqn{n \le 5}, or \eqn{i = 3, \ldots, n-2} if
    \eqn{n > 5}, where
    \deqn{\eta = \frac{\underline{\tilde{m}}^T \underline{\tilde{m}} - 2 \tilde{m}_n^2}{1 - 2 \tilde{a}_n^2} \;\;\;\;\;\; (23)}
    if \eqn{n \le 5}, and
    \deqn{\eta = \frac{\underline{\tilde{m}}^T \underline{\tilde{m}} - 2 \tilde{m}_n^2 - 2 \tilde{m}_{n-1}^2}{1 - 2 \tilde{a}_n^2 - 2 \tilde{a}_{n-1}^2} \;\;\;\;\;\; (24)}
    if \eqn{n > 5}.

    Royston (1992a) found his approximation to \eqn{\underline{a}} to be accurate to
    at least \eqn{\pm 1} in the third decimal place over all values of \eqn{i} and
    selected values of \eqn{n}, and also found that critical percentage points of
    \eqn{W} based on his approximation agreed closely with the exact critical
    percentage points calculated by Verrill and Johnson (1988).
    \cr

    \emph{Transformation of the Null Distribution of W to Normality} \cr
    In order to compute a p-value associated with a particular value of \eqn{W},
    Royston (1992a) approximated the distribution of \eqn{(1-W)} by a
    \link[=Lognormal3]{three-parameter lognormal distribution} for \eqn{4 \le n \le 11},
    and the upper half of the distribution of \eqn{(1-W)} by a two-parameter
    \link[stats:Lognormal]{lognormal distribution} for \eqn{12 \le n \le 2000}.
    Setting
    \deqn{z = \frac{w - \mu}{\sigma} \;\;\;\;\;\; (25)}
    the p-value associated with \eqn{W} is given by:
    \deqn{p = 1 - \Phi(z) \;\;\;\;\;\; (26)}
    For \eqn{4 \le n \le 11}, the quantities necessary to compute \eqn{z} are given by:
    \deqn{w = -log[\gamma - log(1 - W)] \;\;\;\;\;\; (27)}
    \deqn{\gamma = -2.273 + 0.459 n \;\;\;\;\;\; (28)}
    \deqn{\mu = 0.5440 - 0.39978 n + 0.025054 n^2 - 0.000671 n^3 \;\;\;\;\;\; (29)}
    \deqn{\sigma = exp(1.3822 - 0.77857 n + 0.062767 n^2 - 0.0020322 n^3) \;\;\;\;\;\; (30)}
    For \eqn{12 \le n \le 2000}, the quantities necessary to compute \eqn{z} are given
    by:
    \deqn{w = log(1 - W) \;\;\;\;\;\; (31)}
    \deqn{\gamma = log(n) \;\;\;\;\;\; (32)}
    \deqn{\mu = -1.5861 - 0.31082 y - 0.083751 y^2 + 0.00038915 y^3 \;\;\;\;\;\; (33)}
    \deqn{\sigma = exp(-0.4803 - 0.082676 y + 0.0030302 y^2) \;\;\;\;\;\; (34)}
    For the last approximation when \eqn{12 \le n \le 2000}, Royston (1992a) claims
    this approximation is actually valid for sample sizes up to \eqn{n = 5000}.
    \cr

    \emph{Modification for the Three-Parameter Lognormal Distribution} \cr
    When \code{distribution="lnorm3"}, the function \code{gofTest} assumes the vector
    \eqn{\underline{x}} is a random sample from a
    \link[=Lognormal3]{three-parameter lognormal distribution}.  It estimates the
    threshold parameter via the zero-skewness method (see \code{\link{elnorm3}}), and
    then performs the Shapiro-Wilk goodness-of-fit test for normality on
    \eqn{log(x-\hat{\gamma})} where \eqn{\hat{\gamma}} is the estimated threshold
    parmater.  Because the threshold parameter has to be estimated, however, the
    p-value associated with the computed z-statistic will tend to be conservative
    (larger than it should be under the null hypothesis).  Royston (1992b) proposed
    the following transformation of the z-statistic:
    \deqn{z' = \frac{z - \mu_z}{\sigma_z} \;\;\;\;\;\; (35)}
    where for \eqn{5 \le n \le 11},
    \deqn{\mu_z = -3.8267 + 2.8242 u - 0.63673 u^2 - 0.020815 v \;\;\;\;\;\; (36)}
    \deqn{\sigma_z = -4.9914 + 8.6724 u - 4.27905 u^2 + 0.70350 u^3 - 0.013431 v \;\;\;\;\;\; (37)}
    and for \eqn{12 \le n \le 2000},
    \deqn{\mu_z =  -3.7796 + 2.4038 u - 0.6675 u^2 - 0.082863 u^3 - 0.0037935 u^4 - 0.027027 v - 0.0019887 vu \;\;\;\;\;\; (38)}
    \deqn{\sigma_z =  2.1924 - 1.0957 u + 0.33737 u^2 - 0.043201 u^3 + 0.0019974 u^4 - 0.0053312 vu   \;\;\;\;\;\; (39)}
    where
    \deqn{u = log(n) \;\;\;\;\;\; (40)}
    \deqn{v = u (\hat{\sigma} - \hat{\sigma}^2) \;\;\;\;\;\; (41)}
    \deqn{\hat{\sigma}^2 = \frac{1}{n-1} \sum_{i=1}^n (y_i - \bar{y})^2 \;\;\;\;\;\; (42)}
    \deqn{y_i = log(x_i - \hat{\gamma}) \;\;\;\;\;\; (43)}
    and \eqn{\gamma} denotes the threshold parameter.  The p-value associated with
    this test is then given by:
    \deqn{p = 1 - \Phi(z') \;\;\;\;\;\; (44)}

    \emph{Testing Goodness-of-Fit for Any Continuous Distribution} \cr
    The function \code{gofTest} extends the Shapiro-Wilk test to test for
    goodness-of-fit for any continuous distribution by using the idea of
    Chen and Balakrishnan (1995), who proposed a general purpose approximate
    goodness-of-fit test based on the Cramer-von Mises or Anderson-Darling
    goodness-of-fit tests for normality.  The function \code{gofTest} modifies the
    approach of Chen and Balakrishnan (1995) by using the same first 2 steps, and then
    applying the Shapiro-Wilk test:
    \enumerate{
      \item Let \eqn{\underline{x} = x_1, x_2, \ldots, x_n} denote the vector of
      \eqn{n} \bold{\emph{ordered}} observations.
      Compute cumulative probabilities for each \eqn{x_i} based on the
      cumulative distribution function for the hypothesized distribution.  That is,
      compute \eqn{p_i = F(x_i, \hat{\theta})} where \eqn{F(x, \theta)} denotes the
      hypothesized cumulative distribution function with parameter(s) \eqn{\theta},
      and \eqn{\hat{\theta}} denotes the estimated parameter(s).

      \item Compute standard normal deviates based on the computed cumulative
      probabilities: \cr
      \eqn{y_i = \Phi^{-1}(p_i)}

      \item Perform the Shapiro-Wilk goodness-of-fit test on the \eqn{y_i}'s.
    }

    \item \bold{Shapiro-Francia Goodness-of-Fit Test} (\code{test="sf"}).

    The Shapiro-Francia goodness-of-fit test (Shapiro and Francia, 1972;
    Weisberg and Bingham, 1975; Royston, 1992c) is also one of the most commonly
    used goodness-of-fit tests for normality.  You can use it to test the following
    hypothesized distributions:
    \link{Normal}, \link{Lognormal}, \link[=ZeroModifiedNormal]{Zero-Modified Normal},
    or \link[=ZeroModifiedLognormal]{Zero-Modified Lognormal (Delta)}.  In addition,
    you can also use it to test the null hypothesis of any continuous distribution
    that is available (see the help file for \code{\link{Distribution.df}}).  See the
    section \emph{Testing Goodness-of-Fit for Any Continuous Distribution} above for
    an explanation of how this is done.
    \cr

    \emph{Royston's Transformation of the Shapiro-Francia W'-Statistic to Normality} \cr
    Equation (13) above gives the formula for the Shapiro-Francia W'-statistic, and
    Equation (16) above gives the formula for Weisberg-Bingham approximation to the
    W'-statistic (denoted \eqn{\tilde{W}'}).  Royston (1992c) presents an algorithm
    to transform the \eqn{\tilde{W}'}-statistic so that its null distribution is
    approximately a standard normal.  For \eqn{5 \le n \le 5000},
    Royston (1992c) approximates the distribution of \eqn{(1-\tilde{W}')} by a
    \link[stats:Lognormal]{lognormal distribution}.  Setting
    \deqn{z = \frac{w-\mu}{\sigma} \;\;\;\;\;\; (45)}
    the p-value associated with \eqn{\tilde{W}'} is given by:
    \deqn{p = 1 - \Phi(z) \;\;\;\;\;\; (46)}
    The quantities necessary to compute \eqn{z} are given by:
    \deqn{w = log(1 - \tilde{W}') \;\;\;\;\;\; (47)}
    \deqn{\nu = log(n) \;\;\;\;\;\; (48)}
    \deqn{u = log(\nu) - \nu \;\;\;\;\;\; (49)}
    \deqn{\mu = -1.2725 + 1.0521 u \;\;\;\;\;\; (50)}
    \deqn{v = log(\nu) + \frac{2}{\nu} \;\;\;\;\;\; (51)}
    \deqn{\sigma = 1.0308 - 0.26758 v \;\;\;\;\;\; (52)}

    \emph{Testing Goodness-of-Fit for Any Continuous Distribution} \cr
    The function \code{gofTest} extends the Shapiro-Francia test to test for
    goodness-of-fit for any continuous distribution by using the idea of
    Chen and Balakrishnan (1995), who proposed a general purpose approximate
    goodness-of-fit test based on the Cramer-von Mises or Anderson-Darling
    goodness-of-fit tests for normality.  The function \code{gofTest} modifies the
    approach of Chen and Balakrishnan (1995) by using the same first 2 steps, and then
    applying the Shapiro-Francia test:
    \enumerate{
      \item Let \eqn{\underline{x} = x_1, x_2, \ldots, x_n} denote the vector of
      \eqn{n} \bold{\emph{ordered}} observations.
      Compute cumulative probabilities for each \eqn{x_i} based on the
      cumulative distribution function for the hypothesized distribution.  That is,
      compute \eqn{p_i = F(x_i, \hat{\theta})} where \eqn{F(x, \theta)} denotes the
      hypothesized cumulative distribution function with parameter(s) \eqn{\theta},
      and \eqn{\hat{\theta}} denotes the estimated parameter(s).

      \item Compute standard normal deviates based on the computed cumulative
      probabilities: \cr
      \eqn{y_i = \Phi^{-1}(p_i)}

      \item Perform the Shapiro-Francia goodness-of-fit test on the \eqn{y_i}'s.
    }


    \item \bold{Probability Plot Correlation Coefficient (PPCC) Goodness-of-Fit Test} (\code{test="ppcc"}).

    The PPPCC goodness-of-fit test (Filliben, 1975; Looney and Gulledge, 1985) can be
    used to test the following hypothesized distributions:
    \link{Normal}, \link{Lognormal},
    \link[=ZeroModifiedNormal]{Zero-Modified Normal}, or
    \link[=ZeroModifiedLognormal]{Zero-Modified Lognormal (Delta)}.  In addition,
    you can also use it to test the null hypothesis of any continuous distribution that
    is available (see the help file for \code{\link{Distribution.df}}).
    The function \code{gofTest} computes the PPCC test
    statistic using Blom plotting positions.

    Filliben (1975) proposed using the correlation coefficient \eqn{r} from a
    \link[=qqPlot]{normal probability plot} to perform a goodness-of-fit test for
    normality, and he provided a table of critical values for \eqn{r} under the
    for samples sizes between 3 and 100.  Vogel (1986) provided an additional table
    for sample sizes between 100 and 10,000.

    Looney and Gulledge (1985) investigated the characteristics of Filliben's
    probability plot correlation coefficient (PPCC) test using the plotting position
    formulas given in Filliben (1975), as well as three other plotting position
    formulas:  Hazen plotting positions, Weibull plotting positions, and Blom plotting
    positions (see the help file for \code{\link{qqPlot}} for an explanation of these
    plotting positions).  They concluded that the PPCC test based on Blom plotting
    positions performs slightly better than tests based on other plotting positions, and
    they provide a table of empirical percentage points for the distribution of \eqn{r}
    based on Blom plotting positions.

    The function \code{gofTest} computes the PPCC test statistic \eqn{r} using Blom
    plotting positions.  It can be shown that the square of this statistic is
    equivalent to the Weisberg-Bingham Approximation to the Shapiro-Francia
    W'-Test (Weisberg and Bingham, 1975; Royston, 1993).  Thus the PPCC
    goodness-of-fit test is equivalent to the Shapiro-Francia goodness-of-fit test.
    \cr

    \item \bold{Anderson-Darling Goodness-of-Fit Test} (\code{test="ad"}).

    The Anderson-Darling goodness-of-fit test (Stephens, 1986a; Thode, 2002) can be used to test the
    following hypothesized distributions:
    \link{Normal}, \link{Lognormal}, \link[=ZeroModifiedNormal]{Zero-Modified Normal},
    or \link[=ZeroModifiedLognormal]{Zero-Modified Lognormal (Delta)}.

    When \code{test="ad"}, the function \code{gofTest} calls the function
    \code{\link[nortest]{ad.test}} in the package \pkg{nortest}.  Documentation from that
    package is as follows:

    The Anderson-Darling test is an EDF omnibus test for the composite hypothesis of normality.
    The test statistic is:
    \deqn{A = -n - \frac{1}{n} \sum_{i=1}^n [2i - 1][ln(p_{(i)}) + ln(1 - p_{(n-i+1)})]}
    where \eqn{p_{(i)} = \Phi([x_{(i)} - \bar{x}]/s)}.  Here, \eqn{\Phi} is the cumulative
    distribution function of the standard normal distribution, and \eqn{\bar{x}} and \eqn{s}
    are mean and standard deviation of the data values. The p-value is computed from the
    modified statistic \eqn{Z = A (1.0 + 0.75/n + 2.25/n^2)} according to Table 4.9 in
    Stephens [(1986a)].
    \cr

    \item \bold{Cramer-von Mises Goodness-of-Fit Test} (\code{test="cvm"}).

    The Cramer-von Mises goodness-of-fit test (Stephens, 1986a; Thode, 2002) can be used to test the
    following hypothesized distributions:
    \link{Normal}, \link{Lognormal}, \link[=ZeroModifiedNormal]{Zero-Modified Normal},
    or \link[=ZeroModifiedLognormal]{Zero-Modified Lognormal (Delta)}.

    When \code{test="cvm"}, the function \code{gofTest} calls the function
    \code{\link[nortest]{cvm.test}} in the package \pkg{nortest}.  Documentation from that
    package is as follows:

    The Cramer-von Mises test is an EDF omnibus test for the composite hypothesis of normality.
    The test statistic is:
    \deqn{W = \frac{1}{12n} + \sum_{i=1}^n \left(p_{(i)} - \frac{2i-1}{2n}\right)^2}
    where \eqn{p_{(i)} = \Phi([x_{(i)} - \bar{x}]/s)}.  Here, \eqn{\Phi} is the cumulative
    distribution function of the standard normal distribution, and \eqn{\bar{x}} and \eqn{s}
    are mean and standard deviation of the data values. The p-value is computed from the
    modified statistic \eqn{Z = W (1.0 + 0.75/n)} according to Table 4.9 in Stephens [(1986a)].
    \cr

    \item \bold{Lilliefors Goodness-of-Fit Test} (\code{test="lillie"}).

    The Lilliefors goodness-of-fit test (Stephens, 1974; Dallal and Wilkinson, 1986; Thode, 2002)
    can be used to test the following hypothesized distributions:
    \link{Normal}, \link{Lognormal}, \link[=ZeroModifiedNormal]{Zero-Modified Normal},
    or \link[=ZeroModifiedLognormal]{Zero-Modified Lognormal (Delta)}.

    When \code{test="lillie"}, the function \code{gofTest} calls the function
    \code{\link[nortest]{lillie.test}} in the package \pkg{nortest}.  Documentation from that
    package is as follows:

    The Lilliefors (Kolmogorov-Smirnov) test is an EDF omnibus test for the
    composite hypothesis of normality.  The test statistic is the
    maximal absolute difference between empirical and hypothetical cumulative
    distribution function.  It may be computed as \eqn{D = max\{D^+, D^-\}} with
    \deqn{D^+ = \max_{i = 1, \ldots, n} \{i/n - p_{(i)}\}, \;\; D^- = \max_{i = 1, \ldots, n} \{p_{(i)} - (i-1)/n\} }
    where \eqn{p_{(i)} = \Phi([x_{(i)} - \bar{x}]/s)}.  Here, \eqn{\Phi} is the cumulative
    distribution function of the standard normal distribution, and \eqn{\bar{x}} and \eqn{s}
    are mean and standard deviation of the data values. The p-value is computed from
    the Dallal-Wilkinson (1986) formula, which is claimed to be only reliable when the
    p-value is smaller than 0.1.  If the Dallal-Wilkinson p-value turns out to be
    greater than 0.1, then the p-value is computed from the distribution of the
    modified statistic \eqn{Z = D (\sqrt{n} - 0.01 + 0.85/\sqrt{n})}, see Stephens (1974),
    the actual p-value formula being obtained by a simulation and approximation process.
    \cr

    \item \bold{Zero-Skew Goodness-of-Fit Test} (\code{test="skew"}).

    The Zero-skew goodness-of-fit test (D'Agostino, 1970) can be used to test the
    following hypothesized distributions:
    \link{Normal}, \link{Lognormal}, \link[=ZeroModifiedNormal]{Zero-Modified Normal},
    or \link[=ZeroModifiedLognormal]{Zero-Modified Lognormal (Delta)}.

    When \code{test="skew"}, the function \code{gofTest} tests the null hypothesis
    that the skew of the distribution is 0:
    \deqn{H_0: \sqrt{\beta}_1 = 0 \;\;\;\;\;\; (53)}
    where
    \deqn{\sqrt{\beta}_1 = \frac{\mu_3}{\mu_2^{3/2}} \;\;\;\;\;\; (54)}
    and the quantity \eqn{\mu_r} denotes the \eqn{r}'th moment about the mean
    (also called the \eqn{r}'th central moment).  The quantity \eqn{\sqrt{\beta_1}}
    is called the coefficient of skewness, and is estimated by:
    \deqn{\sqrt{b}_1 = \frac{m_3}{m_2^{3/2}} \;\;\;\;\;\; (55)}
    where
    \deqn{m_r = \frac{1}{n} \sum_{i=1}^n (x_i - \bar{x})^r \;\;\;\;\;\; (56)}
    denotes the \eqn{r}'th sample central moment.

    The possible alternative hypotheses are:
    \deqn{H_a: \sqrt{\beta}_1 \ne 0 \;\;\;\;\;\; (57)}
    \deqn{H_a: \sqrt{\beta}_1 < 0 \;\;\;\;\;\; (58)}
    \deqn{H_a: \sqrt{\beta}_1 > 0 \;\;\;\;\;\; (59)}
    which correspond to \code{alternative="two-sided"}, \code{alternative="less"}, and \cr
    \code{alternative="greater"}, respectively.

    To test the null hypothesis of zero skew, D'Agostino (1970) derived an
    approximation to the distribution of \eqn{\sqrt{b_1}} under the null hypothesis of
    zero-skew, assuming the observations comprise a random sample from a normal
    (Gaussian) distribution.  Based on D'Agostino's approximation, the statistic
    \eqn{Z} shown below is assumed to follow a standard normal distribution and is
    used to compute the p-value associated with the test of \eqn{H_0}:
    \deqn{Z = \delta \;\; log\{ \frac{Y}{\alpha} + [(\frac{Y}{\alpha})^2 + 1]^{1/2} \} \;\;\;\;\;\; (60)}
    where
    \deqn{Y = \sqrt{b_1} [\frac{(n+1)(n+3)}{6(n-2)}]^{1/2} \;\;\;\;\;\; (61)}
    \deqn{\beta_2 = \frac{3(n^2 + 27n - 70)(n+1)(n+3)}{(n-2)(n+5)(n+7)(n+9)} \;\;\;\;\;\; (62)}
    \deqn{W^2 = -1 + \sqrt{2\beta_2 - 2} \;\;\;\;\;\; (63)}
    \deqn{\delta = 1 / \sqrt{log(W)} \;\;\;\;\;\; (64)}
    \deqn{\alpha = [2 / (W^2 - 1)]^{1/2} \;\;\;\;\;\; (65)}
    When the sample size \eqn{n} is at least 150, a simpler approximation may be
    used in which \eqn{Y} in Equation (61) is assumed to follow a standard normal
    distribution and is used to compute the p-value associated with the hypothesis
    test.
    \cr

    \item \bold{Kolmogorov-Smirnov Goodness-of-Fit Test} (\code{test="ks"}).

    When \code{test="ks"}, the function \code{gofTest} calls the \R function
    \code{\link{ks.test}} to compute the test statistic and p-value.  Note that for the
    one-sample case, the distribution parameters
    should be pre-specified and not estimated from the data, and if the distribution parameters
    are estimated from the data you will receive a warning that this test is very conservative
    (Type I error smaller than assumed; high Type II error) in this case.
    \cr

    \item \bold{ProUCL Kolmogorov-Smirnov Goodness-of-Fit Test for Gamma} (\code{test="proucl.ks.gamma"}).

    When \code{test="proucl.ks.gamma"}, the function \code{gofTest} calls the \R function
    \code{\link{ks.test}} to compute the Kolmogorov-Smirnov test statistic based on the
    maximum likelihood estimates of the shape and scale parameters (see \code{\link{egamma}}).
    The p-value is computed based on the simulated critical values given in
    \code{\link{ProUCL.Crit.Vals.for.KS.Test.for.Gamma.array}} (USEPA, 2015).
    The sample size must be between 5 and 1000, and the value of the maximum likelihood
    estimate of the shape parameter must be between 0.025 and 50.  The critical value
    for the test statistic is computed using the simulated critical values and
    linear interpolation.
    \cr

    \item \bold{ProUCL Anderson-Darling Goodness-of-Fit Test for Gamma} (\code{test="proucl.ad.gamma"}).

    When \code{test="proucl.ad.gamma"}, the function \code{gofTest} computes the
    Anderson-Darling test statistic (Stephens, 1986a, p.101) based on the
    maximum likelihood estimates of the shape and scale parameters (see \code{\link{egamma}}).
    The p-value is computed based on the simulated critical values given in
    \code{\link{ProUCL.Crit.Vals.for.AD.Test.for.Gamma.array}} (USEPA, 2015).
    The sample size must be between 5 and 1000, and the value of the maximum likelihood
    estimate of the shape parameter must be between 0.025 and 50.  The critical value
    for the test statistic is computed using the simulated critical values and
    linear interpolation.
    \cr

    \item \bold{Chi-Squared Goodness-of-Fit Test} (\code{test="chisq"}).

    The method used by \code{gofTest} is a modification of what is used for \code{\link{chisq.test}}.
    If the hypothesized distribution function is completely specified, the degrees of
    freedom are \eqn{m-1} where \eqn{m} denotes the number of classes.  If any parameters
    are estimated, the degrees of freedom depend on the method of estimation.
    The function \code{gofTest} follows the convention of computing
    degrees of freedom as \eqn{m-1-k}, where \eqn{k} is the number of parameters estimated.
    It can be shown that if the parameters are estimated by maximum likelihood, the degrees of
    freedom are bounded between \eqn{m-1} and \eqn{m-1-k}.  Therefore, especially when the
    sample size is small, it is important to compare the test statistic to the chi-squared
    distribution with both \eqn{m-1} and \eqn{m-1-k} degrees of freedom.  See
    Kendall and Stuart (1991, Chapter 30) for a more complete discussion.

    The distribution theory of chi-square statistics is a large sample theory.
    The expected cell counts are assumed to be at least moderately large.
    As a rule of thumb, each should be at least 5.  Although authors have found this rule
    to be conservative (especially when the class probabilities are not too different
    from each other), the user should regard p-values with caution when expected cell
    counts are small.
    \cr

    \item \bold{Wilk-Shapiro Goodness-of-Fit Test for Uniform [0, 1] Distribution} (\code{test="ws"}).

    Wilk and Shapiro (1968) suggested this test in the context of jointly testing several
    independent samples for normality simultaneously.  If \eqn{p_1, p_2, \ldots, p_n} denote
    the p-values associated with the test for normality of \eqn{n} independent samples, then
    under the null hypothesis that all \eqn{n} samples come from a normal distribution, the
    p-values are a random sample of \eqn{n} observations from a Uniform [0,1] distribution,
    that is a Uniform distribution with minimum 0 and maximum 1.  Wilk and Shapiro (1968)
    suggested two different methods for testing whether the p-values come from a Uniform [0, 1]
    distribution:

    \itemize{
      \item \emph{Test Based on Normal Scores.} Under the null hypothesis,
      the normal scores
      \deqn{\Phi^{-1}(p_1), \Phi^{-1}(p_2), \ldots, \Phi^{-1}(p_n)}
      are a random sample of \eqn{n} observations from a standard normal distribution.
      Wilk and Shapiro (1968) denote the \eqn{i}'th normal score by
      \deqn{G_i = \Phi^{-1}(p_i) \;\;\;\;\;\; (66)}
      and note that under the null hypothesis, the quantity \eqn{G} defined as
      \deqn{G = \frac{1}{\sqrt{n}} \, \sum^n_{1}{G_i} \;\;\;\;\;\; (67)}
      has a standard normal distribution.  Wilk and Shapiro (1968) were
      interested in the alternative hypothesis that some of the \eqn{n}
      independent samples did not come from a normal distribution and hence
      would be associated with smaller p-values than expected under the
      null hypothesis, which translates to the alternative that the cdf for
      the distribution of the p-values is greater than the cdf of a
      Uniform [0, 1] distribution (\code{alternative="greater"}).  In terms
      of the test statistic \eqn{G}, this alternative hypothesis would
      tend to make \eqn{G} smaller than expected, so the p-value is given by
      \eqn{\Phi(G)}.  For the one-sided lower alternative that the cdf for the
      distribution of p-values is less than the cdf for a Uniform [0, 1]
      distribution, the p-value is given by
      \deqn{p = 1 - \Phi(G) \;\;\;\;\;\; (68)}.

      \item \emph{Test Based on Chi-Square Scores.}  Under the null hypothesis, the
      chi-square scores
      \deqn{-2 \, log(p_1), -2 \, log(p_2), \ldots, -2 \, log(p_n)}
      are a random sample of \eqn{n} observations from a chi-square distribution with
      2 degrees of freedom (Fisher, 1950).  Wilk and Shapiro (1968) denote the
      \eqn{i}'th chi-square score by
      \deqn{C_i = -2 \, log(p_i) \;\;\;\;\;\; (69)}
      and note that under the null hypothesis, the quantity \eqn{C} defined as
      \deqn{C = \sum^n_{1}{C_i} \;\;\;\;\;\; (70)}
      has a chi-square distribution with \eqn{2n} degrees of freedom.
      Wilk and Shapiro (1968) were
      interested in the alternative hypothesis that some of the \eqn{n}
      independent samples did not come from a normal distribution and hence
      would be associated with smaller p-values than expected under the
      null hypothesis, which translates to the alternative that the cdf for
      the distribution of the p-values is greater than the cdf of a
      Uniform [0, 1] distribution (\code{alternative="greater"}).  In terms
      of the test statistic \eqn{C}, this alternative hypothesis would
      tend to make \eqn{C} larger than expected, so the p-value is given by
      \deqn{p = 1 - F_{2n}(C) \;\;\;\;\;\; (71)}
      where \eqn{F_2n} denotes the cumulative distribution function of the
      chi-square distribution with \eqn{2n} degrees of freedom.
      For the one-sided lower alternative that
      the cdf for the distribution of p-values is less than the cdf for a
      Uniform [0, 1] distribution, the p-value is given by
      \deqn{p = F_{2n}(C) \;\;\;\;\;\; (72)}
   }
  }
}
\value{
  a list of class \code{"gof"} containing the results of the goodness-of-fit test, unless
  the two-sample \cr
  Kolmogorov-Smirnov test is used, in which case the value is a list of
  class \code{"gofTwoSample"}.  Objects of class \code{"gof"} and \code{"gofTwoSample"}
  have special printing and plotting methods.  See the help files for \code{\link{gof.object}}
  and \code{\link{gofTwoSample.object}} for details.
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

  Razali, N.M., and Y.B. Wah. (2011). Power Comparisons of Shapiro-Wilk, Kolmogorov-Smirnov,
  Lilliefors, and Anderson-Darling Tests.  \emph{Journal of Statistical Modeling and Analytics}
  \bold{2}(1), 21--33.

  Romao, X., Delgado, R., and A. Costa. (2010). An Empirical Power Comparison of Univariate
  Goodness-of-Fit Tests for Normality.  \emph{Journal of Statistical Computation and Simulation}
  \bold{80}(5), 545--591.

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

  Stephens, M.A. (1974). EDF Statistics for Goodness of Fit and Some Comparisons.
  \emph{Journal of the American Statistical Association} \bold{69}, 730-737.

  Stephens, M.A. (1986a). Tests Based on EDF Statistics. In D'Agostino, R. B., and M.A. Stevens, eds.
  \emph{Goodness-of-Fit Techniques}. Marcel Dekker, New York.

  Thode Jr., H.C. (2002). \emph{Testing for Normality}. Marcel Dekker, New York.

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

  Juergen Gross and Uwe Ligges for the Anderson-Darling, Carmer-von Mises, and Lilliefors tests called
  from the package \pkg{nortest}.
}
\note{
  The Shapiro-Wilk test (Shapiro and Wilk, 1965) and the Shapiro-Francia test
  (Shapiro and Francia, 1972) are probably the two most commonly used hypothesis tests to
  test departures from normality.  The Shapiro-Wilk test is most powerful at detecting
  short-tailed (platykurtic) and skewed distributions, and least powerful against
  symmetric, moderately long-tailed (leptokurtic) distributions.  Conversely, the
  Shapiro-Francia test is more powerful against symmetric long-tailed distributions and
  less powerful against short-tailed distributions (Royston, 1992b; 1993).
  In general, the Shapiro-Wilk and Shapiro-Francia tests outperform the Anderson-Darling
  test, which in turn outperforms the Cramer-von Mises test, which in turn
  outperforms the Lilliefors test (Stephens, 1986a; Razali and Wah, 2011; Romao et al., 2010).

  The zero-skew goodness-of-fit test for normality is one of several tests that have
  been proposed to test the assumption of a normal distribution (D'Agostino, 1986b).
  This test has been included mainly because it is called by \code{\link{elnorm3}}.
  Ususally, the Shapiro-Wilk or Shapiro-Francia test is preferred to this test, unless
  the direction of the alternative to normality (e.g., positive skew) is known
  (D'Agostino, 1986b, pp. 405--406).

  Kolmogorov (1933) introduced a goodness-of-fit test to test the hypothesis that a
  random sample of \eqn{n} observations \bold{x} comes from a specific hypothesized distribution
  with cumulative distribution function \eqn{H}.  This test is now usually called the
  one-sample Kolmogorov-Smirnov goodness-of-fit test.  Smirnov (1939) introduced a
  goodness-of-fit test to test the hypothesis that a random sample of \eqn{n}
  observations \bold{x} comes from the same distribution as a random sample of
  \eqn{m} observations \bold{y}.  This test is now usually called the two-sample
  Kolmogorov-Smirnov goodness-of-fit test.  Both tests are based on the maximum
  vertical distance between two cumulative distribution functions.  For the one-sample problem
  with a small sample size, the Kolmogorov-Smirnov test may be preferred over the chi-squared
  goodness-of-fit test since the KS-test is exact, while the chi-squared test is based on
  an asymptotic approximation.

  The chi-squared test, introduced by Pearson in 1900, is the oldest and best known
  goodness-of-fit test.  The idea is to reduce the goodness-of-fit problem to a
  multinomial setting by comparing the observed cell counts with their expected values
  under the null hypothesis.  Grouping the data sacrifices information, especially if the
  hypothesized distribution is continuous.  On the other hand, chi-squared tests can be be
  applied to any type of variable: continuous, discrete, or a combination of these.

  The Wilk-Shapiro (1968) tests for a Uniform [0, 1] distribution were introduced in the context
  of testing whether several independent samples all come from normal distributions, with
  possibly different means and variances.  The function \code{\link{gofGroupTest}} extends
  this idea to allow you to test whether several independent samples come from the same
  distribution (e.g., gamma, extreme value, etc.), with possibly different parameters.

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
  \code{\link{rosnerTest}}, \code{\link{gof.object}}, \code{\link{print.gof}},
  \code{\link{plot.gof}},
  \code{\link{shapiro.test}}, \code{\link{ks.test}}, \code{\link{chisq.test}},
  \link{Normal}, \link{Lognormal}, \link{Lognormal3},
  \link{Zero-Modified Normal}, \link{Zero-Modified Lognormal (Delta)},
  \code{\link{enorm}}, \code{\link{elnorm}}, \code{\link{elnormAlt}},
  \code{\link{elnorm3}}, \code{\link{ezmnorm}}, \code{\link{ezmlnorm}},
  \code{\link{ezmlnormAlt}}, \code{\link{qqPlot}}.
}
\examples{
  # Generate 20 observations from a gamma distribution with
  # parameters shape = 2 and scale = 3 then run various
  # goodness-of-fit tests.
  # (Note:  the call to set.seed lets you reproduce this example.)

  set.seed(47)
  dat <- rgamma(20, shape = 2, scale = 3)

  # Shapiro-Wilk generalized goodness-of-fit test
  #----------------------------------------------
  gof.list <- gofTest(dat, distribution = "gamma")
  gof.list

  #Results of Goodness-of-Fit Test
  #-------------------------------
  #
  #Test Method:                     Shapiro-Wilk GOF Based on
  #                                 Chen & Balakrisnan (1995)
  #
  #Hypothesized Distribution:       Gamma
  #
  #Estimated Parameter(s):          shape = 1.909462
  #                                 scale = 4.056819
  #
  #Estimation Method:               mle
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Test Statistic:                  W = 0.9834958
  #
  #Test Statistic Parameter:        n = 20
  #
  #P-value:                         0.970903
  #
  #Alternative Hypothesis:          True cdf does not equal the
  #                                 Gamma Distribution.

  dev.new()
  plot(gof.list)

  #----------

  # Redo the example above, but use the bias-corrected mle

  gofTest(dat, distribution = "gamma",
    est.arg.list = list(method = "bcmle"))

  #Results of Goodness-of-Fit Test
  #-------------------------------
  #
  #Test Method:                     Shapiro-Wilk GOF Based on
  #                                 Chen & Balakrisnan (1995)
  #
  #Hypothesized Distribution:       Gamma
  #
  #Estimated Parameter(s):          shape = 1.656376
  #                                 scale = 4.676680
  #
  #Estimation Method:               bcmle
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Test Statistic:                  W = 0.9834346
  #
  #Test Statistic Parameter:        n = 20
  #
  #P-value:                         0.9704046
  #
  #Alternative Hypothesis:          True cdf does not equal the
  #                                 Gamma Distribution.

  #----------

  # Komogorov-Smirnov goodness-of-fit test (pre-specified parameters)
  #------------------------------------------------------------------

  gofTest(dat, test = "ks", distribution = "gamma",
    param.list = list(shape = 2, scale = 3))

  #Results of Goodness-of-Fit Test
  #-------------------------------
  #
  #Test Method:                     Kolmogorov-Smirnov GOF
  #
  #Hypothesized Distribution:       Gamma(shape = 2, scale = 3)
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Test Statistic:                  ks = 0.2313878
  #
  #Test Statistic Parameter:        n = 20
  #
  #P-value:                         0.2005083
  #
  #Alternative Hypothesis:          True cdf does not equal the
  #                                 Gamma(shape = 2, scale = 3)
  #                                 Distribution.

  #----------

  # ProUCL Version of Komogorov-Smirnov goodness-of-fit test
  # for a Gamma Distribution (estimated parameters)
  #---------------------------------------------------------

  gofTest(dat, test = "proucl.ks.gamma", distribution = "gamma")

  #Results of Goodness-of-Fit Test
  #-------------------------------
  #
  #Test Method:                     ProUCL Kolmogorov-Smirnov Gamma GOF
  #
  #Hypothesized Distribution:       Gamma
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
  #Test Statistic:                  D = 0.0988692
  #
  #Test Statistic Parameter:        n = 20
  #
  #Critical Values:                 D.0.01 = 0.228
  #                                 D.0.05 = 0.196
  #                                 D.0.10 = 0.180
  #
  #P-value:                         >= 0.10
  #
  #Alternative Hypothesis:          True cdf does not equal the
  #                                 Gamma Distribution.

  #----------

  # Chi-squared goodness-of-fit test (estimated parameters)
  #--------------------------------------------------------

  gofTest(dat, test = "chisq", distribution = "gamma", n.classes = 4)

  #Results of Goodness-of-Fit Test
  #-------------------------------
  #
  #Test Method:                     Chi-square GOF
  #
  #Hypothesized Distribution:       Gamma
  #
  #Estimated Parameter(s):          shape = 1.909462
  #                                 scale = 4.056819
  #
  #Estimation Method:               mle
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Test Statistic:                  Chi-square = 1.2
  #
  #Test Statistic Parameter:        df = 1
  #
  #P-value:                         0.2733217
  #
  #Alternative Hypothesis:          True cdf does not equal the
  #                                 Gamma Distribution.

  #----------
  # Clean up

  rm(dat, gof.list)
  graphics.off()

  #--------------------------------------------------------------------

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

  # Test for a normal distribution:
  #--------------------------------

  gof.list <- gofTest(Nickel.ppb ~ 1, data = EPA.09.Ex.10.1.nickel.df)
  gof.list

  #Results of Goodness-of-Fit Test
  #-------------------------------
  #
  #Test Method:                     Shapiro-Wilk GOF
  #
  #Hypothesized Distribution:       Normal
  #
  #Estimated Parameter(s):          mean = 169.5250
  #                                 sd   = 259.7175
  #
  #Estimation Method:               mvue
  #
  #Data:                            Nickel.ppb
  #
  #Data Source:                     EPA.09.Ex.10.1.nickel.df
  #
  #Sample Size:                     20
  #
  #Test Statistic:                  W = 0.6788888
  #
  #Test Statistic Parameter:        n = 20
  #
  #P-value:                         2.17927e-05
  #
  #Alternative Hypothesis:          True cdf does not equal the
  #                                 Normal Distribution.

  dev.new()
  plot(gof.list)

  #----------

  # Test for a lognormal distribution:
  #-----------------------------------

  gofTest(Nickel.ppb ~ 1, data = EPA.09.Ex.10.1.nickel.df,
    dist = "lnorm")

  #Results of Goodness-of-Fit Test
  #-------------------------------
  #
  #Test Method:                     Shapiro-Wilk GOF
  #
  #Hypothesized Distribution:       Lognormal
  #
  #Estimated Parameter(s):          meanlog = 3.918529
  #                                 sdlog   = 1.801404
  #
  #Estimation Method:               mvue
  #
  #Data:                            Nickel.ppb
  #
  #Data Source:                     EPA.09.Ex.10.1.nickel.df
  #
  #Sample Size:                     20
  #
  #Test Statistic:                  W = 0.978946
  #
  #Test Statistic Parameter:        n = 20
  #
  #P-value:                         0.9197735
  #
  #Alternative Hypothesis:          True cdf does not equal the
  #                                 Lognormal Distribution.

  #----------

  # Test for a lognormal distribution, but use the
  # Mean and CV parameterization:
  #-----------------------------------------------

  gofTest(Nickel.ppb ~ 1, data = EPA.09.Ex.10.1.nickel.df,
    dist = "lnormAlt")

  #Results of Goodness-of-Fit Test
  #-------------------------------
  #
  #Test Method:                     Shapiro-Wilk GOF
  #
  #Hypothesized Distribution:       Lognormal
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
  #Test Statistic:                  W = 0.978946
  #
  #Test Statistic Parameter:        n = 20
  #
  #P-value:                         0.9197735
  #
  #Alternative Hypothesis:          True cdf does not equal the
  #                                 Lognormal Distribution.

  #----------
  # Clean up

  rm(gof.list)
  graphics.off()

  #---------------------------------------------------------------------------

  # Generate 20 observations from a normal distribution with mean=3 and sd=2, and
  # generate 10 observaions from a normal distribution with mean=2 and sd=2 then
  # test whether these sets of observations come from the same distribution.
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(300)
  dat1 <- rnorm(20, mean = 3, sd = 2)
  dat2 <- rnorm(10, mean = 1, sd = 2)
  gofTest(x = dat1, y = dat2, test = "ks")

  #Results of Goodness-of-Fit Test
  #-------------------------------
  #
  #Test Method:                     2-Sample K-S GOF
  #
  #Hypothesized Distribution:       Equal
  #
  #Data:                            x = dat1
  #                                 y = dat2
  #
  #Sample Sizes:                    n.x = 20
  #                                 n.y = 10
  #
  #Test Statistic:                  ks = 0.7
  #
  #Test Statistic Parameters:       n = 20
  #                                 m = 10
  #
  #P-value:                         0.001669561
  #
  #Alternative Hypothesis:          The cdf of 'dat1' does not equal
  #                                 the cdf of 'dat2'.

  #----------
  # Clean up

  rm(dat1, dat2)
}
\keyword{ htest }
\keyword{ models }

\name{distChooseCensored}
\alias{distChooseCensored}
\title{
  Choose Best Fitting Distribution Based on Goodness-of-Fit Tests for Censored Data
}
\description{
  Perform a series of goodness-of-fit tests for censored data
  from a (possibly user-specified) set of candidate probability
  distributions to determine which probability distribution
  provides the best fit for a data set.
}
\usage{
distChooseCensored(x, censored, censoring.side = "left", alpha = 0.05,
    method = "sf", choices = c("norm", "gamma", "lnorm"),
    est.arg.list = NULL, prob.method = "hirsch-stedinger",
    plot.pos.con = 0.375, warn = TRUE, keep.data = TRUE,
    data.name = NULL, censoring.name = NULL)
}
\arguments{
  \item{x}{
  a numeric vector containing data for the goodness-of-fit tests.
  Missing (\code{NA}), undefined (\code{NaN}), and
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be
  removed.
}
  \item{censored}{
  numeric or logical vector indicating which values of \code{x} are censored.
  This must be the same length as \code{x}.  If the mode of \code{censored} is
  \code{"logical"}, \code{TRUE} values correspond to elements of \code{x} that
  are censored, and \code{FALSE} values correspond to elements of \code{x} that
  are not censored.  If the mode of \code{censored} is \code{"numeric"},
  it must contain only \code{1}'s and \code{0}'s; \code{1} corresponds to
  \code{TRUE} and \code{0} corresponds to \code{FALSE}.  Missing (\code{NA})
  values are allowed but will be removed.
}
  \item{censoring.side}{
  character string indicating on which side the censoring occurs.  The possible
  values are \code{"left"} (the default) and \code{"right"}.
}
  \item{alpha}{
  numeric scalar between 0 and 1 specifying the Type I error associated with each
  goodness-of-fit test.  When \code{method="proucl"} the only allowed values for
  \code{alpha} are 0.01, 0.05, and 0.1.  The default value is \code{alpha=0.05}.
  }
  \item{method}{
  character string defining which method to use.  Possible values are:
  \itemize{
    \item \code{"sw"}. Shapiro-Wilk.
    \item \code{"sf"}. Shapiro-Francia; the default.
    \item \code{"ppcc"}. Probability Plot Correlation Coefficient.
    \item \code{"proucl"}.  ProUCL.
  }

  The Shapiro-Wilk method is only available for singly censored data.

  See the DETAILS section for more information.
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
  \code{choices=c("norm", "gammaAlt", "lnormAlt")}, setting \cr
  \code{est.arg.list=list(lnormAlt=list(method="bcmle"))} indicates
  using the bias-corrected maximum-likelihood estimators (see the help file for
  \code{\link{elnormAltCensored}}).
  See the section \bold{Estimating Distribution Parameters} in the help file
  \link[=FcnsByCatCensoredData]{EnvStats Functions for Censored Data} for a list of
  available estimating functions for censored data.
  The default value is \code{est.arg.list=NULL} so that all default values for the
  estimating functions are used.

  In the course of testing for some form of normality (i.e., \link{Normal}, \link{Lognormal}),
  the estimated parameters are saved in the \code{test.results}
  component of the returned object, but the choice of the method of estimation
  has no effect on the goodness-of-fit test statistic or p-value.

  This argument is ignored when \code{method="proucl"}.
}
  \item{prob.method}{
  character string indicating what method to use to compute the plotting positions
  (empirical probabilities) when \code{test="sf"} or \code{test="ppcc"}.
  Possible values are:
  \itemize{
  \item \code{"modified kaplan-meier"} (modification of product-limit method of
    Kaplan and Meier (1958))
  \item \code{"nelson"} (hazard plotting method of Nelson (1972))
  \item \code{"michael-schucany"} (generalization of the product-limit method due to
    Michael and Schucany (1986))
  \item \code{"hirsch-stedinger"} (generalization of the product-limit method due to
    Hirsch and Stedinger (1987))
  }
  The default value is \code{prob.method="hirsch-stedinger"}.

  The \code{"nelson"} method is only available for \code{censoring.side="right"}, and
  the \code{"modified kaplan-meier"} method is only available for
  \code{censoring.side="left"}.  See the help files for
  \code{\link{gofTestCensored}} and \code{\link{ppointsCensored}} for more information.
}
  \item{plot.pos.con}{
  numeric scalar between 0 and 1 containing the value of the plotting position
  constant to use when \code{test="sf"} or \code{test="ppcc"}.  The default value is \cr
  \code{plot.pos.con=0.375}.  See the help files for \code{\link{gofTestCensored}} and
  \code{\link{ppointsCensored}} for more information.
}
  \item{warn}{
  logical scalar indicating whether to print a warning message when
  observations with \code{NA}s, \code{NaN}s, or \code{Inf}s in
  \code{y} are removed.  The default value is \code{warn=TRUE}.
}
  \item{keep.data}{
  logical scalar indicating whether to return the original data.  The
  default value is \code{keep.data=TRUE}.
}
  \item{data.name}{
  optional character string indicating the name of the data used for argument \code{x}.
}
  \item{censoring.name}{
  optional character string indicating the name for the data used for argument \code{censored}.
}
}
\details{
  The function \code{distChooseCensored} returns a list with information on the goodness-of-fit
  tests for various distributions and which distribution appears to best fit the
  data based on the p-values from the goodness-of-fit tests.  This function was written in
  order to compare ProUCL's way of choosing the best-fitting distribution (USEPA, 2015) with
  other ways of choosing the best-fitting distribution.

  \bold{Method Based on Shapiro-Wilk, Shapiro-Francia, or Probability Plot Correlation Test} \cr
  (\code{method="sw"}, \code{method="sf"}, or \code{method="ppcc"})

  For each value of the argument \code{choices}, the function \code{distChooseCensored}
  runs the goodness-of-fit test using the data in \code{x} assuming that particular
  distribution.  For example, if \cr
  \code{choices=c("norm", "gamma", "lnorm")},
  indicating the \link{Normal}, \link{Gamma}, and \link{Lognormal} distributions, and
  \code{method="sf"}, then the usual Shapiro-Francia test is performed for the \link{Normal}
  and \link{Lognormal} distributions, and the extension of the Shapiro-Francia test is performed
  for the \link{Gamma} distribution (see the section
  \emph{Testing Goodness-of-Fit for Any Continuous Distribution} in the help
  file for \code{\link{gofTestCensored}} for an explanation of the latter).  The distribution associated
  with the largest p-value is the chosen distribution.  In the case when all p-values are
  less than the value of the argument \code{alpha}, the distribution \dQuote{Nonparametric} is chosen.  \cr

  \bold{Method Based on ProUCL Algorithm} (\code{method="proucl"})

  When \code{method="proucl"}, the function \code{distChooseCensored} uses the
  algorithm that ProUCL (USEPA, 2015) uses to determine the best fitting
  distribution.  The candidate distributions are the
  \link{Normal}, \link{Gamma}, and \link{Lognormal} distributions.  The algorithm
  used by ProUCL is as follows:

  \enumerate{
  \item Remove all censored observations and use only the uncensored observations.

  \item Perform the Shapiro-Wilk and Lilliefors goodness-of-fit tests for the \link{Normal} distribution,
        i.e., call the function \code{\link{gofTest}} with \code{distribution="norm", test="sw"} and \cr
        \code{distribution = "norm", test="lillie"}.
        If either or both of the associated p-values are greater than or equal to the user-supplied value
        of \code{alpha}, then choose the \link{Normal} distribution.  Otherwise, proceed to the next step.

  \item Perform the \dQuote{ProUCL Anderson-Darling} and
        \dQuote{ProUCL Kolmogorov-Smirnov} goodness-of-fit tests for the \link{Gamma} distribution,
        i.e., call the function \code{\link{gofTest}} with \cr
        \code{distribution="gamma", test="proucl.ad.gamma"} and \cr
        \code{distribution="gamma", test="proucl.ks.gamma"}.
        If either or both of the associated p-values are greater than or equal to the user-supplied value
        of \code{alpha}, then choose the \link{Gamma} distribution.  Otherwise, proceed to the next step.

  \item Perform the Shapiro-Wilk and Lilliefors goodness-of-fit tests for the
        \link{Lognormal} distribution, i.e., call the function \code{\link{gofTest}}
        with \code{distribution = "lnorm", test="sw"} and \cr
        \code{distribution = "lnorm", test="lillie"}.
        If either or both of the associated p-values are greater than or equal to the user-supplied value
        of \code{alpha}, then choose the \link{Lognormal} distribution.  Otherwise, proceed to the next step.

  \item If none of the goodness-of-fit tests above yields a p-value greater than or equal to the user-supplied value
        of \code{alpha}, then choose the \dQuote{Nonparametric} distribution.
  }
}
\value{
  a list of class \code{"distChooseCensored"} containing the results of the goodness-of-fit tests.
  Objects of class \code{"distChooseCensored"} have a special printing method.
  See the help file for \cr
  \code{\link{distChooseCensored.object}} for details.
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
  quantile-quantile (Q-Q) plots (see \code{\link{qqPlotCensored}}).
}
\seealso{
  \code{\link{gofTestCensored}}, \code{\link{distChooseCensored.object}},
  \code{\link{print.distChooseCensored}}, \code{\link{distChoose}}.
}
\examples{
  # Generate 30 observations from a gamma distribution with
  # parameters mean=10 and cv=1 and then censor observations less than 5.
  # Then:
  #
  # 1) Call distChooseCensored using the Shapiro-Wilk method and specify
  #    choices of the
  #      normal,
  #      gamma (alternative parameterzation), and
  #      lognormal (alternative parameterization)
  #    distributions.
  #
  # 2) Compare the results in 1) above with the results using the
  #    ProUCL method.
  #
  # Notes:  The call to set.seed lets you reproduce this example.
  #
  #         The ProUCL method chooses the Normal distribution, whereas the
  #         Shapiro-Wilk method chooses the Gamma distribution.

  set.seed(598)

  dat <- sort(rgammaAlt(30, mean = 10, cv = 1))
  dat
  # [1]  0.5313509  1.4741833  1.9936208  2.7980636  3.4509840
  # [6]  3.7987348  4.5542952  5.5207531  5.5253596  5.7177872
  #[11]  5.7513827  9.1086375  9.8444090 10.6247123 10.9304922
  #[16] 11.7925398 13.3432689 13.9562777 14.6029065 15.0563342
  #[21] 15.8730642 16.0039936 16.6910715 17.0288922 17.8507891
  #[26] 19.1105522 20.2657141 26.3815970 30.2912797 42.8726101

  dat.censored <- dat
  censored <- dat.censored < 5
  dat.censored[censored] <- 5


  # 1) Call distChooseCensored using the Shapiro-Wilk method.
  #----------------------------------------------------------

  distChooseCensored(dat.censored, censored, method = "sw",
    choices = c("norm", "gammaAlt", "lnormAlt"))

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
  #Estimated Parameter(s):          mean = 12.4911448
  #                                 cv   =  0.7617343
  #
  #Estimation Method:               MLE
  #
  #Data:                            dat.censored
  #
  #Sample Size:                     30
  #
  #Censoring Side:                  left
  #
  #Censoring Variable:              censored
  #
  #Censoring Level(s):              5
  #
  #Percent Censored:                23.33333%
  #
  #Test Results:
  #
  #  Normal
  #    Test Statistic:              W = 0.9372741
  #    P-value:                     0.1704876
  #
  #  Gamma
  #    Test Statistic:              W = 0.9613711
  #    P-value:                     0.522329
  #
  #  Lognormal
  #    Test Statistic:              W = 0.9292406
  #    P-value:                     0.114511

  #--------------------

  # 2) Compare the results in 1) above with the results using the
  #    ProUCL method.
  #---------------------------------------------------------------

  distChooseCensored(dat.censored, censored, method = "proucl")

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
  #Estimated Parameter(s):          mean = 15.397584
  #                                 sd   =  8.688302
  #
  #Estimation Method:               mvue
  #
  #Data:                            dat.censored
  #
  #Sample Size:                     30
  #
  #Censoring Side:                  left
  #
  #Censoring Variable:              censored
  #
  #Censoring Level(s):              5
  #
  #Percent Censored:                23.33333%
  #
  #ProUCL Sample Size:              23
  #
  #Test Results:
  #
  #  Normal
  #    Shapiro-Wilk GOF
  #      Test Statistic:            W = 0.861652
  #      P-value:                   0.004457924
  #    Lilliefors (Kolmogorov-Smirnov) GOF
  #      Test Statistic:            D = 0.1714435
  #      P-value:                   0.07794315
  #
  #  Gamma
  #    ProUCL Anderson-Darling Gamma GOF
  #      Test Statistic:            A = 0.3805556
  #      P-value:                   >= 0.10
  #    ProUCL Kolmogorov-Smirnov Gamma GOF
  #      Test Statistic:            D = 0.1035271
  #      P-value:                   >= 0.10
  #
  #  Lognormal
  #    Shapiro-Wilk GOF
  #      Test Statistic:            W = 0.9532604
  #      P-value:                   0.3414187
  #    Lilliefors (Kolmogorov-Smirnov) GOF
  #      Test Statistic:            D = 0.115588
  #      P-value:                   0.5899259

  #--------------------

  # Clean up
  #---------

  rm(dat, censored, dat.censored)

  #====================================================================

  # Check the assumption that the silver data stored in Helsel.Cohn.88.silver.df
  # follows a lognormal distribution.
  # Note that the small p-value and the shape of the Q-Q plot
  # (an inverted S-shape) suggests that the log transformation is not quite strong
  # enough to "bring in" the tails (i.e., the log-transformed silver data has tails
  # that are slightly too long relative to a normal distribution).
  # Helsel and Cohn (1988, p.2002) note that the gross outlier of 560 mg/L tends to
  # make the shape of the data resemble a gamma distribution, but
  # the distChooseCensored function decision is neither Gamma nor Lognormal,
  # but instead Nonparametric.

  # First create a lognormal Q-Q plot
  #----------------------------------

  dev.new()
  with(Helsel.Cohn.88.silver.df,
    qqPlotCensored(Ag, Censored, distribution = "lnorm",
      points.col = "blue", add.line = TRUE))

  #----------

  # Now call the distChooseCensored function using the default settings.
  #---------------------------------------------------------------------

  with(Helsel.Cohn.88.silver.df,
    distChooseCensored(Ag, Censored))

  #Results of Choosing Distribution
  #--------------------------------
  #
  #Candidate Distributions:         Normal
  #                                 Gamma
  #                                 Lognormal
  #
  #Choice Method:                   Shapiro-Francia
  #
  #Type I Error per Test:           0.05
  #
  #Decision:                        Nonparametric
  #
  #Data:                            Ag
  #
  #Sample Size:                     56
  #
  #Censoring Side:                  left
  #
  #Censoring Variable:              Censored
  #
  #Censoring Level(s):               0.1  0.2  0.3  0.5  1.0  2.0  2.5  5.0  6.0 10.0 20.0 25.0
  #
  #Percent Censored:                60.71429%
  #
  #Test Results:
  #
  #  Normal
  #    Test Statistic:              W = 0.3065529
  #    P-value:                     8.346126e-08
  #
  #  Gamma
  #    Test Statistic:              W = 0.6254148
  #    P-value:                     1.884155e-05
  #
  #  Lognormal
  #    Test Statistic:              W = 0.8957198
  #    P-value:                     0.03490314

  #----------

  # Clean up
  #---------

  graphics.off()

  #====================================================================

  # Chapter 15 of USEPA (2009) gives several examples of looking
  # at normal Q-Q plots and estimating the mean and standard deviation
  # for manganese concentrations (ppb) in groundwater at five background
  # wells (USEPA, 2009, p. 15-10).  The Q-Q plot shown in Figure 15-4
  # on page 15-13 clearly indicates that the Lognormal distribution
  # is a good fit for these data.
  # In EnvStats these data are stored in the data frame EPA.09.Ex.15.1.manganese.df.

  # Here we will call the distChooseCensored function to determine
  # whether the data appear to come from a normal, gamma, or lognormal
  # distribution.
  #
  # Note that using the Probability Plot Correlation Coefficient method
  # (equivalent to using the Shapiro-Francia method) yields a decision of
  # Lognormal, but using the ProUCL method yields a decision of Gamma.
  #----------------------------------------------------------------------


  # First look at the data:
  #-----------------------

  EPA.09.Ex.15.1.manganese.df

  #   Sample   Well Manganese.Orig.ppb Manganese.ppb Censored
  #1       1 Well.1                 <5           5.0     TRUE
  #2       2 Well.1               12.1          12.1    FALSE
  #3       3 Well.1               16.9          16.9    FALSE
  #...
  #23      3 Well.5                3.3           3.3    FALSE
  #24      4 Well.5                8.4           8.4    FALSE
  #25      5 Well.5                 <2           2.0     TRUE

  longToWide(EPA.09.Ex.15.1.manganese.df,
    "Manganese.Orig.ppb", "Sample", "Well",
    paste.row.name = TRUE)

  #         Well.1 Well.2 Well.3 Well.4 Well.5
  #Sample.1     <5     <5     <5    6.3   17.9
  #Sample.2   12.1    7.7    5.3   11.9   22.7
  #Sample.3   16.9   53.6   12.6     10    3.3
  #Sample.4   21.6    9.5  106.3     <2    8.4
  #Sample.5     <2   45.9   34.5   77.2     <2


  # Use distChooseCensored with the probability plot correlation method,
  # and for the gamma and lognormal distribution specify the
  # mean and CV parameterization:
  #------------------------------------------------------------

  with(EPA.09.Ex.15.1.manganese.df,
    distChooseCensored(Manganese.ppb, Censored,
      choices = c("norm", "gamma", "lnormAlt"), method = "ppcc"))

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
  #Estimated Parameter(s):          mean = 23.003987
  #                                 cv   =  2.300772
  #
  #Estimation Method:               MLE
  #
  #Data:                            Manganese.ppb
  #
  #Sample Size:                     25
  #
  #Censoring Side:                  left
  #
  #Censoring Variable:              Censored
  #
  #Censoring Level(s):              2 5
  #
  #Percent Censored:                24%
  #
  #Test Results:
  #
  #  Normal
  #    Test Statistic:              r = 0.9147686
  #    P-value:                     0.004662658
  #
  #  Gamma
  #    Test Statistic:              r = 0.9844875
  #    P-value:                     0.6836625
  #
  #  Lognormal
  #    Test Statistic:              r = 0.9931982
  #    P-value:                     0.9767731

  #--------------------

  # Repeat the above example using the ProUCL method.
  #--------------------------------------------------

  with(EPA.09.Ex.15.1.manganese.df,
    distChooseCensored(Manganese.ppb, Censored, method = "proucl"))

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
  #Estimated Parameter(s):          shape =  1.284882
  #                                 scale = 19.813413
  #
  #Estimation Method:               MLE
  #
  #Data:                            Manganese.ppb
  #
  #Sample Size:                     25
  #
  #Censoring Side:                  left
  #
  #Censoring Variable:              Censored
  #
  #Censoring Level(s):              2 5
  #
  #Percent Censored:                24%
  #
  #ProUCL Sample Size:              19
  #
  #Test Results:
  #
  #  Normal
  #    Shapiro-Wilk GOF
  #      Test Statistic:            W = 0.7423947
  #      P-value:                   0.0001862975
  #    Lilliefors (Kolmogorov-Smirnov) GOF
  #      Test Statistic:            D = 0.2768771
  #      P-value:                   0.0004771155
  #
  #  Gamma
  #    ProUCL Anderson-Darling Gamma GOF
  #      Test Statistic:            A = 0.6857121
  #      P-value:                   0.05 <= p < 0.10
  #    ProUCL Kolmogorov-Smirnov Gamma GOF
  #      Test Statistic:            D = 0.1830034
  #      P-value:                   >= 0.10
  #
  #  Lognormal
  #    Shapiro-Wilk GOF
  #      Test Statistic:            W = 0.969805
  #      P-value:                   0.7725528
  #    Lilliefors (Kolmogorov-Smirnov) GOF
  #      Test Statistic:            D = 0.138547
  #      P-value:                   0.4385195

  #====================================================================

  \dontrun{
  # 1) Simulate 1000 trials where for each trial you:
  #    a) Generate 30 observations from a Gamma distribution with
  #       parameters mean = 10 and CV = 1.
  #    b) Censor observations less than 5 (the 39th percentile).
  #    c) Use distChooseCensored with the Shapiro-Francia method.
  #    d) Use distChooseCensored with the ProUCL method.
  #
  #  2) Compare the proportion of times the
  #     Normal vs. Gamma vs. Lognormal vs. Nonparametric distribution
  #     is chosen for c) and d) above.
  #------------------------------------------------------------------

  set.seed(58)
  N <- 1000

  Choose.fac <- factor(rep("", N), levels = c("Normal", "Gamma", "Lognormal", "Nonparametric"))
  Choose.df <- data.frame(SW = Choose.fac, ProUCL = Choose.fac)

  for(i in 1:N) {
    dat <- rgammaAlt(30, mean = 10, cv = 1)
    censored <- dat < 5
    dat[censored] <- 5
    Choose.df[i, "SW"]     <- distChooseCensored(dat, censored, method = "sw")$decision
    Choose.df[i, "ProUCL"] <- distChooseCensored(dat, censored, method = "proucl")$decision
  }

  summaryStats(Choose.df, digits = 0)

  #                ProUCL(N) ProUCL(Pct) SW(N) SW(Pct)
  #Normal              520          52   398      40
  #Gamma               336          34   375      38
  #Lognormal           105          10   221      22
  #Nonparametric        39           4     6       1
  #Combined           1000         100  1000     100

  #--------------------


  # Repeat above example for the Lognormal Distribution with mean=10 and CV = 1.
  # In this case, 5 is the 34th percentile.
  #-----------------------------------------------------------------------------

  set.seed(297)
  N <- 1000

  Choose.fac <- factor(rep("", N), levels = c("Normal", "Gamma", "Lognormal", "Nonparametric"))
  Choose.df <- data.frame(SW = Choose.fac, ProUCL = Choose.fac)

  for(i in 1:N) {
    dat <- rlnormAlt(30, mean = 10, cv = 1)
    censored <- dat < 5
    dat[censored] <- 5
    Choose.df[i, "SW"]     <- distChooseCensored(dat, censored, method = "sf")$decision
    Choose.df[i, "ProUCL"] <- distChooseCensored(dat, censored, method = "proucl")$decision
  }

  summaryStats(Choose.df, digits = 0)

  #              ProUCL(N) ProUCL(Pct) SW(N) SW(Pct)
  #Normal              277          28    92       9
  #Gamma               393          39   231      23
  #Lognormal           190          19   624      62
  #Nonparametric       140          14    53       5
  #Combined           1000         100  1000     100

  #--------------------


  # Clean up
  #---------

  rm(N, Choose.fac, Choose.df, i, dat, censored)
  }

}
\keyword{ htest }
\keyword{ models }

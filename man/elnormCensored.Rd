\name{elnormCensored}
\alias{elnormCensored}
\title{
  Estimate Parameters for a Lognormal Distribution (Log-Scale) Based on Type I Censored Data
}
\description{
  Estimate the mean and standard deviation parameters of the logarithm of a
  \link[stats:Lognormal]{lognormal distribution} given a
  sample of data that has been subjected to Type I censoring,
  and optionally construct a confidence interval for the mean.
}
\usage{
  elnormCensored(x, censored, method = "mle", censoring.side = "left",
    ci = FALSE, ci.method = "profile.likelihood", ci.type = "two-sided",
    conf.level = 0.95, n.bootstraps = 1000, pivot.statistic = "z",
    nmc = 1000, seed = NULL, ...)
}
\arguments{
  \item{x}{
  numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), and
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
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
  \item{method}{
  character string specifying the method of estimation.

  For singly censored data, the possible values are:  \cr
  \code{"mle"} (maximum likelihood; the default), \cr
  \code{"bcmle"} (bias-corrected maximum likelihood), \cr
  \code{"ROS"} or \code{"qq.reg"} (quantile-quantile regression; also called
    \emph{regression on order statistics} and abbreviated ROS), \cr
  \code{"qq.reg.w.cen.level"} (quantile-quantile regression including the
    censoring level), \cr
  \code{"rROS"} or \code{"impute.w.qq.reg"} (moment estimation based on imputation using
    quantile-quantile regression; also called \emph{robust regression on order statistics}
    and abbreviated rROS), \cr
  \code{"impute.w.qq.reg.w.cen.level"} (moment estimation based on imputation
    using the \code{qq.reg.w.cen.level} method), \cr
  \code{"impute.w.mle"} (moment estimation based on imputation using the mle), \cr
  \code{"iterative.impute.w.qq.reg"} (moment estimation based on iterative
    imputation using the \code{qq.reg} method), \cr
  \code{"m.est"} (robust M-estimation), and \cr
  \code{"half.cen.level"} (moment estimation based on setting the censored
    observations to half the censoring level).

  For multiply censored data, the possible values are:  \cr
  \code{"mle"} (maximum likelihood; the default), \cr
  \code{"ROS"} or \code{"qq.reg"} (quantile-quantile regression; also called
    \emph{regression on order statistics} and abbreviated ROS), \cr
  \code{"rROS"} or \code{"impute.w.qq.reg"} (moment estimation based on imputation using
    quantile-quantile regression; also called \emph{robust regression on order statistics}
    and abbreviated rROS), and \cr
  \code{"half.cen.level"} (moment estimation based on setting the censored
    observations to half the censoring level).

  See the DETAILS section for more information.
}
  \item{censoring.side}{
  character string indicating on which side the censoring occurs.  The possible
  values are \code{"left"} (the default) and \code{"right"}.
}
  \item{ci}{
  logical scalar indicating whether to compute a confidence interval for the
  mean or variance.  The default value is \code{ci=FALSE}.
}
  \item{ci.method}{
  character string indicating what method to use to construct the confidence interval
  for the mean.  The possible values are: \cr
  \code{"profile.likelihood"} (profile likelihood; the default), \cr
  \code{"normal.approx"} (normal approximation), \cr
  \code{"normal.approx.w.cov"} (normal approximation taking into account the
  covariance between the estimated mean and standard deviation; only available for
  singly censored data), \cr
  \code{"gpq"} (generalized pivotal quantity), and \cr
  \code{"bootstrap"} (based on bootstrapping).

  See the DETAILS section for more information.
  This argument is ignored if \code{ci=FALSE}.
}
  \item{ci.type}{
  character string indicating what kind of confidence interval to compute.  The
  possible values are \code{"two-sided"} (the default), \code{"lower"}, and
  \code{"upper"}.  This argument is ignored if \code{ci=FALSE}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level of the confidence interval.
  The default value is \code{conf.level=0.95}. This argument is ignored if
  \code{ci=FALSE}.
}
  \item{n.bootstraps}{
  numeric scalar indicating how many bootstraps to use to construct the
  confidence interval for the mean when \code{ci.type="bootstrap"}.  This
  argument is ignored if \code{ci=FALSE} and/or \code{ci.method} does not
  equal \code{"bootstrap"}.
}
  \item{pivot.statistic}{
  character string indicating which pivot statistic to use in the construction
  of the confidence interval for the mean when \code{ci.method="normal.approx"} or
  \code{ci.method="normal.approx.w.cov"} (see the DETAILS section).  The possible
  values are \code{pivot.statistic="z"} (the default) and \code{pivot.statistic="t"}.
  When \code{pivot.statistic="t"} you may supply the argument
  \code{ci.sample size} (see below).  The argument \code{pivot.statistic} is
  ignored if \code{ci=FALSE}.
}
  \item{nmc}{
  numeric scalar indicating the number of Monte Carlo simulations to run when
  \code{ci.method="gpq"}.  The default is \code{nmc=1000}.  This argument is ignored if \cr
  \code{ci=FALSE}.
}
  \item{seed}{
  integer supplied to the function \code{\link[base]{set.seed}} and used when \cr
  \code{ci.method="bootstrap"} or \code{ci.method="gpq"}.  The default value is
  \code{seed=NULL}, in which case the current value of \code{.Random.seed} is used.
  This argument is ignored when \code{ci=FALSE}.
}
  \item{\dots}{
  additional arguments to pass to other functions.
  \itemize{
  \item \code{prob.method}.  Character string indicating what method to use to
    compute the plotting positions (empirical probabilities) when \code{method}
    is one of \code{"ROS"}, \code{"qq.reg"}, \code{"qq.reg.w.cen.level"},
    \code{"rROS"}, \code{"impute.w.qq.reg"},
    \code{"impute.w.qq.reg.w.cen.level"}, \code{"impute.w.mle"}, or \cr
    \code{"iterative.impute.w.qq.reg"}.  Possible values are: \cr
    \code{"kaplan-meier"} (product-limit method of Kaplan and Meier (1958)), \cr
    \code{"nelson"} (hazard plotting method of Nelson (1972)), \cr
    \code{"michael-schucany"} (generalization of the product-limit method due to Michael and Schucany (1986)), and \cr
    \code{"hirsch-stedinger"} (generalization of the product-limit method due to Hirsch and Stedinger (1987)). \cr
    The default value is \code{prob.method="hirsch-stedinger"}.  The \code{"nelson"}
    method is only available for \code{censoring.side="right"}.
    See the DETAILS section and the help file for \code{\link{ppointsCensored}}
    for more information.

  \item \code{plot.pos.con}.  Numeric scalar between 0 and 1 containing the
    value of the plotting position constant to use when \code{method} is one of
    \code{"qq.reg"}, \code{"qq.reg.w.cen.level"}, \code{"impute.w.qq.reg"}, \cr
    \code{"impute.w.qq.reg.w.cen.level"}, \code{"impute.w.mle"}, or \cr
    \code{"iterative.impute.w.qq.reg"}.  The default value is \code{plot.pos.con=0.375}.
    See the DETAILS section and the help file for \code{\link{ppointsCensored}}
    for more information.

  \item \code{ci.sample.size}.  Numeric scalar indicating what sample size to
    assume to construct the confidence interval for the mean if
    \code{pivot.statistic="t"} and \code{ci.method="normal.approx"} or
    \code{ci.method="normal.approx.w.cov"}.  When \code{method} equals
    \code{"mle"} or \code{"bcmle"}, the default value is the expected number of
    uncensored observations, otherwise it is the observed number of
    uncensored observations.

  \item \code{lb.impute}.  Numeric scalar indicating the lower bound for imputed
    observations when method is one of \code{"impute.w.qq.reg"}, \cr
    \code{"impute.w.qq.reg.w.cen.level"}, \code{"impute.w.mle"}, or \cr
    \code{"iterative.impute.w.qq.reg"}.  Imputed values smaller than this
    value will be set to this value.  The default is \code{lb.impute=-Inf}.

  \item \code{ub.impute}.  Numeric scalar indicating the upper bound for imputed
    observations when method is one of \code{"impute.w.qq.reg"}, \cr
    \code{"impute.w.qq.reg.w.cen.level"}, \code{"impute.w.mle"}, or \cr
    \code{"iterative.impute.w.qq.reg"}.  Imputed values larger than this value
    will be set to this value.  The default is \code{ub.impute=Inf}.

  \item \code{convergence}.  Character string indicating the kind of convergence
    criterion when \code{method="iterative.impute.w.qq.reg"}.  The possible values
    are \code{"relative"} (the default) and \code{"absolute"}.  See the DETAILS
    section for more information.

  \item \code{tol}.  Numeric scalar indicating the convergence tolerance when \cr
    \code{method="iterative.impute.w.qq.reg"}.  The default value is \code{tol=1e-6}.
    If \code{convergence="relative"}, then the relative difference in the old and
    new estimates of the mean and the relative difference in the old and new estimates
    of the standard deviation must be less than \code{tol} for convergence to be
    achieved.  If \code{convergence="absolute"}, then the absolute difference in the
    old and new estimates of the mean and the absolute difference in the old and new
    estimates of the standard deviation must be less than \code{tol} for convergence
    to be achieved.

  \item \code{max.iter}.  Numeric scalar indicating the maximum number of iterations
    when \code{method="iterative.impute.w.qq.reg"}.

  \item \code{t.df}.  Numeric scalar greater than or equal to 1 that determines the
    robustness and efficiency properties of the estimator when \code{method="m.est"}.
    The default value is \code{t.df=3}.
  }
}
}
\details{
  If \code{x} or \code{censored} contain any missing (\code{NA}), undefined (\code{NaN}) or
  infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to
  performing the estimation.

  Let \eqn{X} denote a random variable with a
  \link[stats:Lognormal]{lognormal distribution} with
  parameters \code{meanlog=}\eqn{\mu} and \code{sdlog=}\eqn{\sigma}.  Then
  \eqn{Y = log(X)} has a \link[stats:Normal]{normal (Gaussian) distribution} with
  parameters \code{mean=}\eqn{\mu} and \code{sd=}\eqn{\sigma}.  Thus, the function
  \code{elnormCensored} simply calls the function \code{\link{enormCensored}} using the
  log-transformed values of \code{x}.
}
\value{
  a list of class \code{"estimateCensored"} containing the estimated parameters
  and other information.  See \code{\link{estimateCensored.object}} for details.
}
\references{
  Bain, L.J., and M. Engelhardt. (1991).  \emph{Statistical Analysis of
  Reliability and Life-Testing Models}.  Marcel Dekker, New York, 496pp.

  Cohen, A.C. (1959).  Simplified Estimators for the Normal Distribution When
  Samples are Singly Censored or Truncated.  \emph{Technometrics} \bold{1}(3), 217--237.

  Cohen, A.C. (1963).  Progressively Censored Samples in Life Testing.
  \emph{Technometrics} \bold{5}, 327--339

  Cohen, A.C. (1991).  \emph{Truncated and Censored Samples}.  Marcel Dekker,
  New York, New York, 312pp.

  Cox, D.R. (1970).  \emph{Analysis of Binary Data}.  Chapman & Hall, London.  142pp.

  Efron, B. (1979).  Bootstrap Methods: Another Look at the Jackknife.
  \emph{The Annals of Statistics} \bold{7}, 1--26.

  Efron, B., and R.J. Tibshirani. (1993).  \emph{An Introduction to the Bootstrap}.
  Chapman and Hall, New York, 436pp.

  El-Shaarawi, A.H. (1989).  Inferences About the Mean from Censored Water Quality
  Data.  \emph{Water Resources Research} \bold{25}(4) 685--690.

  El-Shaarawi, A.H., and D.M. Dolan. (1989).  Maximum Likelihood Estimation of
  Water Quality Concentrations from Censored Data.
  \emph{Canadian Journal of Fisheries and Aquatic Sciences} \bold{46}, 1033--1039.

  El-Shaarawi, A.H., and S.R. Esterby. (1992).  Replacement of Censored Observations
  by a Constant: An Evaluation.  \emph{Water Research} \bold{26}(6), 835--844.

  El-Shaarawi, A.H., and A. Naderi. (1991).  Statistical Inference from Multiply
  Censored Environmental Data.
  \emph{Environmental Monitoring and Assessment} \bold{17}, 339--347.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009).
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.
  John Wiley & Sons, Hoboken.

  Gilliom, R.J., and D.R. Helsel. (1986).  Estimation of Distributional Parameters for
  Censored Trace Level Water Quality Data: 1. Estimation Techniques.
  \emph{Water Resources Research} \bold{22}, 135--146.

  Gleit, A. (1985).  Estimation for Small Normal Data Sets with Detection Limits.
  \emph{Environmental Science and Technology} \bold{19}, 1201--1206.

  Haas, C.N., and P.A. Scheff. (1990).  Estimation of Averages in Truncated Samples.
  \emph{Environmental Science and Technology} \bold{24}(6), 912--919.

  Hashimoto, L.K., and R.R. Trussell. (1983).  Evaluating Water Quality Data Near
  the Detection Limit.  Paper presented at the Advanced Technology Conference,
  American Water Works Association, Las Vegas, Nevada, June 5-9, 1983.

  Helsel, D.R. (1990).  Less than Obvious: Statistical Treatment of Data Below the
  Detection Limit.  \emph{Environmental Science and Technology} \bold{24}(12),
  1766--1774.

  Helsel, D.R. (2012). \emph{Statistics for Censored Environmental Data Using Minitab and R,
  Second Edition}.  John Wiley & Sons, Hoboken, New Jersey.

  Helsel, D.R., and T.A. Cohn. (1988).  Estimation of Descriptive Statistics for
  Multiply Censored Water Quality Data.  \emph{Water Resources Research}
  \bold{24}(12), 1997--2004.

  Hirsch, R.M., and J.R. Stedinger. (1987).  Plotting Positions for Historical
  Floods and Their Precision.  \emph{Water Resources Research} \bold{23}(4), 715--727.

  Korn, L.R., and D.E. Tyler. (2001).  Robust Estimation for Chemical Concentration
  Data Subject to Detection Limits.  In Fernholz, L., S. Morgenthaler, and W. Stahel,
  eds.  \emph{Statistics in Genetics and in the Environmental Sciences}.
  Birkhauser Verlag, Basel, pp.41--63.

  Krishnamoorthy K., and T. Mathew. (2009).
  \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}.
  John Wiley and Sons, Hoboken.

  Michael, J.R., and W.R. Schucany. (1986).  Analysis of Data from Censored Samples.
  In D'Agostino, R.B., and M.A. Stephens, eds. \emph{Goodness-of Fit Techniques}.
  Marcel Dekker, New York, 560pp, Chapter 11, 461--496.

  Millard, S.P., P. Dixon, and N.K. Neerchal. (2014; in preparation).
  \emph{Environmental Statistics with R}.  CRC Press, Boca Raton, Florida.

  Nelson, W. (1982).  \emph{Applied Life Data Analysis}.
  John Wiley and Sons, New York, 634pp.

  Newman, M.C., P.M. Dixon, B.B. Looney, and J.E. Pinder. (1989).  Estimating Mean
  and Variance for Environmental Samples with Below Detection Limit Observations.
  \emph{Water Resources Bulletin} \bold{25}(4), 905--916.

  Pettitt, A. N. (1983).  Re-Weighted Least Squares Estimation with Censored and
  Grouped Data: An Application of the EM Algorithm.
  \emph{Journal of the Royal Statistical Society, Series B} \bold{47}, 253--260.

  Regal, R. (1982).  Applying Order Statistic Censored Normal Confidence Intervals
  to Time Censored Data.  Unpublished manuscript, University of Minnesota, Duluth,
  Department of Mathematical Sciences.

  Royston, P. (2007).  Profile Likelihood for Estimation and Confdence Intervals.
  \emph{The Stata Journal} \bold{7}(3), pp. 376--387.

  Saw, J.G. (1961b).  The Bias of the Maximum Likelihood Estimators of Location and
  Scale Parameters Given a Type II Censored Normal Sample.
  \emph{Biometrika} \bold{48}, 448--451.

  Schmee, J., D.Gladstein, and W. Nelson. (1985).  Confidence Limits for Parameters
  of a Normal Distribution from Singly Censored Samples, Using Maximum Likelihood.
  \emph{Technometrics} \bold{27}(2) 119--128.

  Schneider, H. (1986).  \emph{Truncated and Censored Samples from Normal Populations}.
  Marcel Dekker, New York, New York, 273pp.

  Shumway, R.H., A.S. Azari, and P. Johnson. (1989).  Estimating Mean Concentrations
  Under Transformations for Environmental Data With Detection Limits.
  \emph{Technometrics} \bold{31}(3), 347--356.

  Singh, A., R. Maichle, and S. Lee. (2006).  \emph{On the Computation of a 95\%
  Upper Confidence Limit of the Unknown Population Mean Based Upon Data Sets
  with Below Detection Limit Observations}.  EPA/600/R-06/022, March 2006.
  Office of Research and Development, U.S. Environmental Protection Agency,
  Washington, D.C.

  Stryhn, H., and J. Christensen. (2003).  \emph{Confidence Intervals by the Profile
  Likelihood Method, with Applications in Veterinary Epidemiology}.  Contributed paper
  at ISVEE X (November 2003, Chile).
  \url{https://gilvanguedes.com/wp-content/uploads/2019/05/Profile-Likelihood-CI.pdf}.

  Travis, C.C., and M.L. Land. (1990).  Estimating the Mean of Data Sets with
  Nondetectable Values.  \emph{Environmental Science and Technology} \bold{24}, 961--962.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
  U.S. Environmental Protection Agency, Washington, D.C. Chapter 15.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C.

  Venzon, D.J., and S.H. Moolgavkar. (1988).  A Method for Computing
  Profile-Likelihood-Based Confidence Intervals.  \emph{Journal of the Royal
  Statistical Society, Series C (Applied Statistics)} \bold{37}(1), pp. 87--94.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  A sample of data contains censored observations if some of the observations are
  reported only as being below or above some censoring level.  In environmental
  data analysis, Type I left-censored data sets are common, with values being
  reported as \dQuote{less than the detection limit} (e.g., Helsel, 2012).  Data
  sets with only one censoring level are called \emph{singly censored}; data sets with
  multiple censoring levels are called \emph{multiply} or \emph{progressively censored}.

  Statistical methods for dealing with censored data sets have a long history in the
  field of survival analysis and life testing.  More recently, researchers in the
  environmental field have proposed alternative methods of computing estimates and
  confidence intervals in addition to the classical ones such as maximum likelihood
  estimation.

  Helsel (2012, Chapter 6) gives an excellent review of past studies of the
  properties of various estimators based on censored environmental data.

  In practice, it is better to use a confidence interval for the mean or a
  joint confidence region for the mean and standard deviation, rather than rely on a
  single point-estimate of the mean.  Since confidence intervals and regions depend
  on the properties of the estimators for both the mean and standard deviation, the
  results of studies that simply evaluated the performance of the mean and standard
  deviation separately cannot be readily extrapolated to predict the performance of
  various methods of constructing confidence intervals and regions.  Furthermore,
  for several of the methods that have been proposed to estimate the mean based on
  type I left-censored data, standard errors of the estimates are not available,
  hence it is not possible to construct confidence intervals
  (El-Shaarawi and Dolan, 1989).

  Few studies have been done to evaluate the performance of methods for constructing
  confidence intervals for the mean or joint confidence regions for the mean and
  standard deviation when data are subjected to single or multiple censoring.
  See, for example, Singh et al. (2006).

  Schmee et al. (1985) studied Type II censoring for a normal distribution and
  noted that the bias and variances of the maximum likelihood estimators are of the
  order \eqn{1/N}, and that the bias is negligible for \eqn{N=100} and as much as
  90\% censoring.  (If the proportion of censored observations is less than 90\%,
  the bias becomes negligible for smaller sample sizes.)  For small samples with
  moderate to high censoring, however, the bias of the mle's causes confidence
  intervals based on them using a normal approximation (e.g., \code{method="mle"}
  and \code{ci.method="normal.approx"}) to be too short.  Schmee et al. (1985)
  provide tables for exact confidence intervals for sample sizes up to \eqn{N=100}
  that were created based on Monte Carlo simulation.  Schmee et al. (1985) state
  that these tables should work well for Type I censored data as well.

  Shumway et al. (1989) evaluated the coverage of 90\% confidence intervals for the
  mean based on using a Box-Cox transformation to induce normality, computing the
  mle's based on the normal distribution, then computing the mean in the original
  scale.  They considered three methods of constructing confidence intervals:  the
  delta method, the bootstrap, and the bias-corrected bootstrap.
  Shumway et al. (1989) used three parent distributions in their study:
  Normal(3,1), the square of this distribuiton, and the exponentiation of this
  distribution (i.e., a lognormal distribution).  Based on sample sizes of 10 and 50
  with a censoring level at the 10'th or 20'th percentile, Shumway et al. (1989)
  found that the delta method performed quite well and was superior to the
  bootstrap method.

  Millard et al. (2014; in preparation) show that the coverage of profile likelihood
  method is excellent.
}
\seealso{
  \code{\link{enormCensored}}, \link[stats]{Lognormal}, \code{\link{elnorm}},
  \code{\link{estimateCensored.object}}.
}
\examples{
  # Chapter 15 of USEPA (2009) gives several examples of estimating the mean
  # and standard deviation of a lognormal distribution on the log-scale using
  # manganese concentrations (ppb) in groundwater at five background wells.
  # In EnvStats these data are stored in the data frame
  # EPA.09.Ex.15.1.manganese.df.

  # Here we will estimate the mean and standard deviation using the MLE,
  # Q-Q regression (also called parametric regression on order statistics
  # or ROS; e.g., USEPA, 2009 and Helsel, 2012), and imputation with Q-Q
  # regression (also called robust ROS or rROS).

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


  # Now estimate the mean and standard deviation on the log-scale
  # using the MLE:
  #---------------------------------------------------------------

  with(EPA.09.Ex.15.1.manganese.df,
    elnormCensored(Manganese.ppb, Censored))

  #Results of Distribution Parameter Estimation
  #Based on Type I Censored Data
  #--------------------------------------------
  #
  #Assumed Distribution:            Lognormal
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              2 5
  #
  #Estimated Parameter(s):          meanlog = 2.215905
  #                                 sdlog   = 1.356291
  #
  #Estimation Method:               MLE
  #
  #Data:                            Manganese.ppb
  #
  #Censoring Variable:              Censored
  #
  #Sample Size:                     25
  #
  #Percent Censored:                24%

  # Now compare the MLE with the estimators based on
  # Q-Q regression (ROS) and imputation with Q-Q regression (rROS)
  #---------------------------------------------------------------

  with(EPA.09.Ex.15.1.manganese.df,
    elnormCensored(Manganese.ppb, Censored))$parameters
  # meanlog    sdlog
  #2.215905 1.356291

  with(EPA.09.Ex.15.1.manganese.df,
    elnormCensored(Manganese.ppb, Censored,
    method = "ROS"))$parameters
  # meanlog    sdlog
  #2.293742 1.283635

  with(EPA.09.Ex.15.1.manganese.df,
    elnormCensored(Manganese.ppb, Censored,
    method = "rROS"))$parameters
  # meanlog    sdlog
  #2.298656 1.238104

  #----------

  # The method used to estimate quantiles for a Q-Q plot is
  # determined by the argument prob.method.  For the functions
  # enormCensored and elnormCensored, for any estimation
  # method that involves Q-Q regression, the default value of
  # prob.method is "hirsch-stedinger" and the default value for the
  # plotting position constant is plot.pos.con=0.375.

  # Both Helsel (2012) and USEPA (2009) also use the Hirsch-Stedinger
  # probability method but set the plotting position constant to 0.

  with(EPA.09.Ex.15.1.manganese.df,
    elnormCensored(Manganese.ppb, Censored,
    method = "rROS", plot.pos.con = 0))$parameters
  # meanlog    sdlog
  #2.277175 1.261431

  #----------

  # Using the same data as above, compute a confidence interval
  # for the mean on the log-scale using the profile-likelihood
  # method.

  with(EPA.09.Ex.15.1.manganese.df,
    elnormCensored(Manganese.ppb, Censored, ci = TRUE))

  #Results of Distribution Parameter Estimation
  #Based on Type I Censored Data
  #--------------------------------------------
  #
  #Assumed Distribution:            Lognormal
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              2 5
  #
  #Estimated Parameter(s):          meanlog = 2.215905
  #                                 sdlog   = 1.356291
  #
  #Estimation Method:               MLE
  #
  #Data:                            Manganese.ppb
  #
  #Censoring Variable:              Censored
  #
  #Sample Size:                     25
  #
  #Percent Censored:                24%
  #
  #Confidence Interval for:         meanlog
  #
  #Confidence Interval Method:      Profile Likelihood
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 1.595062
  #                                 UCL = 2.771197
}
\keyword{ distribution }
\keyword{ htest }


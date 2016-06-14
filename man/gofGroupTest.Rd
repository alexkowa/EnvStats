\name{gofGroupTest}
\alias{gofGroupTest}
\alias{gofGroupTest.formula}
\alias{gofGroupTest.default}
\alias{gofGroupTest.data.frame}
\alias{gofGroupTest.matrix}
\alias{gofGroupTest.list}
\title{
  Goodness-of-Fit Test for a Specified Probability Distribution for Groups
}
\description{
  Perform a goodness-of-fit test to determine whether data in a set of 
  groups appear to all come from the same probability distribution 
  (with possibly different parameters for each group).
}
\usage{
gofGroupTest(object, ...)

\method{gofGroupTest}{formula}(object, data = NULL, subset, 
  na.action = na.pass, ...)

\method{gofGroupTest}{default}(object, group, test = "sw", 
  distribution = "norm", est.arg.list = NULL, n.classes = NULL, 
  cut.points = NULL, param.list = NULL, 
  estimate.params = ifelse(is.null(param.list), TRUE, FALSE), 
  n.param.est = NULL, correct = NULL, digits = .Options$digits, 
  exact = NULL, ws.method = "normal scores", 
  data.name = NULL, group.name = NULL, parent.of.data = NULL, 
  subset.expression = NULL, ...)

\method{gofGroupTest}{data.frame}(object, ...)

\method{gofGroupTest}{matrix}(object, ...)

\method{gofGroupTest}{list}(object, ...)
}
\arguments{
  \item{object}{
  an object containing data for 2 or more groups to be compared to the 
  hypothesized distribution specified by \code{distribution}.  In the default method, 
  the argument \code{object} must be a numeric vector.  
  When \code{object} is a data frame, all columns must be numeric.  
  When \code{object} is a matrix, it must be a numeric matrix.  
  When \code{object} is a list, all components must be numeric vectors.
  In the formula method, a symbolic specification of the form \code{y ~ g} 
  can be given, indicating the observations in the vector \code{y} are to be grouped 
  according to the levels of the factor \code{g}.  Missing (\code{NA}), undefined (\code{NaN}), 
  and infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{data}{
  when \code{object} is a formula, \code{data} specifies an optional data frame, list or 
  environment (or object coercible by \code{as.data.frame} to a data frame) containing the 
  variables in the model.  If not found in \code{data}, the variables are taken from 
  \code{environment(formula)}, typically the environment from which \cr
  \code{summaryStats} is called.
}
  \item{subset}{
  when \code{object} is a formula, \code{subset} specifies an optional vector specifying 
  a subset of observations to be used.
}
  \item{na.action}{
  when \code{object} is a formula, \code{na.action} specifies a function which indicates 
  what should happen when the data contain \code{NA}s. The default is \code{\link{na.pass}}.
}
  \item{group}{
  when \code{object} is a numeric vector, \code{group} is a factor or character vector 
  indicating which group each observation belongs to.  When \code{object} is a matrix or data frame
  this argument is ignored and the columns define the groups.  When \code{object} is a list 
  this argument is ignored and the components define the groups.  When \code{object} is a formula, 
  this argument is ignored and the right-hand side of the formula specifies the grouping variable.
}
  \item{test}{
  character string defining which goodness-of-fit test to perform on each group.  
  Possible values are:  
  \code{"sw"} (Shapiro-Wilk; the default), \code{"sf"} (Shapiro-Francia), 
  \code{"ppcc"} (Probability Plot Correlation Coefficient), \code{"skew"} (Zero-skew), 
  \code{"chisq"} (Chi-squared), \code{"ks"} (Kolmogorov-Smirnov), and 
  \code{"ws"} (Wilk-Shapiro test for Uniform [0, 1] distribution).
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
  \code{"zmlnorm.alt"} (zero-modified lognormal with alternative parameterization)).

  When \code{test="skew"}, only the values \code{"norm"} (normal), \code{"lnorm"} (lognormal), 
  \code{"lnorm.alt"} (lognormal with alternative parameterization), 
  \code{"zmnorm"} (zero-modified normal), \code{"zmlnorm"} (zero-modified lognormal (delta)), and \cr
  \code{"zmlnorm.alt"} (zero-modified lognormal with alternative parameterization) are allowed.

  When \code{test="ks"}, any continuous distribution is allowed.

  When \code{test="chisq"}, any distribuiton is allowed.

  When \code{test="ws"}, this argument is ignored.
}
  \item{est.arg.list}{
  a list of arguments to be passed to the function estimating the distribution parameters 
  for each group of observations.  
  For example, if \code{test="sw"} and \cr
  \code{distribution="gamma"}, setting \code{est.arg.list=list(method="bcmle")} 
  indicates using the bias-corrected maximum-likelihood estimators of shape and scale 
  (see the help file for \code{\link{egamma}}.  See the help file 
  \link{Estimating Distribution Parameters} for a list of estimating functions.  
  The default value is \cr
  \code{est.arg.list=NULL} so that all 
  default values for the estimating function are used.  This argument is ignored if 
  \code{estimate.params=FALSE}.

  When \code{test="sw"}, \code{test="sf"}, \code{test="ppcc"}, or \code{test="skew"}, 
  and you are testing for some form of normality (i.e., \link{Normal}, \link{Lognormal}, 
  \link[=Lognormal3]{Three-Parameter Lognormal}, 
  \link[=ZeroModifiedNormal]{Zero-Modified Normal}, or 
  \link[=ZeroModifiedLognormal]{Zero-Modified Lognormal (Delta)}), 
  the estimated parameters are provided in the 
  output merely for information, and the choice of the method of estimation has no effect 
  on the goodness-of-fit test statistics or p-values.

  When \code{test="ks"}, and \code{estimate.params=TRUE}, 
  the estimated parameters are used to specify the null hypothesis of which distribution 
  the data are assumed to come from. 

  When \code{test="chisq"} and \code{estimate.params=TRUE}, 
  the estimated parameters are used to specify the null hypothesis of which distribution 
  the data are assumed to come from. 

  When \code{test="ws"}, this argument is ignored.
}
  \item{n.classes}{
  for the case when \code{test="chisq"}, the number of cells into which the observations 
  within each group are to be allocated.  If the argument \code{cut.points} is supplied, 
  then \code{n.classes} is set to \code{length(cut.points)-1}.  The default value is \cr
  \code{ceiling(2* (length(x)^(2/5)))} and is recommended by Moore (1986).
}
  \item{cut.points}{
  for the case when \code{test="chisq"}, a vector of cutpoints that defines the cells for each 
  group of observations. 
  The element \code{x[i]} is allocated to cell \code{j} if \cr
  \code{cut.points[j]} < \code{x[i]} \eqn{\le} \code{cut.points[j+1]}. If \code{x[i]} is less than or equal to the first cutpoint or 
  greater than the last cutpoint, then \code{x[i]} is treated as missing.  If the 
  hypothesized distribution is discrete, \code{cut.points} must be supplied.  The default 
  value is \code{cut.points=NULL}, in which case the cutpoints are determined by 
  \code{n.classes} equi-probable intervals.
}
  \item{param.list}{
  for the case when \code{test="ks"} or \code{test="chisq"}, 
  a list with values for the parameters of the specified distribution.  See the help file 
  for \code{\link{Distribution.df}} for the names and possible values of the parameters 
  associated with each distribution.  The default value is \code{NULL}, which forces 
  estimation of the distribution parameters.  This argument is ignored if 
  \code{estimate.params=TRUE}.
}
  \item{estimate.params}{
  for the case when \code{test="ks"} or \code{test="chisq"}, 
  a logical scalar indicating whether to perform the goodness-of-fit test based on 
  estimating the distribution parameters (\code{estimate.params=TRUE}) or using the 
  user-supplied distribution parameters specified by \code{param.list} \cr
  (\code{estimate.params=FALSE}).  The default value of \code{estimate.params} is \code{TRUE} if 
  \code{param.list=NULL}, otherwise it is \code{FALSE}.
}
  \item{n.param.est}{
  for the case when \code{test="ks"} or \code{test="chisq"}, 
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
  a scalar indicating how many significant digits to print out for the parameters 
  associated with the hypothesized distribution.  The default value is \cr
  \code{.Options$digits}.
}
  \item{exact}{
  for the case when \code{test="ks"}, \code{exact=NULL} by default, but can be set to 
  a logical scalar indicating whether an exact p-value should be computed.  
  See the help file for \code{\link{ks.test}} for more information.
}
  \item{ws.method}{
  character string indicating which method to use when performing the 
  Wilk-Shapiro test for a \link[stats:Uniform]{Uniform [0,1] distribution} 
  on the p-values from the goodness-of-fit tests on each group.  Possible values 
  are \code{ws.method="normal scores"} (the default) or 
  \code{ws.method="chi-square scores"}.  See the subsection 
  \emph{Wilk-Shapiro goodness-of-fit test for Uniform [0, 1] distribution} 
  under the DETAILS section of the help file for \code{\link{gofTest}} 
  for more information.

  \bold{NOTE:} In the case where you are testing whether each group comes from a 
  \link[stats:Uniform]{Uniform [0,1] distribution} (i.e., when you set 
  \code{test="ws"}), the argument \code{ws.method} determines which score types 
  are used for each individual test of the groups as well.
}
  \item{data.name}{
  character string indicating the name of the data used for the goodness-of-fit tests.  
  The default value is \code{data.name=deparse(substitute(object))}.
}
  \item{group.name}{
  character string indicating the name of the data used to create the groups.
  The default value is \code{group.name=deparse(substitute(group))}.
}
  \item{parent.of.data}{
  character string indicating the source of the data used for the goodness-of-fit tests.
}
  \item{subset.expression}{
  character string indicating the expression used to subset the data.
}
  \item{\dots}{
  additional arguments affecting the goodness-of-fit test.
}
}
\details{
  The function \code{gofGroupTest} performs a goodness-of-fit test for each group of 
  data by calling the function \code{\link{gofTest}}.  Using the p-values from these 
  goodness-of-fit tests, it then calls the function \code{\link{gofTest}} with the 
  argument \code{test="ws"} to test whether the p-values appear to come from a 
  \link[stats:Uniform]{Uniform [0,1] distribution}.
}
\value{
  a list of class \code{"gofGroup"} containing the results of the group goodness-of-fit test.  
  Objects of class \code{"gofGroup"} have special printing and plotting methods.  
  See the help file for \code{\link{gofGroup.object}} for details.
}
\references{
  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009). 
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.  
  John Wiley & Sons, Hoboken.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C. p.17-17.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C.

  Wilk, M.B., and S.S. Shapiro. (1968). The Joint Assessment of Normality of Several Independent 
  Samples. \emph{Technometrics}, \bold{10}(4), 825-839.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The Wilk-Shapiro (1968) tests for a Uniform [0, 1] distribution were introduced in the context 
  of testing whether several independent samples all come from normal distributions, with 
  possibly different means and variances.  The function \code{gofGroupTest} extends 
  this idea to allow you to test whether several independent samples come from the same 
  distribution (e.g., gamma, extreme value, etc.), with possibly different parameters.

  Examples of simultaneously assessing whether several groups come from the same distribution 
  are given in USEPA (2009) and Gibbons et al. (2009).

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
  \code{\link{gofTest}}, \code{\link{gofGroup.object}}, \code{\link{print.gofGroup}}, 
  \code{\link{plot.gofGroup}}, \code{\link{qqPlot}}.
}
\examples{
  # Example 10-4 of USEPA (2009, page 10-20) gives an example of 
  # simultaneously testing the assumption of normality for nickel 
  # concentrations (ppb) in groundwater collected at 4 monitoring 
  # wells over 5 months.  The data for this example are stored in 
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


  # Test for a normal distribution at each well:
  #--------------------------------------------

  gofGroup.list <- gofGroupTest(Nickel.ppb ~ Well, 
    data = EPA.09.Ex.10.1.nickel.df)

  gofGroup.list

  #Results of Group Goodness-of-Fit Test
  #-------------------------------------
  #
  #Test Method:                     Wilk-Shapiro GOF (Normal Scores)
  #
  #Hypothesized Distribution:       Normal
  #
  #Data:                            Nickel.ppb
  #
  #Grouping Variable:               Well
  #
  #Data Source:                     EPA.09.Ex.10.1.nickel.df
  #
  #Number of Groups:                4
  #
  #Sample Sizes:                    Well.1 = 5
  #                                 Well.2 = 5
  #                                 Well.3 = 5
  #                                 Well.4 = 5
  #
  #Test Statistic:                  z (G) = -3.658696
  #
  #P-values for
  #Individual Tests:                Well.1 = 0.03510747
  #                                 Well.2 = 0.02385344
  #                                 Well.3 = 0.01120775
  #                                 Well.4 = 0.10681461
  #
  #P-value for
  #Group Test:                      0.0001267509
  #
  #Alternative Hypothesis:          At least one group
  #                                 does not come from a
  #                                 Normal Distribution.

  dev.new()
  plot(gofGroup.list)

  #----------

  # Test for a lognormal distribution at each well:
  #-----------------------------------------------

  gofGroupTest(Nickel.ppb ~ Well, data = EPA.09.Ex.10.1.nickel.df, 
    dist = "lnorm")

  #Results of Group Goodness-of-Fit Test
  #-------------------------------------
  #
  #Test Method:                     Wilk-Shapiro GOF (Normal Scores)
  #
  #Hypothesized Distribution:       Lognormal
  #
  #Data:                            Nickel.ppb
  #
  #Grouping Variable:               Well
  #
  #Data Source:                     EPA.09.Ex.10.1.nickel.df
  #
  #Number of Groups:                4
  #
  #Sample Sizes:                    Well.1 = 5
  #                                 Well.2 = 5
  #                                 Well.3 = 5
  #                                 Well.4 = 5
  #
  #Test Statistic:                  z (G) = 0.2401720
  #
  #P-values for
  #Individual Tests:                Well.1 = 0.6898164
  #                                 Well.2 = 0.6700394
  #                                 Well.3 = 0.3208299
  #                                 Well.4 = 0.5041375
  #
  #P-value for
  #Group Test:                      0.5949015
  #
  #Alternative Hypothesis:          At least one group
  #                                 does not come from a
  #                                 Lognormal Distribution.

  #----------
  # Clean up
  rm(gofGroup.list)
  graphics.off()
}
\keyword{ htest }
\keyword{ models }

\name{varGroupTest}
\alias{varGroupTest}
\alias{varGroupTest.formula}
\alias{varGroupTest.default}
\alias{varGroupTest.data.frame}
\alias{varGroupTest.matrix}
\alias{varGroupTest.list}
\title{
  Test for Homogeneity of Variance Among Two or More Groups
}
\description{
  Test the null hypothesis that the variances of two or more normal distributions are the same using 
  Levene's or Bartlett's test.
}
\usage{
varGroupTest(object, ...)

\method{varGroupTest}{formula}(object, data = NULL, subset, 
  na.action = na.pass, ...)

\method{varGroupTest}{default}(object, group, test = "Levene", 
  correct = TRUE, data.name = NULL, group.name = NULL, 
  parent.of.data = NULL, subset.expression = NULL, ...)

\method{varGroupTest}{data.frame}(object, ...)

\method{varGroupTest}{matrix}(object, ...)

\method{varGroupTest}{list}(object, ...)
}
\arguments{
  \item{object}{
  an object containing data for 2 or more groups whose variances are to be compared.  
  In the default method, the argument \code{object} must be a numeric vector.  
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
  character string indicating which test to use.  The possible values are \code{"Levene"} 
  (Levene's test; the default) and \code{"Bartlett"} (Bartlett's test).
}
  \item{correct}{
  logical scalar indicating whether to use the correction factor for Bartlett's test.  
  The default value is \code{correct=TRUE}.  This argument is ignored if \code{test="Levene"}.
}
  \item{data.name}{
  character string indicating the name of the data used for the group variance test.  
  The default value is \code{data.name=deparse(substitute(object))}.
}
  \item{group.name}{
  character string indicating the name of the data used to create the groups.
  The default value is \code{group.name=deparse(substitute(group))}.
}
  \item{parent.of.data}{
  character string indicating the source of the data used for the group variance test.
}
  \item{subset.expression}{
  character string indicating the expression used to subset the data.
}
  \item{\dots}{
  additional arguments affecting the group variance test.
}
}
\details{
  The function \code{varGroupTest} performs Levene's or Bartlett's test for 
  homogeneity of variance among two or more groups.  The \R function \code{\link{var.test}} 
  compares two variances.

  Bartlett's test is very sensitive to the assumption of normality and will tend to give 
  significant results even when the null hypothesis is true if the underlying distributions 
  have long tails (e.g., are leptokurtic).  Levene's test is almost as powerful as Bartlett's 
  test when the underlying distributions are normal, and unlike Bartlett's test it tends to 
  maintain the assumed \eqn{alpha}-level when the underlying distributions are not normal 
  (Snedecor and Cochran, 1989, p.252; Milliken and Johnson, 1992, p.22; Conover et al., 1981).  
  Thus, Levene's test is generally recommended over Bartlett's test.
}
\value{
  a list of class \code{"htestEnvStats"} containing the results of the group variance test.  
  Objects of class \code{"htestEnvStats"} have special printing and plotting methods.  
  See the help file for \code{\link{htestEnvStats.object}} for details.
}
\references{
  Conover, W.J., M.E. Johnson, and M.M. Johnson. (1981). A Comparative Study of Tests for Homogeneity 
  of Variances, with Applications to the Outer Continental Shelf Bidding Data. 
  \emph{Technometrics} \bold{23}(4), 351-361.

  Davis, C.B. (1994). Environmental Regulatory Statistics. In Patil, G.P., and C.R. Rao, eds., 
  \emph{Handbook of Statistics, Vol. 12: Environmental Statistics}. North-Holland, Amsterdam, 
  a division of Elsevier, New York, NY, Chapter 26, 817-865.

  Milliken, G.A., and D.E. Johnson. (1992). \emph{Analysis of Messy Data, Volume I: Designed Experiments}. 
  Chapman & Hall, New York.

  Snedecor, G.W., and W.G. Cochran. (1989). \emph{Statistical Methods, Eighth Edition}. 
  Iowa State University Press, Ames Iowa.

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
  Chapter 11 of USEPA (2009) discusses using Levene's test to test the assumption of 
  equal variances between monitoring wells or to test that the variance is stable over time 
  when performing intrawell tests.
}
\seealso{
  \code{\link{var.test}}, \code{\link{varTest}}.
}
\examples{
  # Example 11-2 of USEPA (2009, page 11-7) gives an example of 
  # testing the assumption of equal variances across wells for arsenic  
  # concentrations (ppb) in groundwater collected at 6 monitoring 
  # wells over 4 months.  The data for this example are stored in 
  # EPA.09.Ex.11.1.arsenic.df.

  head(EPA.09.Ex.11.1.arsenic.df)
  #  Arsenic.ppb Month Well
  #1        22.9     1    1
  #2         3.1     2    1
  #3        35.7     3    1
  #4         4.2     4    1
  #5         2.0     1    2
  #6         1.2     2    2

  longToWide(EPA.09.Ex.11.1.arsenic.df, "Arsenic.ppb", "Month", "Well", 
    paste.row.name = TRUE, paste.col.name = TRUE)
  #        Well.1 Well.2 Well.3 Well.4 Well.5 Well.6
  #Month.1   22.9    2.0    2.0    7.8   24.9    0.3
  #Month.2    3.1    1.2  109.4    9.3    1.3    4.8
  #Month.3   35.7    7.8    4.5   25.9    0.8    2.8
  #Month.4    4.2   52.0    2.5    2.0   27.0    1.2

  varGroupTest(Arsenic.ppb ~ Well, data = EPA.09.Ex.11.1.arsenic.df)

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 Ratio of each pair of variances = 1
  #
  #Alternative Hypothesis:          At least one variance differs
  #
  #Test Name:                       Levene's Test for
  #                                 Homogenity of Variance
  #
  #Estimated Parameter(s):          Well.1 =  246.8158
  #                                 Well.2 =  592.6767
  #                                 Well.3 = 2831.4067
  #                                 Well.4 =  105.2967
  #                                 Well.5 =  207.4467
  #                                 Well.6 =    3.9025
  #
  #Data:                            Arsenic.ppb
  #
  #Grouping Variable:               Well
  #
  #Data Source:                     EPA.09.Ex.11.1.arsenic.df
  #
  #Sample Sizes:                    Well.1 = 4
  #                                 Well.2 = 4
  #                                 Well.3 = 4
  #                                 Well.4 = 4
  #                                 Well.5 = 4
  #                                 Well.6 = 4
  #
  #Test Statistic:                  F = 4.564176
  #
  #Test Statistic Parameters:       num df   =  5
  #                                 denom df = 18
  #
  #P-value:                         0.007294084
}
\keyword{ htestEnvStats }
\keyword{ models }

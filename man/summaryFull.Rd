\name{summaryFull}
\alias{summaryFull}
\alias{summaryFull.formula}
\alias{summaryFull.default}
\alias{summaryFull.factor}
\alias{summaryFull.data.frame}
\alias{summaryFull.matrix}
\alias{summaryFull.list}
\title{
  Full Complement of Summary Statistics
}
\description{
  \code{summaryFull} is a generic function used to produce a full complement of summary statistics.  
  The function invokes particular \code{\link{methods}} which depend on the \code{\link{class}} of 
  the first argument.  The summary statistics include: sample size, number of missing values, 
  mean, median, trimmed mean, geometric mean, skew, kurtosis, min, max, range, 1st quartile, 3rd quartile, 
  standard deviation, geometric standard deviation, interquartile range, median absolute deviation, and 
  coefficient of variation.
}
\usage{
summaryFull(object, ...)

\method{summaryFull}{formula}(object, data = NULL, subset, 
  na.action = na.pass, ...)

\method{summaryFull}{default}(object, group = NULL, 
    combine.groups = FALSE, drop.unused.levels = TRUE, 
    rm.group.na = TRUE, stats = NULL, trim = 0.1, 
    sd.method = "sqrt.unbiased", geo.sd.method = "sqrt.unbiased", 
    skew.list = list(), kurtosis.list = list(), 
    cv.list = list(), digits = max(3, getOption("digits") - 3), 
    digit.type = "signif", stats.in.rows = TRUE, 
    drop0trailing = TRUE, data.name = deparse(substitute(object)), 
    ...)

\method{summaryFull}{data.frame}(object, ...)

\method{summaryFull}{matrix}(object, ...)

\method{summaryFull}{list}(object, ...)
}
\arguments{
  \item{object}{
  an object for which summary statistics are desired.  In the default method, 
  the argument \code{object} must be a numeric vector, a data frame, a matrix, or a list.  
  When \code{object} is a data frame, all columns must be numeric.  
  When \code{object} is a matrix, it must be a numeric matrix.  
  When \code{object} is a list, all components must be numeric vectors.
  In the formula method, a symbolic specification of the form \code{y ~ g} 
  can be given, indicating the observations in the vector \code{y} are to be grouped 
  according to the levels of the factor \code{g} (the form \code{y ~ 1} indicates no grouping).  
  \code{NA}s are allowed in the data.
}
  \item{data}{
  when \code{object} is a formula, \code{data} specifies an optional data frame, list or 
  environment (or object coercible by \code{as.data.frame} to a data frame) containing the 
  variables in the model.  If not found in \code{data}, the variables are taken from 
  \code{environment(formula)}, typically the environment from which \cr
  \code{summaryFull} is called.
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
  this argument is ignored and the columns define the groups.  When \code{object} is a formula, 
  this argument is ignored and the right-hand side of the formula specifies the grouping variable.
}
  \item{combine.groups}{
  logical scalar indicating whether to show summary statistics for all groups combined.  
  The default value is \code{FALSE}.
}
  \item{drop.unused.levels}{
  when \code{drop.unused.levels=TRUE}, groups with no observations are dropped.
}
  \item{rm.group.na}{
  logical scalar indicating whether to remove missing values from the \code{group} argument.  By 
  default \code{rm.group.na=TRUE}.
}
  \item{stats}{
  character vector indicating which statistics to compute.  Possible elements of the character 
  vector include:  \code{"all"} (indicating to include all summary statistics), 
  \code{"for.non.pos"} (only compute statistics that are meaningful for datasets with non-positive values), 
  \code{"n"} (number of non-missing values), \code{"n.miss"} (number of missing values), 
  \code{"mean"}, \code{"median"}, \cr
  \code{"trimmed.mean"}, \code{"geo.mean"}, \code{"skew"}, \code{"kurtosis"}, \code{"min"}, \code{"max"}, \cr
  \code{"range"}, \code{"1st.quart"}, \code{"3rd.quart"}, \code{"sd"}, \code{"geo.sd"}, \code{"iqr"}, \cr
  \code{"mad"}, \code{"cv"}.  The default value is \code{stats="for.non.pos"} when \code{object} contains 
  non-positive values (i.e., values \eqn{\le 0}), and \code{stats="all"} when \code{object} contains 
  only positive values.
}
  \item{trim}{
  fraction (between 0 and 0.5 inclusive) of values to be trimmed from each end of the ordered data 
  to compute the trimmed mean.  The default value is \code{trim=0.1}.  
  If \code{trim=0.5}, this yields the median.
}
  \item{sd.method}{
  character string specifying what method to use to compute the sample standard deviation.  
  The possible values are \code{"sqrt.ubiased"} (the square root of the unbiased estimate of variance; 
  the default), or \code{"moments"} (the method of moments estimator).
}
  \item{geo.sd.method}{
  character string specifying what method to use to compute the sample standard deviation of the 
  log-transformed observations prior to exponentiating this quantity.  The possible values are 
  \code{"sqrt.ubiased"} (the square root of the unbiased estimate of variance; the default), or 
  \code{"moments"} (the method of moments estimator).  See the help file for \code{\link{geoSD}} 
  for more information.
}
  \item{skew.list}{
  list of arguments to supply to the \code{\link{skewness}} function.  See the help file for 
  \code{\link{skewness}} for more information.  The default value is \code{skew.list=list()}, 
  which results in using the default arguments to \code{\link{skewness}}.
}
  \item{kurtosis.list}{
  list of arguments to supply to the \code{\link{kurtosis}} function.  See the help file for 
  \code{\link{kurtosis}} for more information.  The default value is \cr
  \code{kurtosis.list=list()}, 
  which results in using the default arguments to \cr
  \code{\link{kurtosis}}.
}
  \item{cv.list}{
  list of arguments to supply to the \code{\link{cv}} function.  See the help file for \code{\link{cv}} 
  for more information.  The default value is \code{cv.list=list()}, which results in using the default 
  arguments to \code{\link{cv}}.
}
  \item{digits}{
  integer indicating the number of digits to use for the summary statistics.  
  When \code{digit.type="signif"}, \code{digits} indicates the number of significant 
  digits.  When \code{digit.type="round"}, \code{digits} indicates the number of 
  decimal places to round to.  The default value is \code{max(3, getOption("digits") - 3)}, 
  that is, the maximum of 3 versus the current setting of the \code{"digits"} component 
  of \code{.Options} minus 3.
}
  \item{digit.type}{
  character string indicating whether the \code{digits} argument refers to significant digits 
  (\code{digit.type="signif"}, the default), or how many decimal places to round to 
  (\code{digit.type="round"}).
}
  \item{stats.in.rows}{
  logical scalar indicating whether to show the summary statistics in the rows or columns of the 
  output.  The default is \code{stats.in.rows=TRUE}.
}
  \item{drop0trailing}{
  logical scalar indicating whether to drop trailing 0's when printing the summary statistics.  
  The value of this argument is added as an attribute to the returned list and is used by the 
  \code{\link{print.summaryStats}} function.  The default value is \code{TRUE}.
}
  \item{data.name}{
  character string indicating the name of the data used for the summary statistics.
}
  \item{\dots}{
  additional arguments affecting the summary statistics produced.
}
}
\details{
  The function \code{summaryFull} returns summary statistics that are useful to describe various 
  characteristics of one or more variables.  It is an extended version of the built-in R function 
  \code{\link{summary}} specifically for non-factor numeric data.  The table below shows what 
  statistics are computed and what functions are called by \code{summaryFull} to compute these statistics.

  The object returned by \code{summaryFull} is useful for printing or report purposes.  You may also 
  use the functions that \code{summaryFull} calls (see table below) to compute summary statistics to 
  be used by other functions.

  See the help files for the functions listed in the table below for more information on these 
  summary statistics.

  \tabular{ll}{
  \bold{Summary Statistic}     \tab \bold{Function Used}               \cr
  Mean                         \tab \code{\link{mean}}                            \cr
  Median                       \tab \code{\link{median}}                          \cr
  Trimmed Mean                 \tab \code{\link{mean}} with \code{trim} argument  \cr
  Geometric Mean               \tab \code{\link{geoMean}}                         \cr
  Skew                         \tab \code{\link{skewness}}                        \cr
  Kurtosis                     \tab \code{\link{kurtosis}}                        \cr
  Min                          \tab \code{\link{min}}                             \cr
  Max                          \tab \code{\link{max}}                             \cr
  Range                        \tab \code{\link{range}} and \code{\link{diff}}    \cr
  1st Quartile                 \tab \code{\link{quantile}}                        \cr
  3rd Quartile                 \tab \code{\link{quantile}}                        \cr
  Standard Deviation           \tab \code{\link{sd}}                              \cr
  Geometric Standard Deviation \tab \code{\link{geoSD}}                           \cr
  Interquartile Range          \tab \code{\link{iqr}}                             \cr
  Median Absolute Deviation    \tab \code{\link{mad}}                             \cr
  Coefficient of Variation     \tab \code{\link{cv}}                              \cr
  }
}
\value{
  an object of class \code{"summaryStats"} (see \code{\link{summaryStats.object}}.  
  Objects of class \code{"summaryStats"} are numeric matrices that contain the 
  summary statisics produced by a call to \code{\link{summaryStats}} or \code{summaryFull}.  
  These objects have a special printing method that by default removes 
  trailing zeros for sample size entries and prints blanks for statistics that are 
  normally displayed as \code{NA} (see \code{\link{print.summaryStats}}).
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). 
  \emph{Statistics for Environmental Engineers, Second Edition}. 
  Lewis Publishers, Boca Raton, FL.

  Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental 
  Pollution Monitoring}.  Van Nostrand Reinhold, NY.

  Helsel, D.R., and R.M. Hirsch. (1992). 
  \emph{Statistical Methods in Water Resources Research}. 
  Elsevier, New York, NY.

  Leidel, N.A., K.A. Busch, and J.R. Lynch. (1977). \emph{Occupational Exposure Sampling Strategy Manual}. 
  U.S. Department of Health, Education, and Welfare, Public Health Service, Center for Disease Control, 
  National Institute for Occupational Safety and Health, Cincinnati, Ohio 45226, January, 1977, pp.102-103.

  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton, FL.

  Ott, W.R. (1995). \emph{Environmental Statistics and Data Analysis}. 
  Lewis Publishers, Boca Raton, FL.

  Zar, J.H. (2010). \emph{Biostatistical Analysis, Fifth Edition}. 
  Prentice-Hall, Upper Saddle River, NJ.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{summary}}, \code{\link{summaryStats}}.
}
\examples{
  # Generate 20 observations from a lognormal distribution with 
  # parameters mean=10 and cv=1, and compute the summary statistics.  
  # (Note: the call to set.seed simply allows you to reproduce this 
  # example.)

  set.seed(250) 

  dat <- rlnormAlt(20, mean=10, cv=1) 

  summary(dat) 
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #2.608   4.995   6.235   7.490   9.295  15.440

  summaryFull(dat) 
  #                             dat     
  #N                            20      
  #Mean                          7.49   
  #Median                        6.235  
  #10% Trimmed Mean              7.125  
  #Geometric Mean                6.674  
  #Skew                          0.9877 
  #Kurtosis                     -0.03539
  #Min                           2.608  
  #Max                          15.44   
  #Range                        12.83   
  #1st Quartile                  4.995  
  #3rd Quartile                  9.295  
  #Standard Deviation            3.803  
  #Geometric Standard Deviation  1.634  
  #Interquartile Range           4.3    
  #Median Absolute Deviation     2.607  
  #Coefficient of Variation      0.5078 

  #----------

  # Compare summary statistics for normal and lognormal data:
  log.dat <- log(dat) 

  summaryFull(list(dat = dat, log.dat = log.dat))
  #                             dat      log.dat
  #N                            20       20     
  #Mean                          7.49     1.898 
  #Median                        6.235    1.83  
  #10% Trimmed Mean              7.125    1.902 
  #Geometric Mean                6.674    1.835 
  #Skew                          0.9877   0.1319
  #Kurtosis                     -0.03539 -0.4288
  #Min                           2.608    0.9587
  #Max                          15.44     2.737 
  #Range                        12.83     1.778 
  #1st Quartile                  4.995    1.607 
  #3rd Quartile                  9.295    2.227 
  #Standard Deviation            3.803    0.4913
  #Geometric Standard Deviation  1.634    1.315 
  #Interquartile Range           4.3      0.62  
  #Median Absolute Deviation     2.607    0.4915
  #Coefficient of Variation      0.5078   0.2588

  # Clean up
  rm(dat, log.dat)

  #--------------------------------------------------------------------

  # Compute summary statistics for 10 observations from a normal 
  # distribution with parameters mean=0 and sd=1.  Note that the 
  # geometric mean and geometric standard deviation are not computed 
  # since some of the observations are non-positive.

  set.seed(287) 

  dat <- rnorm(10) 

  summaryFull(dat) 
  #                          dat     
  #N                         10      
  #Mean                       0.07406
  #Median                     0.1095 
  #10% Trimmed Mean           0.1051 
  #Skew                      -0.1646 
  #Kurtosis                  -0.7135 
  #Min                       -1.549  
  #Max                        1.449  
  #Range                      2.998  
  #1st Quartile              -0.5834 
  #3rd Quartile               0.6966 
  #Standard Deviation         0.9412 
  #Interquartile Range        1.28   
  #Median Absolute Deviation  1.05

  # Clean up
  rm(dat)

  #--------------------------------------------------------------------

  # Compute summary statistics for the TcCB data given in USEPA (1994b) 
  # (the data are stored in EPA.94b.tccb.df).  Arbitrarily set the one 
  # censored observation to the censoring level. Group by the variable 
  # Area.

  summaryFull(TcCB ~ Area, data = EPA.94b.tccb.df)
  #                             Cleanup  Reference
  #N                             77       47      
  #Mean                           3.915    0.5985 
  #Median                         0.43     0.54   
  #10% Trimmed Mean               0.6846   0.5728 
  #Geometric Mean                 0.5784   0.5382 
  #Skew                           7.717    0.9019 
  #Kurtosis                      62.67     0.132  
  #Min                            0.09     0.22   
  #Max                          168.6      1.33   
  #Range                        168.5      1.11   
  #1st Quartile                   0.23     0.39   
  #3rd Quartile                   1.1      0.75   
  #Standard Deviation            20.02     0.2836 
  #Geometric Standard Deviation   3.898    1.597  
  #Interquartile Range            0.87     0.36   
  #Median Absolute Deviation      0.3558   0.2669 
  #Coefficient of Variation       5.112    0.4739 
}
\keyword{ univar }

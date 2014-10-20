\name{gofOutlier.object}
\alias{gofOutlier.object}
\alias{gofOutlier}
\docType{class}
\title{
  S3 Class "gofOutlier"
}
\description{
  Objects of S3 class \code{"gofOutlier"} are returned by the \pkg{EnvStats} function 
  \code{\link{rosnerTest}}.
}
\details{
  Objects of S3 class \code{"gofOutlier"} are lists that contain 
  information about the assumed distribution, the test statistics, 
  the Type I error level, and the number of outliers detected.
}
\section{Methods}{
  Generic functions that have methods for objects of class 
  \code{"gofOutlier"} include: \cr
  \code{\link{print}}.
}
\value{
  \strong{Required Components} \cr
  The following components must be included in a legitimate list of 
  class \code{"gofOutlier"}.

  \item{distribution}{a character string indicating the name of the 
    assumed distribution (see \cr
    \code{\link{Distribution.df}}).}
  \item{statistic}{a numeric vector with a names attribute containing 
    the names and values of the outlier test statistic for each outlier tested.}
  \item{sample.size}{a numeric scalar containing the number 
    of non-missing observations in the sample used for the 
    outlier test.}
  \item{parameters}{numeric vector with a names attribute containing 
    the name(s) and value(s) of the parameter(s) associated with the 
    test statistic given in the \code{statistic} component.}
  \item{alpha}{numeric scalar indicating the Type I error level.}
  \item{crit.value}{numeric vector containing the critical values associated with 
    the test for each outlier.}
  \item{alternative}{character string indicating the alternative hypothesis.}
  \item{method}{character string indicating the name of the outlier test.}
  \item{data}{numeric vector containing the data actually used for 
    the outlier test (i.e., the original data without any missing or infinite values).}
  \item{data.name}{character string indicating the name of the data 
    object used for the goodness-of-fit test.}
  \item{all.stats}{data frame containing all of the results of the test.}
  \cr

  \strong{Optional Components} \cr
  The following component is included when the data object 
  contains missing (\code{NA}), undefined (\code{NaN}) and/or infinite 
  (\code{Inf}, \code{-Inf}) values.

  \item{bad.obs}{numeric scalar indicating the number of missing (\code{NA}), 
    undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf}) 
    values that were removed from the data object prior to performing 
    the test for outliers.}
}
\note{
  Since objects of class \code{"gofOutlier"} are lists, you may extract 
  their components with the \code{$} and \code{[[} operators.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{rosnerTest}}, \code{\link{print.gofOutlier}},  
  \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests}.
}
\examples{
  # Create an object of class "gofOutlier", then print it out. 
  # (Note: the call to set.seed simply allows you to reproduce 
  # this example.)

  set.seed(250) 

  dat <- c(rnorm(30, mean = 3, sd = 2), rnorm(3, mean = 10, sd = 1)) 

  gofOutlier.obj <- rosnerTest(dat, k = 4) 

  mode(gofOutlier.obj) 
  #[1] "list" 

  class(gofOutlier.obj) 
  #[1] "gofOutlier" 

  names(gofOutlier.obj) 
  # [1] "distribution" "statistic"    "sample.size"  "parameters"  
  # [5] "alpha"        "crit.value"   "n.outliers"   "alternative" 
  # [9] "method"       "data"         "data.name"    "bad.obs"     
  #[13] "all.stats"

  gofOutlier.obj 

  #Results of Outlier Test
  #-------------------------
  #
  #Test Method:                     Rosner's Test for Outliers
  #
  #Hypothesized Distribution:       Normal
  #
  #Data:                            dat
  #
  #Sample Size:                     33
  #
  #Test Statistics:                 R.1 = 2.848514
  #                                 R.2 = 3.086875
  #                                 R.3 = 3.033044
  #                                 R.4 = 2.380235
  #
  #Test Statistic Parameter:        k = 4
  #
  #Alternative Hypothesis:          Up to 4 observations are not
  #                                 from the same Distribution.
  #
  #Type I Error:                    5%
  #
  #Number of Outliers Detected:     3
  #
  #  i   Mean.i     SD.i      Value Obs.Num    R.i+1 lambda.i+1 Outlier
  #1 0 3.549744 2.531011 10.7593656      33 2.848514   2.951949    TRUE
  #2 1 3.324444 2.209872 10.1460427      31 3.086875   2.938048    TRUE
  #3 2 3.104392 1.856109  8.7340527      32 3.033044   2.923571    TRUE
  #4 3 2.916737 1.560335 -0.7972275      25 2.380235   2.908473   FALSE

  #==========

  # Extract the data frame with all the test results
  #-------------------------------------------------

  gofOutlier.obj$all.stats
  #  i   Mean.i     SD.i      Value Obs.Num    R.i+1 lambda.i+1 Outlier
  #1 0 3.549744 2.531011 10.7593656      33 2.848514   2.951949    TRUE
  #2 1 3.324444 2.209872 10.1460427      31 3.086875   2.938048    TRUE
  #3 2 3.104392 1.856109  8.7340527      32 3.033044   2.923571    TRUE
  #4 3 2.916737 1.560335 -0.7972275      25 2.380235   2.908473   FALSE

  #==========

  # Clean up
  #---------
  rm(dat, gofOutlier.obj)
}
\keyword{classes}

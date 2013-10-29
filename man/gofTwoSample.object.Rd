\name{gofTwoSample.object}
\alias{gofTwoSample.object}
\alias{gofTwoSample}
\docType{class}
\title{
  S3 Class "gofTwoSample"
}
\description{
  Objects of S3 class \code{"gofTwoSample"} are returned by the \pkg{EnvStats} function 
  \code{\link{gofTest}} when both the \code{x} and \code{y} arguments are supplied.
}
\details{
  Objects of S3 class \code{"gofTwoSample"} are lists that contain 
  information about the assumed distribution, the estimated or 
  user-supplied distribution parameters, and the test statistic 
  and p-value.
}
\section{Methods}{
  Generic functions that have methods for objects of class 
  \code{"gofTwoSample"} include: \cr
  \code{\link{print}}, \code{\link{plot}}.
}
\value{
  \strong{Required Components} \cr
  The following components must be included in a legitimate list of 
  class \code{"gofTwoSample"}.

  \item{distribution}{a character string with the value \code{"Equal"}.}
  \item{statistic}{a numeric scalar with a names attribute containing 
    the name and value of the goodness-of-fit statistic.}
  \item{sample.size}{a numeric scalar containing the number 
    of non-missing observations in the sample used for the 
    goodness-of-fit test.}
  \item{parameters}{numeric vector with a names attribute containing 
    the name(s) and value(s) of the parameter(s) associated with the 
    test statistic given in the \code{statistic} component.}
  \item{p.value}{numeric scalar containing the p-value associated with 
    the goodness-of-fit statistic.}
  \item{alternative}{character string indicating the alternative hypothesis.}
  \item{method}{character string indicating the name of the 
    goodness-of-fit test.}
  \item{data}{a list of length 2 containing the numeric vectors actually used for 
    the goodness-of-fit test (i.e., the original data but with any 
    missing or infinite values removed).}
  \item{data.name}{a character vector of length 2 indicating the name of the data 
    object used for the \code{x} argument and the name of the data object used 
    for the \code{y} argument in the goodness-of-fit test.}

  \strong{Optional Component} \cr
  The following component is included when the arguments \code{x} and/or \code{y}
  contain missing (\code{NA}), undefined (\code{NaN}) and/or infinite 
  (\code{Inf}, \code{-Inf}) values.

  \item{bad.obs}{numeric vector of length 2 indicating the number of missing (\code{NA}), 
    undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf}) 
    values that were removed from the data in the \code{x} and \code{y} arguments 
    prior to performing the goodness-of-fit test.}
}
\note{
  Since objects of class \code{"gofTwoSample"} are lists, you may extract 
  their components with the \code{$} and \code{[[} operators.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{print.gofTwoSample}}, \code{\link{plot.gofTwoSample}}, 
  \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests}.
}
\examples{
  # Create an object of class "gofTwoSample", then print it out. 

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
\keyword{classes}

\name{gofCensored.object}
\alias{gofCensored.object}
\alias{gofCensored}
\docType{class}
\title{
  S3 Class "gofCensored"
}
\description{
  Objects of S3 class \code{"gofCensored"} are returned by the \pkg{EnvStats} function 
  \code{\link{gofTestCensored}}.
}
\details{
  Objects of S3 class \code{"gofCensored"} are lists that contain 
  information about the assumed distribution, the amount of censoring, 
  the estimated or user-supplied distribution parameters, and the test 
  statistic and p-value.
}
\section{Methods}{
  Generic functions that have methods for objects of class 
  \code{"gofCensored"} include: \cr
  \code{\link{print}}, \code{\link{plot}}.
}
\value{
  \strong{Required Components} \cr
  The following components must be included in a legitimate list of 
  class \code{"gofCensored"}.

  \item{distribution}{a character string indicating the name of the 
    assumed distribution (see \cr
    \code{\link{Distribution.df}}).}
  \item{dist.abb}{a character string containing the abbreviated name 
    of the distribution (see \cr
    \code{\link{Distribution.df}}).}
  \item{distribution.parameters}{a numeric vector 
    with a names attribute containing the names and values of the 
    estimated or user-supplied distribution parameters associated 
    with the assumed distribution.}
  \item{n.param.est}{a scalar indicating the number of distribution 
    parameters estimated prior to performing the goodness-of-fit 
    test. The value of this component will be \code{0} if the parameters 
    were supplied by the user.}
  \item{estimation.method}{a character string indicating the method 
    used to compute the estimated parameters.  The value of this 
    component will depend on the available estimation methods 
    (see \code{\link{Distribution.df}}).  The value of this component 
    will be \code{NULL} if the parameters were supplied by the user.}
  \item{statistic}{a numeric scalar with a names attribute containing 
    the name and value of the goodness-of-fit statistic.}
  \item{sample.size}{a numeric scalar containing the number 
    of non-missing observations in the sample used for the 
    goodness-of-fit test.}
  \item{censoring.side}{character string indicating whether the data are 
    left- or right-censored.}
  \item{censoring.levels}{numeric scalar or vector indicating the censoring level(s).}
  \item{percent.censored}{numeric scalar indicating the percent of non-missing 
    observations that are censored.}
  \item{parameters}{numeric vector with a names attribute containing 
    the name(s) and value(s) of the parameter(s) associated with the 
    test statistic given in the \code{statistic} component.}
  \item{z.value}{(except when \code{test="chisq"} or \code{test="ks"}) 
    numeric scalar containing the z-value associated with the 
    goodness-of-fit statistic.}
  \item{p.value}{numeric scalar containing the p-value associated with 
    the goodness-of-fit statistic.}
  \item{alternative}{character string indicating the alternative hypothesis.}
  \item{method}{character string indicating the name of the 
    goodness-of-fit test (e.g., \cr
    \code{"Shapiro-Wilk GOF"}).}
  \item{data.name}{character string indicating the name of the data 
    object used for the goodness-of-fit test.}
  \item{censored}{logical vector indicating which observations are censored.}
  \item{censoring.name}{character string indicating the name of the object 
    used to indicate the censoring.} \cr

  \strong{Optional Components} \cr
  The following components are included when the argument \code{keep.data} is 
  set to \code{TRUE} in the call to the function producing the 
  object of class \code{"gofCensored"}.
  \item{data}{numeric vector containing the data actually used for 
    the goodness-of-fit test (i.e., the original data without any 
    missing or infinite values).}
  \item{censored}{logical vector indicating the censoring status of the data 
    actually used for the goodness-of-fit test.} \cr

  The following component is included when the data object 
  contains missing (\code{NA}), undefined (\code{NaN}) and/or infinite 
  (\code{Inf}, \code{-Inf}) values.

  \item{bad.obs}{numeric scalar indicating the number of missing (\code{NA}), 
    undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf}) 
    values that were removed from the data object prior to performing 
    the goodness-of-fit test.}
}
\note{
  Since objects of class \code{"gofCensored"} are lists, you may extract 
  their components with the \code{$} and \code{[[} operators.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{gofTestCensored}}, \code{\link{print.gofCensored}}, 
  \code{\link{plot.gofCensored}}, 
  \link[=FcnsByCatCensoredData]{Censored Data}, 
  \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests}, 
  \code{\link{Distribution.df}}, \code{gof.object}.
}
\examples{
  # Create an object of class "gofCensored", then print it out. 
  #------------------------------------------------------------

  gofCensored.obj <- with(EPA.09.Ex.15.1.manganese.df,
    gofTestCensored(Manganese.ppb, Censored, test = "sf"))

  mode(gofCensored.obj) 
  #[1] "list" 

  class(gofCensored.obj) 
  #[1] "gofCensored" 

  names(gofCensored.obj) 
  # [1] "distribution"            "dist.abb"               
  # [3] "distribution.parameters" "n.param.est"            
  # [5] "estimation.method"       "statistic"              
  # [7] "sample.size"             "censoring.side"         
  # [9] "censoring.levels"        "percent.censored"       
  #[11] "parameters"              "z.value"                
  #[13] "p.value"                 "alternative"            
  #[15] "method"                  "data"                   
  #[17] "data.name"               "censored"               
  #[19] "censoring.name"          "bad.obs" 

  gofCensored.obj 
  
  #Results of Goodness-of-Fit Test
  #Based on Type I Censored Data
  #-------------------------------
  #
  #Test Method:                     Shapiro-Francia GOF
  #                                 (Multiply Censored Data)
  #
  #Hypothesized Distribution:       Normal
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              2 5 
  #
  #Estimated Parameter(s):          mean = 15.23508
  #                                 sd   = 30.62812
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
  #Test Statistic:                  W = 0.8368016
  #
  #Test Statistic Parameters:       N     = 25.00
  #                                 DELTA =  0.24
  #
  #P-value:                         0.004662658
  #
  #Alternative Hypothesis:          True cdf does not equal the
  #                                 Normal Distribution.

  #==========

  # Extract the p-value
  #--------------------

  gofCensored.obj$p.value
  #[1] 0.004662658

  #==========

  # Plot the results of the test
  #-----------------------------

  dev.new()
  plot(gofCensored.obj)

  #==========

  # Clean up
  #---------
  rm(gofCensored.obj)
  graphics.off()
}
\keyword{classes}

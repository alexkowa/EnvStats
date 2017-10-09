\name{distChoose.object}
\alias{distChoose.object}
\docType{class}
\title{
  S3 Class "distChoose"
}
\description{
  Objects of S3 class \code{"distChoose"} are returned by the \pkg{EnvStats} function 
  \code{\link{distChoose}}.
}
\details{
  Objects of S3 class \code{"distChoose"} are lists that contain 
  information about the candidate distributions, the estimated distribution 
  parameters for each candidate distribution, and the test statistics and 
  p-values associated with each candidate distribution.
}
\section{Methods}{
  Generic functions that have methods for objects of class 
  \code{"distChoose"} include: \cr
  \code{\link{print}}.
}
\value{
  \strong{Required Components} \cr
  The following components must be included in a legitimate list of 
  class \code{"distChoose"}.

  \item{choices}{a character vector containing the full names  
    of the candidate distributions. (see \code{\link{Distribution.df}}).}
  \item{method}{a character string denoting which method was used.}
  \item{decision}{a character vector containing the full name of the chosen distribution.}
  \item{alpha}{a numeric scalar between 0 and 1 specifying the Type I error 
    associated with each goodness-of-fit test.}
  \item{distribution.parameters}{a numeric vector containing the estimated 
    parameters associated with the chosen distribution.}
  \item{estimation.method}{a character string indicating the method 
    used to compute the estimated parameters associated with the chosen 
    distribution.  The value of this component will depend on the 
    available estimation methods (see \code{\link{Distribution.df}}).}
  \item{sample.size}{a numeric scalar containing the number 
    of non-missing observations in the sample used for the 
    goodness-of-fit tests.}
  \item{test.results}{a list with the same number of components as the number 
    of elements in the component \code{choices}.  The names of the list are the 
    distribution abbreviations of the candidate distributions.  
    (See the help file for \code{\link{Distribution.df}} for a list 
      of distributions and their abbreviations.)
    Each component is an object of class \code{\link[=gof.object]{gof}} 
    containing the results of the goodness-of-fit test for that particular 
    hypothesized distribution.}
  \item{data.name}{character string indicating the name of the data 
    object used for the goodness-of-fit tests.}
  \cr

  \strong{Optional Components} \cr
  The following component is included in the result of 
  calling \code{\link{distChoose}} when the argument \code{keep.data=TRUE}:

  \item{data}{numeric vector containing the data actually used for 
    the goodness-of-fit tests (i.e., the original data without any 
    missing or infinite values).}
  \cr

  The following component is included in the result of 
  calling \code{\link{distChoose}} when missing (\code{NA}), 
  undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf}) 
  values are present:

  \item{bad.obs}{numeric scalar indicating the number of missing (\code{NA}), 
    undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf}) 
    values that were removed from the data object prior to choosing 
    a distribution.}
}
\note{
  Since objects of class \code{"distChoose"} are lists, you may extract 
  their components with the \code{$} and \code{[[} operators.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{distChoose}}, \code{\link{print.distChoose}},  
  \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests}, 
  \code{\link{Distribution.df}}.
}
\examples{
  # Create an object of class "distChoose", then print it out. 
  # (Note: the call to set.seed simply allows you to reproduce 
  # this example.)

  set.seed(47)
  dat <- rgamma(20, shape = 2, scale = 3)

  distChoose.obj <- distChoose(dat) 

  mode(distChoose.obj) 
  #[1] "list" 

  class(distChoose.obj) 
  #[1] "distChoose" 

  names(distChoose.obj) 
  #[1] "choices"                 "method"                 
  #[3] "decision"                "alpha"                  
  #[5] "distribution.parameters" "estimation.method"      
  #[7] "sample.size"             "test.results"           
  #[9] "data"                    "data.name"  

  distChoose.obj 
  
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

  #==========

  # Extract the choices
  #--------------------

  distChoose.obj$choices
  #[1] "Normal"    "Gamma"     "Lognormal"

  #==========

  # Clean up
  #---------
  rm(dat, distChoose.obj)
}
\keyword{classes}

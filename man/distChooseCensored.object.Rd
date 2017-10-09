\name{distChooseCensored.object}
\alias{distChooseCensored.object}
\docType{class}
\title{
  S3 Class "distChooseCensored"
}
\description{
  Objects of S3 class \code{"distChooseCensored"} are returned by the \pkg{EnvStats} function 
  \code{\link{distChooseCensored}}.
}
\details{
  Objects of S3 class \code{"distChooseCensored"} are lists that contain 
  information about the candidate distributions, the estimated distribution 
  parameters for each candidate distribution, and the test statistics and 
  p-values associated with each candidate distribution.
}
\section{Methods}{
  Generic functions that have methods for objects of class 
  \code{"distChooseCensored"} include: \cr
  \code{\link{print}}.
}
\value{
  \strong{Required Components} \cr
  The following components must be included in a legitimate list of 
  class \code{"distChooseCensored"}.

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
  \item{censoring.side}{character string indicating whether the data are 
    left- or right-censored.}
  \item{censoring.levels}{numeric scalar or vector indicating the censoring level(s).}
  \item{percent.censored}{numeric scalar indicating the percent of non-missing 
    observations that are censored.}
  \item{test.results}{a list with the same number of components as the number 
    of elements in the component \code{choices}.  The names of the list are the 
    distribution abbreviations of the candidate distributions.  
    (See the help file for \code{\link{Distribution.df}} for a list 
      of distributions and their abbreviations.)
    Each component is an object of class \code{\link[=gofCensored.object]{gofCensored}} 
    containing the results of the goodness-of-fit test for that particular 
    hypothesized distribution.}
  \item{data.name}{character string indicating the name of the data 
    object used for the goodness-of-fit tests.}
  \item{censoring.name}{character string indicating the name of the data object 
    used to identify which values are censored.}
  \cr

  \strong{Optional Components} \cr
  The following components are included in the result of 
  calling \code{\link{distChooseCensored}} when the argument \code{keep.data=TRUE}:

  \item{data}{numeric vector containing the data actually used for 
    the goodness-of-fit tests (i.e., the original data without any 
    missing or infinite values).}
  \item{censored}{logical vector containing the censoring status for the 
    data actually used for the goodness-of-fit tests 
   (i.e., the original data without any missing or infinite values).}
  \cr

  The following component is included in the result of 
  calling \code{\link{distChooseCensored}} when missing (\code{NA}), 
  undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf}) 
  values are present:

  \item{bad.obs}{numeric scalar indicating the number of missing (\code{NA}), 
    undefined (\code{NaN}) and/or infinite (\code{Inf}, \code{-Inf}) 
    values that were removed from the data object prior to choosing 
    a distribution.}
}
\note{
  Since objects of class \code{"distChooseCensored"} are lists, you may extract 
  their components with the \code{$} and \code{[[} operators.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{distChooseCensored}}, \code{\link{print.distChooseCensored}},  
  \link[=FcnsByCatCensoredData]{Censored Data}, 
  \link[=FcnsByCatGOFTests]{Goodness-of-Fit Tests}, 
  \code{\link{Distribution.df}}.
}
\examples{
  # Create an object of class "distChooseCensored", then print it out. 
  # (Note: the call to set.seed simply allows you to reproduce 
  # this example.)

  set.seed(598)

  dat <- rgammaAlt(30, mean = 10, cv = 1)
  censored <- dat < 5
  dat[censored] <- 5

  distChooseCensored.obj <- distChooseCensored(dat, censored, 
    method = "sw", choices = c("norm", "gammaAlt", "lnormAlt"))

  mode(distChooseCensored.obj) 
  #[1] "list" 

  class(distChooseCensored.obj) 
  #[1] "distChooseCensored" 

  names(distChooseCensored.obj) 
  # [1] "choices"                 "method"                 
  # [3] "decision"                "alpha"                  
  # [5] "distribution.parameters" "estimation.method"      
  # [7] "sample.size"             "censoring.side"         
  # [9] "censoring.levels"        "percent.censored"       
  #[11] "test.results"            "data"                   
  #[13] "censored"                "data.name"              
  #[15] "censoring.name" 

  distChooseCensored.obj 
  
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

  #==========

  # Extract the choices
  #--------------------

  distChooseCensored.obj$choices
  #[1] "Normal"    "Gamma"     "Lognormal"

  #==========

  # Clean up
  #---------
  rm(dat, censored, distChooseCensored.obj)
}
\keyword{classes}

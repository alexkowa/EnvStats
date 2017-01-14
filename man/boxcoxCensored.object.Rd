\name{boxcoxCensored.object}
\alias{boxcoxCensored.object}
\docType{class}
\title{
  S3 Class "boxcoxCensored"
}
\description{
  Objects of S3 class \code{"boxcoxCensored"} are returned by the \pkg{EnvStats} 
  function \code{\link{boxcoxCensored}}, which computes objective values for 
  user-specified powers, or computes the optimal power for the specified 
  objective, based on Type I censored data. 
}
\details{
  Objects of class \code{"boxcoxCensored"} are lists that contain 
  information about the powers that were used, the objective that was used, 
  the values of the objective for the given powers, and whether an 
  optimization was specified.
}
\section{Methods}{
  Generic functions that have methods for objects of class 
  \code{"boxcoxCensored"} include: \cr
  \code{\link{plot}}, \code{\link{print}}.
}
\value{
  \strong{Required Components} \cr
  The following components must be included in a legitimate list of 
  class \code{"boxcoxCensored"}.

  \item{lambda}{
    Numeric vector containing the powers used in the Box-Cox transformations.  
    If the value of the \code{optimize} component is \code{FALSE}, then 
    \code{lambda} contains the values of all of the powers at which the objective 
    was evaluated.  If the value of the \code{optimize} component is \code{TRUE}, 
    then \code{lambda} is a scalar containing the value of the power that 
    maximizes the objective.}
  \item{objective}{
    Numeric vector containing the value(s) of the objective for the given value(s) 
    of \eqn{\lambda} that are stored in the component \code{lambda}.}
  \item{objective.name}{
    Character string indicating the objective that was used. The possible values are
    \code{"PPCC"} (probability plot correlation coefficient; the default), 
    \code{"Shapiro-Wilk"} (the Shapiro-Wilk goodness-of-fit statistic), and 
    \code{"Log-Likelihood"} (the log-likelihood function).}
  \item{optimize}{
    Logical scalar indicating whether the objective was simply evaluted at the 
    given values of \code{lambda} (\code{optimize=FALSE}), or instead  
    the optimal power transformation was computed within the bounds specified by 
    \code{lambda} (\code{optimize=TRUE}).}
  \item{optimize.bounds}{
    Numeric vector of length 2 with a names attribute indicating the bounds within 
    which the optimization took place.  When \code{optimize=FALSE}, this contains 
    missing values.}
  \item{eps}{
    Finite, positive numeric scalar indicating what value of \code{eps} was used.  
    When the absolute value of \code{lambda} is less 
    than \code{eps}, lambda is assumed to be 0 for the Box-Cox transformation.}
  \item{sample.size}{
    Numeric scalar indicating the number of finite, non-missing observations.}
  \item{censoring.side}{
    Character string indicating the censoring side.  Possible values are 
    \code{"left"} and \code{"right"}.}
  \item{censoring.levels}{
    Numeric vector containing the censoring levels.}
  \item{percent.censored}{Numeric scalar indicating the percent of observations 
    that are censored.}
  \item{data.name}{
    The name of the data object used for the Box-Cox computations.}
  \item{censoring.name}{
    The name of the data object indicating which observations are censored.}
  \item{bad.obs}{
    The number of missing (\code{NA}), undefined (\code{NaN}) and/or infinite 
    (\code{Inf}, \code{-Inf}) values that were removed from the data object 
    prior to performing the Box-Cox computations.} \cr

  \strong{Optional Component} \cr
  The following components may optionally be included in a legitimate 
  list of class \code{"boxcoxCensored"}.  They must be included if you want to 
  call the function \code{\link{plot.boxcoxCensored}} and specify Q-Q plots or 
  Tukey Mean-Difference Q-Q plots.

  \item{data}{
    Numeric vector containing the data actually used for the Box-Cox 
    computations (i.e., the original data without any missing or infinite values).}
  \item{censored}{
    Logical vector indicating which of the vales in the component \code{data} 
    are censored.}
}
\note{
  Since objects of class \code{"boxcoxCensored"} are lists, you may extract 
  their components with the \code{$} and \code{[[} operators.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{boxcoxCensored}}, \code{\link{plot.boxcoxCensored}}, 
  \code{\link{print.boxcoxCensored}}.
}
\examples{
  # Create an object of class "boxcoxCensored", then print it out.
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 

  x.1 <- rlnormAlt(15, mean = 10, cv = 2) 
  censored.1 <- x.1 < 2
  x.1[censored.1] <- 2

  x.2 <- rlnormAlt(15, mean = 10, cv = 2) 
  censored.2 <- x.2 < 4
  x.2[censored.2] <- 4

  x <- c(x.1, x.2)
  censored <- c(censored.1, censored.2)

  boxcox.list <- boxcoxCensored(x, censored)

  data.class(boxcox.list)
  #[1] "boxcoxCensored"
  
  names(boxcox.list)
  # [1] "lambda"           "objective"        "objective.name"  
  # [4] "optimize"         "optimize.bounds"  "eps"             
  # [7] "data"             "censored"         "sample.size"     
  #[10] "censoring.side"   "censoring.levels" "percent.censored"
  #[13] "data.name"        "censoring.name"   "bad.obs"

  boxcox.list

  #Results of Box-Cox Transformation
  #Based on Type I Censored Data
  #---------------------------------
  #
  #Objective Name:                  PPCC
  #
  #Data:                            x
  #
  #Censoring Variable:              censored
  #
  #Censoring Side:                  left
  #
  #Censoring Level(s):              2 4 
  #
  #Sample Size:                     30
  #
  #Percent Censored:                26.7%
  #
  # lambda      PPCC
  #   -2.0 0.8954683
  #   -1.5 0.9338467
  #   -1.0 0.9643680
  #   -0.5 0.9812969
  #    0.0 0.9776834
  #    0.5 0.9471025
  #    1.0 0.8901990
  #    1.5 0.8187488
  #    2.0 0.7480494

  boxcox.list2 <- boxcox(x, optimize = TRUE) 
  names(boxcox.list2)
  # [1] "lambda"          "objective"       "objective.name" 
  # [4] "optimize"        "optimize.bounds" "eps"            
  # [7] "data"            "sample.size"     "data.name"      
  #[10] "bad.obs"

  boxcox.list2
  #Results of Box-Cox Transformation
  #---------------------------------
  #
  #Objective Name:                  PPCC
  #
  #Data:                            x
  #
  #Sample Size:                     30
  #
  #Bounds for Optimization:         lower = -2
  #                                 upper =  2
  #
  #Optimal Value:                   lambda = -0.5826431
  #
  #Value of Objective:              PPCC = 0.9755402

  #==========

  # Clean up
  #---------
  rm(x.1, censored.1, x.2, censored.2, x, censored, boxcox.list, boxcox.list2)
}
\keyword{classes}


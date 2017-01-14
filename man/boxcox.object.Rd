\name{boxcox.object}
\alias{boxcox.object}
\docType{class}
\title{
  S3 Class "boxcox"
}
\description{
  Objects of S3 class \code{"boxcox"} are returned by the \pkg{EnvStats} 
  function \code{\link{boxcox}}, which computes objective values for 
  user-specified powers, or computes the optimal power for the specified 
  objective. 
}
\details{
  Objects of class \code{"boxcox"} are lists that contain 
  information about the powers that were used, the objective that was used, 
  the values of the objective for the given powers, and whether an 
  optimization was specified.
}
\section{Methods}{
  Generic functions that have methods for objects of class 
  \code{"boxcox"} include: \cr
  \code{\link{plot}}, \code{\link{print}}.
}
\value{
  \strong{Required Components} \cr
  The following components must be included in a legitimate list of 
  class \code{"boxcox"}.

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
    character string indicating the objective that was used. The possible values are
    \code{"PPCC"} (probability plot correlation coefficient; the default), 
    \code{"Shapiro-Wilk"} (the Shapiro-Wilk goodness-of-fit statistic), and 
    \code{"Log-Likelihood"} (the log-likelihood function).}
  \item{optimize}{
    logical scalar indicating whether the objective was simply evaluted at the 
    given values of \code{lambda} (\code{optimize=FALSE}), or instead  
    the optimal power transformation was computed within the bounds specified by 
    \code{lambda} (\code{optimize=TRUE}).}
  \item{optimize.bounds}{
    Numeric vector of length 2 with a names attribute indicating the bounds within 
    which the optimization took place.  When \code{optimize=FALSE}, this contains 
    missing values.}
  \item{eps}{
    finite, positive numeric scalar indicating what value of \code{eps} was used.  
    When the absolute value of \code{lambda} is less 
    than \code{eps}, lambda is assumed to be 0 for the Box-Cox transformation.}
  \item{sample.size}{
    Numeric scalar indicating the number of finite, non-missing observations.}
  \item{data.name}{
    The name of the data object used for the Box-Cox computations.}
  \item{bad.obs}{
    The number of missing (\code{NA}), undefined (\code{NaN}) and/or infinite 
    (\code{Inf}, \code{-Inf}) values that were removed from the data object 
    prior to performing the Box-Cox computations.} \cr

  \strong{Optional Component} \cr
  The following component may optionally be included in a legitimate 
  list of class \code{"boxcox"}.  It must be included if you want to call the 
  function \code{\link{plot.boxcox}} and specify Q-Q plots or 
  Tukey Mean-Difference Q-Q plots.

  \item{data}{
    Numeric vector containing the data actually used for the Box-Cox 
    computations (i.e., the original data without any missing or infinite values).}
}
\note{
  Since objects of class \code{"boxcox"} are lists, you may extract 
  their components with the \code{$} and \code{[[} operators.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{boxcox}}, \code{\link{plot.boxcox}}, \code{\link{print.boxcox}}, 
  \code{\link{boxcoxLm.object}}.
}
\examples{
  # Create an object of class "boxcox", then print it out.
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  x <- rlnormAlt(30, mean = 10, cv = 2) 

  dev.new()
  hist(x, col = "cyan")

  boxcox.list <- boxcox(x)

  data.class(boxcox.list)
  #[1] "boxcox"
  
  names(boxcox.list)
  # [1] "lambda"          "objective"       "objective.name" 
  # [4] "optimize"        "optimize.bounds" "eps"            
  # [7] "data"            "sample.size"     "data.name"      
  #[10] "bad.obs" 

  boxcox.list
  #Results of Box-Cox Transformation
  #---------------------------------
  #
  #Objective Name:                  PPCC
  #
  #Data:                            x
  #
  #Sample Size:                     30
  #
  # lambda      PPCC
  #   -2.0 0.5423739
  #   -1.5 0.6402782
  #   -1.0 0.7818160
  #   -0.5 0.9272219
  #    0.0 0.9921702
  #    0.5 0.9581178
  #    1.0 0.8749611
  #    1.5 0.7827009
  #    2.0 0.7004547

  boxcox(x, optimize = TRUE) 
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
  #Optimal Value:                   lambda = 0.04530789
  #
  #Value of Objective:              PPCC = 0.9925919 

  #----------

  # Clean up
  #---------
  rm(x, boxcox.list)
}
\keyword{classes}


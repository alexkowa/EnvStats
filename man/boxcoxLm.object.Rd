\name{boxcoxLm.object}
\alias{boxcoxLm.object}
\docType{class}
\title{
  S3 Class "boxcoxLm"
}
\description{
  Objects of S3 class \code{"boxcoxLm"} are returned by the \pkg{EnvStats} 
  function \code{\link{boxcox}} when the argument \code{x} is an object 
  of class \code{"lm"}.  In this case, \code{\link{boxcox}} computes 
  values of an objective function for user-specified powers, or computes the 
  optimal power for the specified objective, based on residuals from the linear model. 
}
\details{
  Objects of class \code{"boxcoxLm"} are lists that contain 
  information about the \code{"lm"} object that was suplied, 
  the powers that were used, the objective that was used, 
  the values of the objective for the given powers, and whether an 
  optimization was specified.
}
\section{Methods}{
  Generic functions that have methods for objects of class 
  \code{"boxcoxLm"} include: \cr
  \code{\link{plot}}, \code{\link{print}}.
}
\value{
  The following components must be included in a legitimate list of 
  class \code{"boxcoxLm"}.

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
  \item{lm.obj}{
    the value of the argument \code{x} provided to \code{\link{boxcox}} 
    (an object that must inherit from class \code{"lm"}).}
  \item{sample.size}{
    Numeric scalar indicating the number of finite, non-missing observations.}
  \item{data.name}{
    The name of the data object used for the Box-Cox computations.}
}
\note{
  Since objects of class \code{"boxcoxLm"} are lists, you may extract 
  their components with the \code{$} and \code{[[} operators.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{boxcox}}, \code{\link{plot.boxcoxLm}}, \code{\link{print.boxcoxLm}}, 
  \code{\link{boxcox.object}}.
}
\examples{
  # Create an object of class "boxcoxLm", then print it out.

  # The data frame Environmental.df contains daily measurements of 
  # ozone concentration, wind speed, temperature, and solar radiation
  # in New York City for 153 consecutive days between May 1 and 
  # September 30, 1973.  In this example, we'll plot ozone vs. 
  # temperature and look at the Q-Q plot of the residuals.  Then 
  # we'll look at possible Box-Cox transformations.  The "optimal" one 
  # based on the PPCC looks close to a log-transformation 
  # (i.e., lambda=0).  The power that produces the largest PPCC is 
  # about 0.2, so a cube root (lambda=1/3) transformation might work too.

  # Fit the model with the raw Ozone data
  #--------------------------------------
  ozone.fit <- lm(ozone ~ temperature, data = Environmental.df) 

  # Plot Ozone vs. Temperature, with fitted line 
  #---------------------------------------------
  dev.new()
  with(Environmental.df, 
    plot(temperature, ozone, xlab = "Temperature (degrees F)",
      ylab = "Ozone (ppb)", main = "Ozone vs. Temperature"))
  abline(ozone.fit) 

  # Look at the Q-Q Plot for the residuals 
  #---------------------------------------
  dev.new()
  qqPlot(ozone.fit$residuals, add.line = TRUE) 

  # Look at Box-Cox transformations of Ozone 
  #-----------------------------------------
  boxcox.list <- boxcox(ozone.fit) 
  boxcox.list 
  #Results of Box-Cox Transformation
  #---------------------------------
  #
  #Objective Name:                  PPCC
  #
  #Linear Model:                    ozone.fit
  #
  #Sample Size:                     116
  #
  # lambda      PPCC
  #   -2.0 0.4286781
  #   -1.5 0.4673544
  #   -1.0 0.5896132
  #   -0.5 0.8301458
  #    0.0 0.9871519
  #    0.5 0.9819825
  #    1.0 0.9408694
  #    1.5 0.8840770
  #    2.0 0.8213675

  #----------

  # Clean up
  #---------
  rm(ozone.fit, boxcox.list)
}
\keyword{classes}


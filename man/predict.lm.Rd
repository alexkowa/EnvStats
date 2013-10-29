\name{predict.lm}
\alias{predict.lm}
\title{
  Predict method for Linear Model Fits
}
\description{
  Predicted values based on linear model object.  This function is a modified version 
  of the built-in \R function \code{\link[stats:predict.lm]{predict.lm}}.  
  The \pkg{EnvStats} function \code{predict.lm} returns a component 
  called \code{n.coefs} when the argument \code{se.fit=TRUE}.  The component 
  \code{n.coefs} is used by the function \code{\link{pointwise}} to create 
  simultaneous confidence or prediction limits.
}
\usage{
\method{predict}{lm}(object, newdata, se.fit = FALSE, scale = NULL, df = Inf, 
  interval = c("none", "confidence", "prediction"), 
  level = 0.95, type = c("response", "terms"), 
  terms = NULL, na.action = na.pass, 
  pred.var = NULL, weights = 1, ...)
}
\arguments{
  \item{object}{
  Object of class \code{"lm"} or a class that inherits from \code{"lm"}
}
  \item{newdata}{
  An optional data frame in which to look for variables with which to predict.  
  If omitted, the fitted values are used.
}
  \item{se.fit}{
  A logical scalar indicating whether to compute standard errors.  The default value is 
  \code{se.fit=FALSE}.
}
  \item{scale}{
  Scale parameter for std.err. calculation.
}
  \item{df}{
  Degrees of freedom for \code{scale}.
}
  \item{interval}{
  Type of interval calculation.  Possible values are \code{"none"} (the default), \cr
  \code{"confidence"}, and \code{"prediction"}.
}
  \item{level}{
  Tolerance/confidence level.
}
  \item{type}{
  Type of prediction (response or model term).
}
  \item{terms}{
  If \code{type="terms"}, this argument determines which terms (the default is all terms).
}
  \item{na.action}{
  A function determining what should be done with missing values in \code{newdata}.  
  The default is to predict \code{NA}.
}
  \item{pred.var}{
  The variance(s) for future observations to be assumed for prediction intervals.  
  See \sQuote{Details}.
}
  \item{weights}{
  The variance weights for prediction.  This can be a numeric vector or a one-sided model formula.  
  In the latter case, it is interpreted as an expression evaluated in \code{newdata}.
}
  \item{\dots}{
  Further arguments passed to or from other methods.
}
}
\details{
  See the \R help file for \code{\link[stats:predict.lm]{predict.lm}}.  
}
\value{
  \code{predict.lm} produces a vector of predictions or a matrix of predictions and bounds with 
  column names \code{fit}, \code{lwr}, and \code{upr} if \code{interval} is set.  
  If \code{se.fit} is \code{TRUE}, a list with the following components is returned: 
  \item{fit}{vector or matrix as above}
  \item{se.fit}{standard error of predicted means}
  \item{residual.scale}{residual standard deviations}
  \item{df}{degrees of freedom for residual}
  \item{n.coefs}{numeric scalar denoting the number of predictor variables used in the model}
}
\references{
  Chambers, J.M., and Hastie, T.J., eds. (1992). \emph{Statistical Models in S}. 
  Chapman and Hall/CRC, Boca Raton, FL.

  Draper, N., and H. Smith. (1998). \emph{Applied Regression Analysis}. 
  Third Edition. John Wiley and Sons, New York, Chapter 3.

  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton, FL, pp.546-553.

  Miller, R.G. (1981a). \emph{Simultaneous Statistical Inference}. 
  Springer-Verlag, New York, pp.111, 124.
}
\author{
  R Development Core Team (for code for \R version of \code{predict.lm}).
 
  Steven P. Millard (for modification to add compenent \code{n.coefs}; \email{EnvStats@ProbStatInfo.com})
}
\note{
  See the \R help file for \code{\link[stats:predict.lm]{predict.lm}}.

  Almost always the process of determining the concentration of a chemical in a soil, water, or air 
  sample involves using some kind of machine that produces a signal, and this signal is related to 
  the concentration of the chemical in the physical sample.  The process of relating the machine 
  signal to the concentration of the chemical is called \emph{calibration} (see \code{\link{calibrate}}).  
  Once calibration has been performed, estimated concentrations in physical samples with unknown 
  concentrations are computed using inverse regression.  The uncertainty in the process used to estimate 
  the concentration may be quantified with decision, detection, and quantitation limits.

  In practice, only the point estimate of concentration is reported (along with a possible qualifier), 
  without confidence bounds for the true concentration \eqn{C}.  This is most unfortunate because it 
  gives the impression that there is no error associated with the reported concentration.  Indeed, both 
  the International Organization for Standardization (ISO) and the International Union of Pure and 
  Applied Chemistry (IUPAC) recommend always reporting both the estimated concentration and the 
  uncertainty associated with this estimate (Currie, 1997).
}
\seealso{
  \R help file for \code{\link[stats:predict.lm]{predict.lm}}, 
  \code{\link{predict}}, \code{\link{lm}}, \code{\link{calibrate}}, 
  \code{\link{calibrate}}, \code{\link{inversePredictCalibrate}}, 
  \code{\link{detectionLimitCalibrate}}.

  \link{SafePrediction} for prediction from polynomial and spline fits.
}
\examples{
  # Using the data from the built-in data frame Air.df, 
  # fit the cube-root of ozone as a function of temperature, 
  # then compute predicted values for ozone at 70 and 90 degrees F,
  # along with the standard errors of these predicted values.

  # First look at the data
  #-----------------------
  attach(Air.df)

  plot(temperature, ozone, xlab = "Temperature (degrees F)", 
    ylab = "Cube-Root Ozone (ppb)")


  # Now create the lm object 
  #-------------------------
  ozone.fit <- lm(ozone ~ temperature, data = Air.df) 


  # Now get predicted values and CIs at 70 and 90 degrees.
  # Note the presence of the last component called n.coefs.
  #--------------------------------------------------------
  predict.list <- predict(ozone.fit, 
    newdata = data.frame(temperature = c(70, 90)), se.fit = TRUE) 

  predict.list
  #$fit
  #       1        2 
  #2.697810 4.101808 
  #
  #$se.fit
  #         1          2 
  #0.07134554 0.08921071 
  #
  #$df
  #[1] 114
  #
  #$residual.scale
  #[1] 0.5903046
  #
  #$n.coefs
  #[1] 2

 
  #----------

  #Continuing with the above example, create a scatterplot of 
  # cube-root ozone vs. temperature, and add the fitted line 
  # along with simultaneous 95\% confidence bands.

  plot(temperature, ozone, xlab = "Temperature (degrees F)", 
    ylab = "Cube-Root Ozone (ppb)")

  abline(ozone.fit, lwd = 3, col = "blue")

  new.temp <- seq(min(temperature), max(temperature), length = 100)

  predict.list <- predict(ozone.fit, 
    newdata = data.frame(temperature = new.temp), 
    se.fit = TRUE)

  ci.ozone <- pointwise(predict.list, coverage = 0.95, 
    simultaneous = TRUE)

  lines(new.temp, ci.ozone$lower, lty = 2, lwd = 3, col = "magenta") 

  lines(new.temp, ci.ozone$upper, lty = 2, lwd = 3, col = "magenta") 

  title(main=paste("Scatterplot of Cube-Root Ozone vs. Temperature", 
    "with Fitted Line and Simultaneous 95\% Confidence Bands", 
    sep="\n"))


  #----------

  # Clean up

  rm(ozone.fit, predict.list, new.temp, ci.ozone)
  detach("Air.df") 

  #----------------------------------------------------------------

  # Examples from the R help file for predict.lm:

  require(graphics)

  ## Predictions
  x <- rnorm(15)
  y <- x + rnorm(15)
  predict(lm(y ~ x))
  new <- data.frame(x = seq(-3, 3, 0.5))
  predict(lm(y ~ x), new, se.fit = TRUE)
  pred.w.plim <- predict(lm(y ~ x), new, interval="prediction")
  pred.w.clim <- predict(lm(y ~ x), new, interval="confidence")
  matplot(new$x,cbind(pred.w.clim, pred.w.plim[,-1]),
          lty=c(1,2,2,3,3), type="l", ylab="predicted y")

  ## Prediction intervals, special cases
  ##  The first three of these throw warnings
  w <- 1 + x^2
  fit <- lm(y ~ x)
  wfit <- lm(y ~ x, weights = w)
  predict(fit, interval = "prediction")
  predict(wfit, interval = "prediction")
  predict(wfit, new, interval = "prediction")
  predict(wfit, new, interval = "prediction", weights = (new$x)^2)
  predict(wfit, new, interval = "prediction", weights = ~x^2)
}
\keyword{ models }
\keyword{ regression }

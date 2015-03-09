\name{pointwise}
\alias{pointwise}
\title{
  Pointwise Confidence Limits for Predictions
}
\description{
  Computes pointwise confidence limits for predictions computed by the function 
  \code{\link{predict}}.
}
\usage{
  pointwise(results.predict, coverage = 0.99, 
    simultaneous = FALSE, individual = FALSE)
}
\arguments{
  \item{results.predict}{
  output from a call to \code{\link{predict}} with \code{se.fit=TRUE}.
}
  \item{coverage}{
  optional numeric scalar between 0 and 1 indicating the confidence level associated with the confidence limits. 
  The default value is \code{coverage=0.99}.
}
  \item{simultaneous}{
  optional logical scalar indicating whether to base the confidence limits for the 
  predicted values on simultaneous or non-simultaneous prediction limits. 
  The default value is \code{simultaneous=FALSE}.
}
  \item{individual}{
  optional logical scalar indicating whether to base the confidence intervals for the 
  predicted values on prediction limits for the mean (\code{individual=FALSE}) or 
  prediction limits for an individual observation (\code{individual=TRUE}). 
  The default value is \code{individual=FALSE}.
}
}
\details{
  This function computes pointwise confidence limits for predictions computed by the 
  function \cr
  \code{\link{predict}}. The limits are computed at those points specified by the argument 
  \code{newdata} of \code{\link{predict}}.

  The \code{\link{predict}} function is a generic function with methods for several 
  different classes.  The funciton \code{pointwise} was part of the S language.  
  The modifications to \code{pointwise} in the package \pkg{EnvStats} involve confidence 
  limits for predictions for a linear model (i.e., an object of class \code{"lm"}).

  \strong{Confidence Limits for a Predicted Mean Value (\code{individual=FALSE})}.  
  Consider a standard linear model with \eqn{p} predictor variables.  
  Often, one of the major goals of regression analysis is to predict a future 
  value of the response variable given known values of the predictor variables. 
  The equations for the predicted mean value of the response given  
  fixed values of the predictor variables as well as the equation for a 
  two-sided (1-\eqn{\alpha})100\% confidence interval for the mean value of the 
  response can be found in Draper and Smith (1998, p.80) and 
  Millard and Neerchal (2001, p.547).

  Technically, this formula is a confidence interval for the mean of 
  the response for one set of fixed values of the predictor variables and 
  corresponds to the case when \code{simultaneous=FALSE}. To create simultaneous 
  confidence intervals over the range of of the predictor variables, 
  the critical t-value in the equation has to be replaced with a critical 
  F-value and the modified formula is given in Draper and Smith (1998, p. 83), 
  Miller (1981a, p. 111), and Millard and Neerchal (2001, p. 547).  
  This formula is used in the case when \code{simultaneous=TRUE}.

  \strong{Confidence Limits for a Predicted Individual Value (\code{individual=TRUE})}.  
  In the above section we discussed how to create a confidence interval for 
  the mean of the response given fixed values for the predictor variables.  
  If instead we want to create a prediction interval for a single 
  future observation of the response variable, the fomula is given in 
  Miller (1981a, p. 115) and Millard and Neerchal (2001, p. 551).

  Technically, this formula is a prediction interval for a single future 
  observation for one set of fixed values of the predictor variables and 
  corresponds to the case when \code{simultaneous=FALSE}.  Miller (1981a, p. 115) 
  gives a formula for simultaneous prediction intervals for \eqn{k} future 
  observations. If we are interested in creating an interval that will 
  encompass \emph{all} possible future observations over the range of the 
  preictor variables with some specified probability however, we need to 
  create simultaneous tolerance intervals.  A formula for such an interval 
  was developed by Lieberman and Miller (1963) and is given in 
  Miller (1981a, p. 124).  This formula is used in the case when 
  \code{simultaneous=TRUE}.
}
\value{
  a list with the following components:
  \item{upper}{upper limits of pointwise confidence intervals.}
  \item{fit}{surface values.  This is the same as the component \code{fit} of the argument \cr 
    \code{results.predict}.}
  \item{lower}{lower limits of pointwise confidence intervals.}
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
  Authors of S (for code for \code{pointwise} in S).
 
  Steven P. Millard (for modification to allow the arguments \code{simultaneous} and \code{individual}); \cr
  \email{EnvStats@ProbStatInfo.com})
}
\note{
  The function \code{pointwise} is called by the functions 
  \code{\link{detectionLimitCalibrate}} and \cr
  \code{\link{inversePredictCalibrate}}, which are used in \strong{calibration}. 

  Almost always the process of determining the concentration of a chemical in 
  a soil, water, or air sample involves using some kind of machine that 
  produces a signal, and this signal is related to the concentration of the 
  chemical in the physical sample. The process of relating the machine signal 
  to the concentration of the chemical is called \strong{calibration} 
  (see \code{\link{calibrate}}). Once calibration has been performed, 
  estimated concentrations in physical samples with unknown concentrations 
  are computed using inverse regression.  The uncertainty in the process used 
  to estimate the concentration may be quantified with decision, detection, 
  and quantitation limits.

  In practice, only the point estimate of concentration is reported (along 
  with a possible qualifier), without confidence bounds for the true 
  concentration \eqn{C}. This is most unfortunate because it gives the 
  impression that there is no error associated with the reported concentration. 
  Indeed, both the International Organization for Standardization (ISO) and 
  the International Union of Pure and Applied Chemistry (IUPAC) recommend 
  always reporting both the estimated concentration and the uncertainty 
  associated with this estimate (Currie, 1997).
}
\seealso{
  \code{\link{predict}}, \code{\link{predict.lm}}, 
  \code{\link{lm}}, \code{\link{calibrate}}, 
  \code{\link{inversePredictCalibrate}}, \code{\link{detectionLimitCalibrate}}.
}
\examples{
  # Using the data in the built-in data frame Air.df, 
  # fit the cube root of ozone as a function of temperature. 
  # Then compute predicted values for ozone at 70 and 90 
  # degrees F, and compute 95\% confidence intervals for the 
  # mean value of ozone at these temperatures.

  # First create the lm object 
  #---------------------------

  ozone.fit <- lm(ozone ~ temperature, data = Air.df) 


  # Now get predicted values and CIs at 70 and 90 degrees 
  #------------------------------------------------------

  predict.list <- predict(ozone.fit, 
    newdata = data.frame(temperature = c(70, 90)), se.fit = TRUE) 

  pointwise(predict.list, coverage = 0.95) 
  # $upper
  #        1        2 
  # 2.839145 4.278533 

  # $fit
  #        1        2 
  # 2.697810 4.101808 

  # $lower
  #        1        2 
  # 2.556475 3.925082 

  #--------------------------------------------------------------------

  # Continuing with the above example, create a scatterplot of ozone 
  # vs. temperature, and add the fitted line along with simultaneous 
  # 95% confidence bands.

  x <- Air.df$temperature 

  y <- Air.df$ozone 

  dev.new()
  plot(x, y, xlab="Temperature (degrees F)",  
    ylab = expression(sqrt("Ozone (ppb)", 3))) 

  abline(ozone.fit, lwd = 2) 

  new.x <- seq(min(x), max(x), length=100) 

  predict.ozone <- predict(ozone.fit, 
    newdata = data.frame(temperature = new.x), se.fit = TRUE) 

  ci.ozone <- pointwise(predict.ozone, coverage=0.95, 
    simultaneous=TRUE) 

  lines(new.x, ci.ozone$lower, lty=2, lwd = 2, col = 2) 

  lines(new.x, ci.ozone$upper, lty=2, lwd = 2, col = 2) 

  title(main=paste("Cube Root Ozone vs. Temperature with Fitted Line", 
    "and Simultaneous 95\% Confidence Bands", 
    sep="\n")) 

  #--------------------------------------------------------------------

  # Redo the last example by creating non-simultaneous 
  # confidence bounds and prediction bounds as well.

  dev.new()
  plot(x, y, xlab = "Temperature (degrees F)", 
    ylab = expression(sqrt("Ozone (ppb)", 3))) 

  abline(ozone.fit, lwd = 2) 

  new.x <- seq(min(x), max(x), length=100) 

  predict.ozone <- predict(ozone.fit, 
    newdata = data.frame(temperature = new.x), se.fit = TRUE) 

  ci.ozone <- pointwise(predict.ozone, coverage=0.95) 

  lines(new.x, ci.ozone$lower, lty=2, col = 2, lwd = 2) 

  lines(new.x, ci.ozone$upper, lty=2, col = 2, lwd = 2) 

  pi.ozone <- pointwise(predict.ozone, coverage = 0.95, 
    individual = TRUE)

  lines(new.x, pi.ozone$lower, lty=4, col = 4, lwd = 2) 

  lines(new.x, pi.ozone$upper, lty=4, col = 4, lwd = 2) 

  title(main=paste("Cube Root Ozone vs. Temperature with Fitted Line", 
    "and 95\% Confidence and Prediction Bands", 
    sep="\n")) 

  #--------------------------------------------------------------------

  # Clean up
  rm(predict.list, ozone.fit, x, y, new.x, predict.ozone, ci.ozone, 
    pi.ozone)
}
\keyword{ models }
\keyword{ regression }

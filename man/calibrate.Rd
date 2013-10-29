\name{calibrate}
\alias{calibrate}
\title{Fit a Calibration Line or Curve}
\description{
  Fit a calibration line or curve based on linear regression.
}
\usage{
  calibrate(formula, data, max.order = 4, p.crit = 0.05, weights, 
    subset, na.action, method = "qr", model = FALSE, x = FALSE, 
    y = FALSE, contrasts = NULL, ...)
}
\arguments{
  \item{formula}{
  a \code{\link{formula}} object, with the response on the left of a \code{~} operator, and 
  the single predictor variable on the right.  For example, \code{Cadmium ~ Spike}.
}
  \item{data}{
  an optional data frame, list or environment (or object coercible by \cr
  \code{\link{as.data.frame}} to a data frame) containing the variables in 
  the model.  If not found in \code{data}, the variables are taken from 
  \code{environment(formula)}, typically the environment from which 
  \code{calibrate} is called.
}
  \item{max.order}{
  optional integer indicating the maximum order of the polynomial to consider for the calibration curve. 
  The default value is \code{max.order=4}.
}
  \item{p.crit}{
  optional numeric scaler between 0 and 1 indicating the p-value to use for the stepwise regression 
  when determining which polynomial model to use.  The default value is \code{p.crit=0.05}.
}
  \item{weights}{
  optional vector of observation weights; if supplied, the algorithm fits to minimize the sum of the 
  weights multiplied into the squared residuals.  The length of weights must be the same as 
  the number of observations.  The weights must be nonnegative and it is strongly recommended 
  that they be strictly positive, since zero weights are ambiguous, compared to use of the 
  \code{subset} argument.
}
  \item{subset}{
  optional expression saying which subset of the rows of the data should be used in the fit. 
  This can be a logical vector (which is replicated to have length equal to the number of 
  observations), or a numeric vector indicating which observation numbers are to be included, 
  or a character vector of the row names to be included.  All observations are included by 
  default.
}
  \item{na.action}{
  optional function which indicates what should happen when the data contain \code{NAs}. 
  The default is set by the \code{na.action} setting of \code{\link{options}}, and is \cr
  \code{\link{na.fail}} if that is unset.  The \sQuote{factory-fresh} default is 
  \code{\link{na.omit}}.  Another possible value is \code{NULL}, no action. 
  Value \code{\link{na.exclude}} can be useful.
}
  \item{method}{
  optional method to be used; for fitting, currently only \code{method = "qr"} is supported; 
  \code{method = "model.frame"} returns the model frame (the same as with 
  \code{model = TRUE}, see below).
}
  \item{model, x, y, qr}{
  optional logicals. If \code{TRUE} the corresponding components of the fit (the model frame, 
  the model matrix, the response, the QR decomposition) are returned.
}
  \item{contrasts}{
  an optional list. See the argument \code{contrasts.arg} of \code{\link{model.matrix}}.
}
  \item{\dots}{
  additional arguments to be passed to the low level regression fitting functions 
  (see \code{\link{lm}}).
}
}
\details{
  A simple and frequently used calibration model is a straight line where the response variable 
  \emph{S} denotes the signal of the machine and the predictor variable \emph{C} denotes the 
  true concentration in the physical sample.  The error term is assumed to follow a normal distribution 
  with mean 0.  Note that the average value of the signal for a blank (\emph{C} = 0) is the intercept.  
  Other possible calibration models include higher order polynomial models such as a quadratic or 
  cubic model.

  In a typical setup, a small number of samples (e.g., n = 6) with known concentrations are measured 
  and the signal is recorded.  A sample with no chemical in it, called a blank, is also measured.  
  (You have to be careful to define exactly what you mean by a \dQuote{blank.}  A blank could mean 
  a container from the lab that has nothing in it but is prepared in a similar fashion to containers 
  with actual samples in them.  Or it could mean a field blank: the container was taken out to the 
  field and subjected to the same process that all other containers were subjected to, except a physical 
  sample of soil or water was not placed in the container.)  Usually, replicate measures at the same 
  known concentrations are taken.  (The term \dQuote{replicate} must be well defined to distinguish 
  between for example the same physical samples that are measured more than once vs. two different 
  physical samples of the same known concentration.)

  The function \code{calibrate} initially fits a linear calibration model.  If the argument 
  \code{max.order} is greater than 1, \code{calibrate} then performs forward stepwise linear 
  regression to determine the \dQuote{best} polynomial model.

  In the case where replicates are available, for each model calibrate computes the p-value of the 
  ANOVA for lack-of-fit vs. pure error (Draper and Smith, 1998, Chapters 2).  If the p-value is 
  greater than or equal to \code{p.crit}, then this is the final model; otherwise the next 
  higher-order term is added to the polynomial and the model is re-fit.

  In the case where replicates are not availble, calibrate uses standard stepwise ANOVA to compare 
  models (Draper and Smith, 1998, p.335). In this case, if the p-value for the partial F-test to 
  compare models is greater than or equal to \code{p.crit}, then the model with fewer terms is used as 
  the final model.

  The stepwise algorithm terminates when either the p-value is greater than or equal to \code{p.crit}, 
  or the currently selected model in the algorithm is of order \code{max.order}.
}
\value{
  An object of \code{\link{class}} \code{"\link{lm}"} that includes a component called 
  \code{x} that stores the model matrix (the values of the predictor variables for the final 
  calibration model).
}
\references{
  Draper, N., and H. Smith. (1998). \emph{Applied Regression Analysis}. Third Edition. John Wiley and Sons, New York, Chapter 3 and p.335.

  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton, FL, pp.562-575.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Almost always the process of determining the concentration of a chemical in a soil, 
  water, or air sample involves using some kind of machine that produces a signal, and 
  this signal is related to the concentration of the chemical in the physical sample. 
  The process of relating the machine signal to the concentration of the chemical is 
  called \strong{calibration}.  Once calibration has been performed, estimated 
  concentrations in physical samples with unknown concentrations are computed using 
  inverse regression (see \code{\link{inversePredictCalibrate}}).  The uncertainty 
  in the process used to estimate the concentration may be quantified with decision, 
  detection, and quantitation limits.
}
\seealso{
  \code{\link{inversePredictCalibrate}}, \code{\link{detectionLimitCalibrate}}, \code{\link{lm}}.
}
\examples{
  # The data frame EPA.97.cadmium.111.df contains calibration data for 
  # cadmium at mass 111 (ng/L) that appeared in Gibbons et al. (1997b) 
  # and were provided to them by the U.S. EPA.  
  # Display a plot of these data along with the fitted calibration line 
  # and 99\% non-simultaneous prediction limits.  See 
  # Millard and Neerchal (2001, pp.566-569) for more details on this 
  # example.

  Cadmium <- EPA.97.cadmium.111.df$Cadmium 

  Spike <- EPA.97.cadmium.111.df$Spike 

  calibrate.list <- calibrate(Cadmium ~ Spike, 
    data=EPA.97.cadmium.111.df) 

  newdata <- data.frame(Spike = seq(min(Spike), max(Spike), len=100)) 

  pred.list <- predict(calibrate.list, newdata=newdata, se.fit=TRUE) 

  pointwise.list <- pointwise(pred.list, coverage=0.99, 
    individual=TRUE) 

  plot(Spike, Cadmium, ylim=c(min(pointwise.list$lower), 
    max(pointwise.list$upper)), xlab="True Concentration (ng/L)", 
    ylab="Observed Concentration (ng/L)") 

  abline(calibrate.list, lwd=2) 

  lines(newdata$Spike, pointwise.list$lower, lty=8, lwd=2) 

  lines(newdata$Spike, pointwise.list$upper, lty=8, lwd=2) 

  title(paste("Calibration Line and 99\% Prediction Limits", 
    "for US EPA Cadmium 111 Data", sep="\n"))

  rm(Cadmium, Spike, newdata, calibrate.list, pred.list, 
    pointwise.list)
}
\keyword{ models }
\keyword{ regression }

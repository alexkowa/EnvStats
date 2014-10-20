\name{anovaPE}
\alias{anovaPE}
\title{
  Compute Lack-of-Fit and Pure Error Anova Table for a Linear Model
}
\description{
  Compute a lack-of-fit and pure error anova table for either a linear model with 
  one predictor variable or else a linear model for which all predictor variables 
  in the model are functions of a single variable (for example, x, x^2, etc.).  There 
  must be replicate observations for at least one value of the predictor variable(s).
}
\usage{
  anovaPE(object)
}
\arguments{
  \item{object}{an object of \code{\link{class}} \code{"\link{lm}"}.  
  The object must have only one predictor variable in the formula, or else 
  all predictor variables in the model must be functions of a single variable 
  (for example, x, x^2, etc.).  Also, the predictor variable(s) must have replicate 
  observations for at least one value of the predictor variable(s).}  Finally, 
  the total number of observations must be such that the degrees of freedom associated 
  with the residual sums of squares is greater than the number of observations minus 
  the number of unique observations.
}
\details{
  Produces an anova table with the the sums of squares partitioned by \dQuote{Lack of Fit} 
  and \dQuote{Pure Error}.  See Draper and Smith (1998, pp.47-53) for details.
  This function is called by the function \code{\link{calibrate}}.
}
\value{
  An object of \code{\link{class}} \code{"\link{anova}"} inheriting from class 
  \code{"\link{data.frame}"}. 
}
\references{
  Draper, N., and H. Smith. (1998). \emph{Applied Regression Analysis}. Third Edition. 
  John Wiley and Sons, New York, pp.47-53.

  Millard, S.P., and Neerchal, N.K. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton, Florida.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{anova.lm}}, \code{\link{lm}}, \code{\link{calibrate}}.
}
\examples{
  # The data frame EPA.97.cadmium.111.df contains calibration data for 
  # cadmium at mass 111 (ng/L) that appeared in Gibbons et al. (1997b) 
  # and were provided to them by the U.S. EPA. 
  #
  # First, display a plot of these data along with the fitted calibration line 
  # and 99\% non-simultaneous prediction limits.  See  
  # Millard and Neerchal (2001, pp.566-569) for more details on this 
  # example.

  EPA.97.cadmium.111.df
  #   Cadmium Spike
  #1     0.88     0
  #2     1.57     0
  #3     0.70     0
  #...
  #33   99.20   100
  #34   93.71   100
  #35  100.43   100

  Cadmium <- EPA.97.cadmium.111.df$Cadmium 

  Spike <- EPA.97.cadmium.111.df$Spike 

  calibrate.list <- calibrate(Cadmium ~ Spike, 
    data = EPA.97.cadmium.111.df) 

  newdata <- data.frame(Spike = seq(min(Spike), max(Spike), length.out = 100)) 

  pred.list <- predict(calibrate.list, newdata = newdata, se.fit = TRUE) 

  pointwise.list <- pointwise(pred.list, coverage = 0.99, individual = TRUE) 

  plot(Spike, Cadmium, ylim = c(min(pointwise.list$lower), 
    max(pointwise.list$upper)), xlab = "True Concentration (ng/L)", 
    ylab = "Observed Concentration (ng/L)") 

  abline(calibrate.list, lwd = 2) 

  lines(newdata$Spike, pointwise.list$lower, lty = 8, lwd = 2) 

  lines(newdata$Spike, pointwise.list$upper, lty = 8, lwd = 2) 

  title(paste("Calibration Line and 99\% Prediction Limits", 
    "for US EPA Cadmium 111 Data", sep="\n"))

  rm(Cadmium, Spike, newdata, calibrate.list, pred.list, 
    pointwise.list)

  #----------

  # Now fit the linear model and produce the anova table to check for 
  # lack of fit.  There is no evidence for lack of fit (p = 0.41).

  fit <- lm(Cadmium ~ Spike, data = EPA.97.cadmium.111.df) 


  anova(fit) 
  #Analysis of Variance Table
  #
  #Response: Cadmium
  #          Df Sum Sq Mean Sq F value    Pr(>F)    
  #Spike      1  43220   43220  9356.9 < 2.2e-16 ***
  #Residuals 33    152       5                      
  #---
  #Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  #Analysis of Variance Table
  #
  #Response: Cadmium 
  #
  #Terms added sequentially (first to last) 
  #          Df Sum of Sq  Mean Sq  F Value Pr(F) 
  #    Spike  1  43220.27 43220.27 9356.879     0 
  #Residuals 33    152.43     4.62 


  anovaPE(fit) 
  #                 Df Sum Sq Mean Sq  F value Pr(>F)    
  #Spike             1  43220   43220 9341.559 <2e-16 ***
  #Lack of Fit       3     14       5    0.982 0.4144    
  #Pure Error       30    139       5                    
  #---
  #Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 


  rm(fit)
}
\keyword{ models }
\keyword{ regression }


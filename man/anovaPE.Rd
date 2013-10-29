\name{anovaPE}
\alias{anovaPE}
\title{
  Compute Lack-of-Fit and Pure Error Anova Table for a Linear Model
}
\description{
  Compute a lack-of-fit and pure error anova table for a linear model with one predictor 
  variable and replicate observations for at least one value of the predictor variable.
}
\usage{
  anovaPE(object)
}
\arguments{
  \item{object}{an object of \code{\link{class}} \code{"\link{lm}"}.  
  The object can have only one predictor variable in the formula and 
  must have replicate observations for at least one value of the predictor variable.}
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
  # The Examples section of the help file for calibrate displays a plot 
  # of these data along with the fitted calibration line and 99\% 
  # non-simultaneous prediction limits. 
  # Here we will just fit the linear model and produce the anova table 
  # to check for lack of fit.

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


\name{varTest}
\alias{varTest}
\title{
  One-Sample Chi-Squared Test on Variance
}
\description{
  Estimate the variance, test the null hypothesis using the chi-squared test that the variance is equal 
  to a user-specified value, and create a confidence interval for the variance.
}
\usage{
  varTest(x, alternative = "two.sided", conf.level = 0.95, 
    sigma.squared = 1, data.name = NULL)
}
\arguments{
  \item{x}{
  numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), and 
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{alternative}{
  character string indicating the kind of alternative hypothesis.  The possible values are 
  \code{"two.sided"} (the default), \code{"greater"}, and \code{"less"}.
}
  \item{conf.level}{
  numeric scalar between 0 and 1 indicating the confidence level associated with the confidence 
  interval for the population variance.  The default value is \cr 
  \code{conf.level=0.95}.
}
  \item{sigma.squared}{
  a numeric scalar indicating the hypothesized value of the variance.  The default value is 
  \code{sigma.squared=1}.
}
  \item{data.name}{
  character string indicating the name of the data used for the test of variance.
}
}
\details{
  The function \code{varTest} performs the one-sample chi-squared test of the hypothesis 
  that the population variance is equal to the user specified value given by the argument 
  \code{sigma.squared}, and it also returns a confidence interval for the population variance.  
  The \R function \code{\link{var.test}} performs the F-test for comparing two variances.
}
\value{
  A list of class \code{"htest"} containing the results of the hypothesis test.  
  See the help file for \code{\link{htest.object}} for details.
}
\references{
  van Belle, G., L.D. Fisher, Heagerty, P.J., and Lumley, T. (2004). 
  \emph{Biostatistics: A Methodology for the Health Sciences, 2nd Edition}. 
  John Wiley & Sons, New York.

  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton, FL.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition. 
  Prentice-Hall, Upper Saddle River, NJ.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Just as you can perform tests of hypothesis on measures of location (mean, median, percentile, etc.), 
  you can do the same thing for measures of spread or variability.  Usually, we are interested in 
  estimating variability only because we want to quantify the uncertainty of our estimated location or 
  percentile.  Sometimes, however, we are interested in estimating variability and quantifying the 
  uncertainty in our estimate of variability (for example, for performing a sensitivity analysis for 
  power or sample size calculations), or testing whether the population variability is equal to a 
  certain value.  There are at least two possible methods of performing a one-sample hypothesis test on 
  variability:

  \itemize{
    \item Perform a hypothesis test for the population variance based on the chi-squared statistic, 
          assuming the underlying population is normal.

    \item Perform a hypothesis test for any kind of measure of spread assuming any kind of underlying 
          distribution based on a bootstrap confidence interval (using, for example, the 
          package \pkg{boot}).
  }
 
  You can use \code{varTest} for the first method.

  \bold{Note:}  For a one-sample test of location, Student's t-test is fairly robust to departures 
  from normality (i.e., the Type I error rate is maintained), as long as the sample size is 
  reasonably "large."  The chi-squared test on the population variance, however, is extremely sensitive 
  to departures from normality.  For example, if the underlying population is skewed, the actual 
  Type I error rate will be larger than assumed.
}
\seealso{
  \code{\link{var.test}}, \code{\link{varGroupTest}}.
}
\examples{
  # Generate 20 observations from a normal distribution with parameters 
  # mean=2 and sd=1.  Test the null hypothesis that the true variance is 
  # equal to 0.5 against the alternative that the true variance is not 
  # equal to 0.5.  
  # (Note: the call to set.seed allows you to reproduce this example).

  set.seed(23) 
  dat <- rnorm(20, mean = 2, sd = 1) 
  varTest(dat, sigma.squared = 0.5) 

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 variance = 0.5
  #
  #Alternative Hypothesis:          True variance is not equal to 0.5
  #
  #Test Name:                       Chi-Squared Test on Variance
  #
  #Estimated Parameter(s):          variance = 0.753708
  #
  #Data:                            dat
  #
  #Test Statistic:                  Chi-Squared = 28.64090
  #
  #Test Statistic Parameter:        df = 19
  #
  #P-value:                         0.1436947
  #
  #95% Confidence Interval:         LCL = 0.4359037
  #                                 UCL = 1.6078623

  # Note that in this case we would not reject the 
  # null hypothesis at the 5% or even the 10% level.

  # Clean up
  rm(dat)
}
\keyword{htest}
\keyword{models}

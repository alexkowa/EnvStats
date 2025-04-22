\name{print.htest.htestEnvStats}
\alias{print.htest}
\alias{print.htestEnvStats}
\title{
  Print Output of Hypothesis Tests
}
\description{
  Print objects of class \code{"htest"} or \code{"htestEnvStats"}, respectively, 
  by simple \code{\link{print}} methods.
  
  The functions \code{print.htest} and \code{print.htestEnvStats} are identical, and are a 
  modification of the \R function \code{\link[stats:print.power.htest]{print.htest}}. 

  The function \code{print.htestEnvStats} formats and prints the results of performing 
  a hypothesis test that was performed using one of the functions listed in the help file for 
  \code{\link{htestEnvStats.object}}.  This method is automatically called by the 
  \pkg{EnvStats} generic function \code{\link{print}} when
  given an object of class \code{"\link[=htestEnvStats.object]{htestEnvStats}"}.

  The function \code{print.htest} formats and prints the results of performing 
  a hypothesis test that was performed using a function that returns an object of 
  class \code{"htest"}.  Because of the design of \R, the \pkg{EnvStats} method 
  \code{print.htest} is NOT automatically called when the user types a command at 
  the command prompt that returns an object of class \code{"\link[=htest.object]{htest}"}.  
  Instead, the \pkg{EnvStats} \code{\link{print}} function must be explicitly called to invoke 
  \code{print.htest}.  See the EXAMPLES section below.
}
\usage{
  \method{print}{htest}(x, ...)
  \method{print}{htestEnvStats}(x, ...)
}
\arguments{
  \item{x}{
  an object of class \code{"htest"} or \code{"htestEnvStats"}.  
  See \code{\link{htest.object}} and \cr
  \code{\link{htestEnvStats.object}} for details.
}
  \item{\dots}{
  arguments that can be supplied to the \code{\link[base]{format}} function.
}
}
\details{
  The function \code{print.htest} is the \code{"htest"} method for the \pkg{EnvStats} generic 
  function \code{\link{print}}, and the function \code{print.htestEnvStats} is the 
  \code{"htestEnvStats"} method for the \pkg{EnvStats} generic function \code{\link{print}}.
  These functions print null and alternative hypotheses, name of the test, estimated population 
  parameter(s) involved in the null hypothesis, estimation method (if present), 
  data name, sample size (if present), number of missing observations removed 
  prior to performing the test (if present), value of the test statistic, 
  parameters associated with the null distribution of the test statistic, 
  p-value associated with the test statistic, and confidence interval for the 
  population parameter (if present).
}
\value{
  Invisibly returns the input \code{x}.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992).  \emph{Statistical Models in S}.  
  Wadsworth & Brooks/Cole. 
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \link{Hypothesis Tests}, \code{\link{htest.object}}, \code{\link{htestEnvStats.object}},
  \code{\link{print}}.
}
\examples{
  # Create an object of class "htestEnvStats", then print it out. 
  #--------------------------------------------------------------
  htestEnvStats.obj <- chenTTest(EPA.02d.Ex.9.mg.per.L.vec, mu = 30)

  mode(htestEnvStats.obj) 
  #[1] "list" 

  class(htestEnvStats.obj) 
  #[1] "htestEnvStats" 

  names(htestEnvStats.obj) 
  # [1] "statistic"   "parameters"  "p.value"     "estimate"   
  # [5] "null.value"  "alternative" "method"      "sample.size"
  # [9] "data.name"   "bad.obs"     "interval" 
 
  htestEnvStats.obj 
  
  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 mean = 30
  #
  #Alternative Hypothesis:          True mean is greater than 30
  #
  #Test Name:                       One-sample t-Test
  #                                 Modified for
  #                                 Positively-Skewed Distributions
  #                                 (Chen, 1995)
  #
  #Estimated Parameter(s):          mean = 34.566667
  #                                 sd   = 27.330598
  #                                 skew =  2.365778
  #
  #Data:                            EPA.02d.Ex.9.mg.per.L.vec
  #
  #Sample Size:                     60
  #
  #Test Statistic:                  t = 1.574075
  #
  #Test Statistic Parameter:        df = 59
  #
  #P-values:                        z               = 0.05773508
  #                                 t               = 0.06040889
  #                                 Avg. of z and t = 0.05907199
  #
  #Confidence Interval for:         mean
  #
  #Confidence Interval Method:      Based on z
  #
  #Confidence Interval Type:        Lower
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 29.82
  #                                 UCL =   Inf

  #============================================================================

  # Now create an object of class "htest" and note the difference in how it is 
  # printed out depending on whether or not you explicitly use the print() command.
  #--------------------------------------------------------------------------------

  htest.obj <- t.test(EPA.02d.Ex.9.mg.per.L.vec, mu = 30, alternative = "greater")

  class(htest.obj) 
  #[1] "htest" 

  names(htest.obj) 
  # [1] "statistic"   "parameter"   "p.value"     "conf.int"    "estimate"   
  # [6] "null.value"  "stderr"      "alternative" "method"      "data.name"

  htest.obj

  #        One Sample t-test
  #
  #data:  EPA.02d.Ex.9.mg.per.L.vec
  #t = 1.2943, df = 59, p-value = 0.1003
  #alternative hypothesis: true mean is greater than 30
  #95 percent confidence interval:
  # 28.67044      Inf
  #sample estimates:
  #mean of x 
  # 34.56667

  print(htest.obj)

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 mean = 30
  #
  #Alternative Hypothesis:          True mean is greater than 30
  #
  #Test Name:                       One Sample t-test
  #
  #Estimated Parameter(s):          mean of x = 34.56667
  #
  #Data:                            EPA.02d.Ex.9.mg.per.L.vec
  #
  #Test Statistic:                  t = 1.294273
  #
  #Test Statistic Parameter:        df = 59
  #
  #P-value:                         0.1003072
  #
  #95% Confidence Interval:         LCL = 28.67044
  #                                 UCL =      Inf

  #==========

  # Clean up
  #---------
  rm(htestEnvStats.obj, htest.obj)
}
\keyword{print}

\name{plotAovDesign}
\alias{plotAovDesign}
\title{
  Create Plots for a Sampling Design Based on a One-Way Fixed-Effects Analysis of Variance
}
\description{
  Create plots involving sample size, power, scaled difference, and significance level for a 
  one-way fixed-effects analysis of variance.
}
\usage{
  plotAovDesign(x.var = "n", y.var = "power", range.x.var = NULL, 
    n.vec = c(25, 25), mu.vec = c(0, 1), sigma = 1, alpha = 0.05, power = 0.95, 
    round.up = FALSE, n.max = 5000, tol = 1e-07, maxiter = 1000, plot.it = TRUE, 
    add = FALSE, n.points = 50, plot.col = 1, plot.lwd = 3 * par("cex"), 
    plot.lty = 1, digits = .Options$digits, main = NULL, xlab = NULL, ylab = NULL, 
    type = "l", ...)
}
\arguments{
  \item{x.var}{
  character string indicating what variable to use for the x-axis.  Possible values are 
  \code{"n"} (sample size; the default), \code{"power"} (power of the test), and 
  \code{"alpha"} (significance level of the test).
}
  \item{y.var}{
  character string indicating what variable to use for the y-axis.  Possible values are 
  \code{"power"} (power of the test; the default) and \code{"n"} (sample size).
}
  \item{range.x.var}{
  numeric vector of length 2 indicating the range of the x-variable to use for the plot.  
  The default value depends on the value of \code{x.var}.  When \code{x.var="n"} 
  the default value is \code{c(2,50)}.  When \code{x.var="power"} the default value is \cr
  \code{c(alpha+.Machine$double.eps, 0.95)}.  When \code{x.var="alpha"}, the default 
  value is \code{c(0.01, 0.2)}.
}
  \item{n.vec}{
  numeric vector indicating the sample size for each group.  The default value is 
  \code{n.vec=c(25, 25)}.  Missing (\code{NA}), undefined (\code{NaN}), and 
  infinite (\code{Inf}, \code{-Inf}) values are not allowed.  This argument must be 
  the same length as \code{mu.vec}.  This argument is ignored if either 
  \code{x.var="n"} or \code{y.var="n"}.
}
  \item{mu.vec}{
  numeric vector indicating the population mean for each group.  The default value is 
  \code{mu.vec=c(0, 1)}.  Missing (\code{NA}), undefined (\code{NaN}), and 
  infinite (\code{Inf}, \code{-Inf}) values are not allowed.  This argument must be 
  the same length as \code{n.vec}.
}
  \item{sigma}{
  numeric scalar indicating the population standard deviation for all groups.  The default
  value is \code{sigma=1}.  Missing (\code{NA}), undefined (\code{NaN}), and 
  infinite (\code{Inf}, \code{-Inf}) values are not allowed.
}
  \item{alpha}{
  numeric scalar between 0 and 1 indicating the Type I error level associated with the 
  hypothesis test.  The default value is \code{alpha=0.05}.  This argument is ignored 
  when \code{x.var="alpha"}.
}
  \item{power}{
  numeric scalar between 0 and 1 indicating the power associated with the hypothesis 
  test.  The default value is \code{power=0.95}.  This argument is ignored when 
  \code{x.var="power"} or \code{y.var="power"}.
}
  \item{round.up}{
  logical scalar indicating whether to round up the values of the computed sample 
  size(s) to the next smallest integer.  The default value is FALSE.  This argument 
  is ignored unless \code{y.var="n"}.
}
  \item{n.max}{
  for the case when \code{y.var="n"}, a positive integer greater than 2 indicating the 
  maximum sample size per group.  The default value is \code{n.max=5000}.
}
  \item{tol}{
  for the case when \code{y.var="n"}, numeric scalar indicating the 
  tolerance to use in the \code{\link{uniroot}} search for the sample size.  
  The default value is \code{tol=1e-7}.  
}
  \item{maxiter}{
  for the case when \code{y.var="n"}, positive integer greater then 1 indicating the 
  maximum number of iterations to use in the \code{\link{uniroot}} search for 
  the sample size.  The default value is \code{maxiter=1000}.  
}
  \item{plot.it}{
  a logical scalar indicating whether to create a plot or add to the existing plot 
  (see \code{add}) on the current graphics device.  If \code{plot.it=FALSE}, no plot 
  is produced, but a list of (x,y) values is returned (see VALUE).  The default 
  value is \code{plot.it=TRUE}.
}
  \item{add}{
  a logical scalar indicating whether to add the design plot to the existing plot 
  (\code{add=TRUE}), or to create a new plot (\code{add=FALSE}).  The default value is \cr
  \code{add=FALSE}.  This argument is ignored if \code{plot.it=FALSE}.
}
  \item{n.points}{
  a numeric scalar specifying how many (x,y) pairs to use to produce the plot.  There are 
  \code{n.points} x-values evenly spaced between \code{range.x.var[1]} and \cr
  \code{range.x.var[2]}. The default value is \code{n.points=50}.
}
  \item{plot.col}{
  a numeric scalar or character string determining the color of the plotted line or points.  The default value 
  is \code{plot.col=1}.  See the entry for \code{col} in the help file for \code{\link{par}} 
  for more information.
}
  \item{plot.lwd}{
  a numeric scalar determining the width of the plotted line.  The default value is 
  \code{3*par("cex")}.  See the entry for \code{lwd} in the help file for \code{\link{par}} 
  for more information.
}
  \item{plot.lty}{
  a numeric scalar determining the line type of the plotted line.  The default value is 
  \code{plot.lty=1}.  See the entry for \code{lty} in the help file for \code{\link{par}} 
  for more information.
}
  \item{digits}{
  a scalar indicating how many significant digits to print out on the plot.  The default 
  value is the current setting of \code{\link{options}("digits")}.
}
  \item{main, xlab, ylab, type, \dots}{
  additional graphical parameters (see \code{\link{par}}). 
}
}
\details{
  See the help files for \code{\link{aovPower}} and \code{\link{aovN}} 
  for information on how to compute the power and sample size for a 
  one-way fixed-effects analysis of variance.
}
\value{
  \code{plotAovDesign} invisibly returns a list with components:

  \item{x.var}{x-coordinates of the points that have been or would have been plotted}
  \item{y.var}{y-coordinates of the points that have been or would have been plotted}
}
\references{
  Berthouex, P.M., and L.C. Brown. (1994). 
  \emph{Statistics for Environmental Engineers}. 
  Lewis Publishers, Boca Raton, FL, Chapter 17.

  Helsel, D.R., and R.M. Hirsch. (1992). 
  \emph{Statistical Methods in Water Resources Research}. 
  Elsevier, New York, NY, Chapter 7.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995). 
  \emph{Continuous Univariate Distributions, Volume 2}. 
  Second Edition. John Wiley and Sons, New York, 
  Chapters 27, 29, 30.

  Scheffe, H. (1959). \emph{The Analysis of Variance}. 
  John Wiley and Sons, New York, 477pp.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. 
  Fifth Edition. Prentice-Hall, Upper Saddle River, NJ, 
  Chapter 10.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The normal and lognormal distribution are probably the two most 
  frequently used distributions to model environmental data.  
  Sometimes it is necessary to compare several means to determine 
  whether any are significantly different from each other 
  (e.g., USEPA, 2009, p.6-38).  In this case, assuming 
  normally distributed data, you perform a one-way parametric 
  analysis of variance.

  In the course of designing a sampling program, an environmental 
  scientist may wish to determine the relationship between sample 
  size, Type I error level, power, and differences in means if 
  one of the objectives of the sampling program is to determine 
  whether a particular mean differs from a group of means.  The 
  functions \code{\link{aovPower}}, \code{\link{aovN}}, and 
  \code{plotAovDesign} can be used to investigate these 
  relationships for the case of normally-distributed observations.
}
\seealso{
  \code{\link{aovPower}}, \code{\link{aovN}}, 
  \code{\link{Normal}}, \code{\link{aov}}.
}
\examples{
  # Look at the relationship between power and sample size 
  # for a one-way ANOVA, assuming k=2 groups, group means of 
  # 0 and 1, a population standard deviation of 1, and a 
  # 5\% significance level:

  dev.new()
  plotAovDesign()

  #--------------------------------------------------------------------

  # Plot power vs. sample size for various levels of significance:

  dev.new()
  plotAovDesign(mu.vec = c(0, 0.5, 1), ylim=c(0, 1), main="") 

  plotAovDesign(mu.vec = c(0, 0.5, 1), alpha=0.1, add=TRUE, plot.col=2) 

  plotAovDesign(mu.vec = c(0, 0.5, 1), alpha=0.2, add=TRUE, plot.col=3) 

  legend(35, 0.6, c("20\%", "10\%", "   5\%"), lty=1, lwd = 3, col=3:1, 
    bty = "n") 

  mtext("Power vs. Sample Size for One-Way ANOVA", line = 3, cex = 1.25)
  mtext(expression(paste("with ", mu, "=(0, 0.5, 1), ", sigma, 
    "=1, and Various Significance Levels", sep="")), 
    line = 1.5, cex = 1.25)

  #--------------------------------------------------------------------

  # The example on pages 5-11 to 5-14 of USEPA (1989b) shows 
  # log-transformed concentrations of lead (mg/L) at two 
  # background wells and four compliance wells, where 
  # observations were taken once per month over four months 
  # (the data are stored in EPA.89b.loglead.df).  
  # Assume the true mean levels at each well are 
  # 3.9, 3.9, 4.5, 4.5, 4.5, and 5, respectively.  Plot the 
  # power vs. sample size of a one-way ANOVA to test for mean 
  # differences between wells.  Use alpha=0.05, and assume the 
  # true standard deviation is equal to the one estimated 
  # from the data in this example.

  names(EPA.89b.loglead.df) 
  #[1] "LogLead"   "Month"     "Well"      "Well.type"

  # Perform the ANOVA and get the estimated sd 
  aov.list <- aov(LogLead ~ Well, data=EPA.89b.loglead.df) 

  summary(aov.list) 
  #            Df Sum Sq Mean Sq F value  Pr(>F)  
  #Well         5 5.7447 1.14895  3.3469 0.02599 *
  #Residuals   18 6.1791 0.34328                  
  #---
  #Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 '' 1
 
  # Now create the plot 
  dev.new()
  plotAovDesign(range.x.var = c(2, 20), 
    mu.vec = c(3.9,3.9,4.5,4.5,4.5,5), 
    sigma=sqrt(0.34), 
    ylim = c(0, 1), digits=2)

  # Clean up
  #---------
  rm(aov.list)
  graphics.off()
}
\keyword{ design }
\keyword{ htest }
\keyword{ models }
\keyword{ regression }
\keyword{ hplot }

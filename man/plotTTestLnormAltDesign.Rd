\name{plotTTestLnormAltDesign}
\alias{plotTTestLnormAltDesign}
\title{
  Plots for a Sampling Design Based on a One- or Two-Sample t-Test, Assuming Lognormal Data 
}
\description{
  Create plots involving sample size, power, ratio of means, coefficient of variation, 
  and significance level for a one- or two-sample t-test, assuming lognormal data.
}
\usage{
  plotTTestLnormAltDesign(x.var = "n", y.var = "power", range.x.var = NULL, 
    n.or.n1 = 25, n2 = n.or.n1, 
    ratio.of.means = switch(alternative, greater = 2, less = 0.5, 
      two.sided = ifelse(two.sided.direction == "greater", 2, 0.5)), 
    cv = 1, alpha = 0.05, power = 0.95, 
    sample.type = ifelse(!missing(n2), "two.sample", "one.sample"), 
    alternative = "two.sided", two.sided.direction = "greater", approx = FALSE, 
    round.up = FALSE, n.max = 5000, tol = 1e-07, maxiter = 1000, plot.it = TRUE, 
    add = FALSE, n.points = 50, plot.col = "black", plot.lwd = 3 * par("cex"), 
    plot.lty = 1, digits = .Options$digits, cex.main = par("cex"), ..., 
    main = NULL, xlab = NULL, ylab = NULL, type = "l")
}
\arguments{
  \item{x.var}{
  character string indicating what variable to use for the x-axis.  
  Possible values are \code{"n"} (sample size; the default), 
  \code{"ratio.of.means"} (minimal or maximal detectable ratio of means), 
  \code{"cv"} (coefficient of variaiton), \code{"power"} (power of the test), and 
  \code{"alpha"} (significance level of the test).
}
  \item{y.var}{
  character string indicating what variable to use for the y-axis.  
  Possible values are \code{"power"} (power of the test; the default), 
  \code{"ratio.of.means"} (minimal or maximal detectable ratio of means), and 
  \code{"n"} (sample size). 
}
  \item{range.x.var}{
  numeric vector of length 2 indicating the range of the x-variable to use 
  for the plot.  The default value depends on the value of \code{x.var}:
  \itemize{
    \item When \code{x.var="n"} the default value is \code{c(2,50)}.  
    \item When \code{x.var="ratio.of.means"} and \code{alternative="greater"} or \cr
      \code{alternative="two.sided"} and \code{two.sided.direction="greater"}, 
      the default value is \code{c(1, 2)}.  
    \item When \code{x.var="delta"} and \code{alternative="less"} or \cr
      \code{alternative="two.sided"} and \code{two.sided.direction="less"}, 
      the default value is \code{c(0.5, 1)}.  
    \item When \code{x.var="cv"} the default value is \code{c(0.5, 2)}.  
    \item When \code{x.var="power"} the default value is \cr
      \code{c(alpha + .Machine$double.eps, 0.95)}.  
    \item When \code{x.var="alpha"}, the default value is \code{c(0.01, 0.2)}.
  }
}
  \item{n.or.n1}{
  numeric scalar indicating the sample size.  The default value is 
  \code{n.or.n1=25}.  When \code{sample.type="one.sample"}, \code{n.or.n1} 
  denotes the number of observations in the single sample.  When 
  \code{sample.type="two.sample"}, \code{n.or.n1} denotes the number of 
  observations from group 1.  Missing (\code{NA}), undefined (\code{NaN}), 
  and infinite (\code{Inf}, \code{-Inf}) values are not allowed.  This 
  argument is ignored if either \code{x.var="n"} or \code{y.var="n"}.
}
  \item{n2}{
  numeric scalar indicating the sample size for group 2.  The default value 
  is the value of \code{n.or.n1}.  Missing (\code{NA}), undefined (\code{NaN}), 
  and infinite (\code{Inf}, \code{-Inf}) values are not allowed.  This 
  argument is ignored when \code{sample.type="one.sample"}.
}
  \item{ratio.of.means}{
  numeric scalar specifying the ratio of the first mean to the second mean.  When 
  \code{sample.type="one.sample"}, this is the ratio of the population mean to the 
  hypothesized mean.  When \code{sample.type="two.sample"}, this is the ratio of the 
  mean of the first population to the mean of the second population. 

  When \code{alternative="greater"} or \code{alternative="two.sided"} and \cr
  \code{two.sided.direction="greater"}, the default value is \code{ratio.of.means=2}.  
  When \code{alternative="less"} or \cr
  \code{alternative="two.sided"} and 
  \code{two.sided.direction="less"}, the default value is \code{ratio.of.means=0.5}.  
  This argument is ignored when \cr
  \code{x.var="ratio.of.means"} or \code{y.var="ratio.of.means"}.
}
  \item{cv}{
  numeric scalar: a positive value specifying the coefficient of 
  variation.  When \code{sample.type="one.sample"}, this is the population coefficient 
  of variation.  When \code{sample.type="two.sample"}, this is the coefficient of 
  variation for both the first and second population.  The default value is \code{cv=1}.
}
  \item{alpha}{
  numeric scalar between 0 and 1 indicating the Type I error level 
  associated with the hypothesis test.  The default value is \code{alpha=0.05}.
}
  \item{power}{
  numeric scalar between 0 and 1 indicating the power  
  associated with the hypothesis test.  The default value is \code{power=0.95}.
}
  \item{sample.type}{
  character string indicating whether to compute power based on a one-sample or 
  two-sample hypothesis test.  When \code{sample.type="one.sample"}, the computed 
  power is based on a hypothesis test for a single mean.  When \cr
  \code{sample.type="two.sample"}, the computed power is based on a hypothesis test 
  for the difference between two means.  The default value is \cr
  \code{sample.type="one.sample"} unless the argument \code{n2} is supplied.
}
  \item{alternative}{
  character string indicating the kind of alternative hypothesis.  The possible values 
  are \code{"two.sided"} (the default), \code{"greater"}, and \code{"less"}.
}
  \item{two.sided.direction}{
  character string indicating the direction (greater than 1 or less than 1) for the 
  detectable ratio of means when \code{alternative="two.sided"}.  When \cr
  \code{two.sided.direction="greater"} (the default), the detectable ratio of means 
  is greater than 1.  When \cr
  \code{two.sided.direction="less"}, the detectable ratio of 
  means is less than 1 (but greater than 0).  This argument is ignored if 
  \code{alternative="less"} or \cr
  \code{alternative="greater"}.
}
  \item{approx}{
  logical scalar indicating whether to compute the power based on an approximation to 
  the non-central t-distribution.  The default value is \code{approx=FALSE}.
}
  \item{round.up}{
  logical scalar indicating whether to round up the values of the computed 
  sample size(s) to the next smallest integer.  The default value is 
  \code{TRUE}.
}
  \item{n.max}{
  for the case when \code{y.var="n"}, a positive integer greater than 1 indicating 
  the maximum sample size when \code{sample.type="one.sample"} or the maximum sample 
  size for group 1 when \code{sample.type="two.sample"}.  The default value is 
  \code{n.max=5000}.
}
  \item{tol}{
  numeric scalar indicating the toloerance to use in the 
  \code{\link{uniroot}} search algorithm.  
  The default value is \code{tol=1e-7}.
}
  \item{maxiter}{
  positive integer indicating the maximum number of iterations 
  argument to pass to the \code{\link{uniroot}} function.  The default 
  value is \code{maxiter=1000}.
}
  \item{plot.it}{
  a logical scalar indicating whether to create a new plot or add to the existing plot 
  (see \code{add}) on the current graphics device.  If \code{plot.it=FALSE}, no plot 
  is produced, but a list of (x,y) values is returned (see VALUE).  The default value 
  is \code{plot.it=TRUE}.
}
  \item{add}{
  a logical scalar indicating whether to add the design plot to the 
  existing plot (\code{add=TRUE}), or to create a plot from scratch 
  (\code{add=FALSE}).  The default value is \code{add=FALSE}.  
  This argument is ignored if \code{plot.it=FALSE}.
}
  \item{n.points}{
  a numeric scalar specifying how many (x,y) pairs to use to produce the plot.  
  There are \code{n.points} x-values evenly spaced between \code{range.x.var[1]} and \cr
  \code{range.x.var[2]}.  The default value is \code{n.points=100}.
}
  \item{plot.col}{
  a numeric scalar or character string determining the color of the plotted 
  line or points.  The default value is \code{plot.col="black"}.  See the 
  entry for \code{col} in the help file for \code{\link{par}} 
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
  \item{cex.main, main, xlab, ylab, type, \dots}{
  additional graphical parameters (see \code{\link{par}}).
}
}
\details{
  See the help files for \code{\link{tTestLnormAltPower}}, 
  \code{\link{tTestLnormAltN}}, and \code{\link{tTestLnormAltRatioOfMeans}} for 
  information on how to compute the power, sample size, or ratio of means for a 
  one- or two-sample t-test assuming lognormal data.
}
\value{
  \code{plotTTestLnormAltDesign} invisibly returns a list with components 
  \code{x.var} and \code{y.var}, giving coordinates of the points that have 
  been or would have been plotted.
}
\references{
  See the help files for \code{\link{tTestLnormAltPower}}, 
  \code{\link{tTestLnormAltN}}, and \code{\link{tTestLnormAltRatioOfMeans}}.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help files for \code{\link{tTestLnormAltPower}}, 
  \code{\link{tTestLnormAltN}}, and \code{\link{tTestLnormAltRatioOfMeans}}.
}
\seealso{
  \code{\link{tTestLnormAltPower}}, \code{\link{tTestLnormAltN}}, 
  \code{\link{tTestLnormAltRatioOfMeans}}, \code{\link{t.test}}.
}
\examples{
  # Look at the relationship between power and sample size for a two-sample t-test, 
  # assuming lognormal data, a ratio of means of 2, a coefficient of variation 
  # of 1, and a 5% significance level:

  dev.new()
  plotTTestLnormAltDesign(sample.type = "two")

  #----------

  # For a two-sample t-test based on lognormal data, plot sample size vs. the 
  # minimal detectable ratio for various levels of power, assuming a coefficient 
  # of variation of 1 and using a 5% significance level:

  dev.new()
  plotTTestLnormAltDesign(x.var = "ratio.of.means", y.var = "n", 
    range.x.var = c(1.5, 2), sample.type = "two", ylim = c(20, 120), main="") 

  plotTTestLnormAltDesign(x.var = "ratio.of.means", y.var = "n", 
    range.x.var = c(1.5, 2), sample.type="two", power = 0.9, 
    add = TRUE, plot.col = "red") 

  plotTTestLnormAltDesign(x.var = "ratio.of.means", y.var = "n", 
    range.x.var = c(1.5, 2), sample.type="two", power = 0.8, 
    add = TRUE, plot.col = "blue") 

  legend("topright", c("95\%", "90\%", "80\%"), lty=1, lwd = 3*par("cex"), 
    col = c("black", "red", "blue"), bty = "n") 

  title(main = paste("Sample Size vs. Ratio of Lognormal Means for", 
    "Two-Sample t-Test, with CV=1, Alpha=0.05 and Various Powers", 
    sep="\n")) 

  #==========

  # The guidance document Soil Screening Guidance: Technical Background Document 
  # (USEPA, 1996c, Part 4) discusses sampling design and sample size calculations 
  # for studies to determine whether the soil at a potentially contaminated site 
  # needs to be investigated for possible remedial action. Let 'theta' denote the 
  # average concentration of the chemical of concern.  The guidance document 
  # establishes the following goals for the decision rule (USEPA, 1996c, p.87):
  #
  #     Pr[Decide Don't Investigate | theta > 2 * SSL] = 0.05
  #
  #     Pr[Decide to Investigate | theta <= (SSL/2)] = 0.2
  #
  # where SSL denotes the pre-established soil screening level.
  #
  # These goals translate into a Type I error of 0.2 for the null hypothesis
  #
  #     H0: [theta / (SSL/2)] <= 1
  #
  # and a power of 95% for the specific alternative hypothesis
  #
  #     Ha: [theta / (SSL/2)] = 4
  #
  # Assuming a lognormal distribution, a coefficient of variation of 2, and the above 
  # values for Type I error and power, create a performance goal diagram 
  # (USEPA, 1996c, p.89) showing the power of a one-sample test versus the minimal 
  # detectable ratio of theta/(SSL/2) when the sample size is 6 and the exact power 
  # calculations are used.

  dev.new()
  plotTTestLnormAltDesign(x.var = "ratio.of.means", y.var = "power", 
    range.x.var = c(1, 5), n.or.n1 = 6, cv = 2, alpha = 0.2, 
    alternative = "greater", approx = FALSE, ylim = c(0.2, 1), 
    xlab = "theta / (SSL/2)") 

  #==========

  # Clean up
  #---------
  graphics.off()
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }

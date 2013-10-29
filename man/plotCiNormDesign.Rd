\name{plotCiNormDesign}
\alias{plotCiNormDesign}
\title{
  Plots for Sampling Design Based on Confidence Interval for Mean of a Normal Distribution or Difference Between Two Means
}
\description{
  Create plots involving sample size, half-width, estimated standard deviation, and confidence level 
  for a confidence interval for the mean of a normal distribution or the difference between two means.
}
\usage{
  plotCiNormDesign(x.var = "n", y.var = "half.width", 
    range.x.var = NULL, n.or.n1 = 25, n2 = n.or.n1, 
    half.width = sigma.hat/2, sigma.hat = 1, conf.level = 0.95, 
    sample.type = ifelse(missing(n2), "one.sample", "two.sample"), 
    round.up = FALSE, n.max = 5000, tol = 1e-07, maxiter = 1000,
    plot.it = TRUE, add = FALSE, n.points = 100,
    plot.col = "black", plot.lwd = 3 * par("cex"), plot.lty = 1, 
    digits = .Options$digits, 
    main = NULL, xlab = NULL, ylab = NULL, type = "l", ...)
}
\arguments{
  \item{x.var}{
  character string indicating what variable to use for the x-axis.  
  Possible values are \code{"n"} (sample size; the default), 
  \code{"half.width"} (the half-width of the confidence interval), 
  \code{"sigma.hat"} (the estimated standard deviation), and 
  \code{"conf.level"} (the confidence level).
}
  \item{y.var}{
  character string indicating what variable to use for the y-axis.  
  Possible values are \code{"half.width"} (the half-width of the confidence interval; the default), and 
  \code{"n"} (sample size).
}
  \item{range.x.var}{
  numeric vector of length 2 indicating the range of the x-variable to use for the plot.  
  The default value depends on the value of \code{x.var}.  
  When \code{x.var="n"} the default value is \code{c(2,50)}.  
  When \code{x.var="half.width"} the default value is \code{c(0.1/sigma.hat, 2/sigma.hat)}.  
  When \code{x.var="sigma.hat"}, the default value is \code{c(0.1, 2)}.  
  When \code{x.var="conf.level"}, the default value is \code{c(0.5, 0.99)}.
}
  \item{n.or.n1}{
  numeric scalar indicating the sample size.  The default value is \code{n.or.n1=25}.  
  When \code{sample.type="one.sample"}, this argument denotes the number of observations 
  in the single sample.  
  When \code{sample.type="two.sample"}, this argument denotes the number of observations 
  from group 1. 
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.  
  This argument is ignored if either \code{x.var="n"} or \code{y.var="n"}.
}
  \item{n2}{
  numeric scalar indicating the sample size for group 2.  
  The default value is the value of \code{n.or.n1}.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.  
  This argument is ignored when \cr
  \code{sample.type="one.sample"}.
}
  \item{half.width}{
  positive numeric scalar indicating the half-width of the confidence interval.  
  The default value is \code{sigma.hat/2}.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed. 
  This argument is ignored if either \cr
  \code{x.var="half.width"} or \code{y.var="half.width"}.
}
  \item{sigma.hat}{
  positive numeric scalar specifying the estimated standard deviation.  
  The default value is \code{sigma.hat=1}.  
  This argument is ignored if \code{x.var="sigma.hat"}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level associated with the confidence interval.  
  The default value is \code{conf.level=0.95}.  This argument is ignored if \code{x.var="conf.level"}.
}
  \item{sample.type}{
  character string indicating whether this is a one-sample or two-sample confidence interval. \cr 
  When \code{sample.type="one.sample"}, the computations for the plot are based on a confidence 
  interval for a single mean.  \cr
  When \code{sample.type="two.sample"}, the computations for the 
  plot are based on a confidence interval for the difference between two means. \cr  
  The default value is \code{sample.type="one.sample"} unless the argument \code{n2} is supplied.
}
  \item{round.up}{
  logical scalar indicating whether to round up the computed sample sizes to the next smallest integer.  
  The default value is \code{round.up=FALSE}.  This argument is ignored unless \code{y.var="n"}.
}
  \item{n.max}{
  for the case when \code{y.var="n"}, positive integer greater than 1 
  specifying the maximum sample size for the single 
  group when \code{sample.type="one.sample"} or for group 1 when 
  \code{sample.type="two.sample"}.  The default value is \code{n.max=5000}.
}
  \item{tol}{
  for the case when \code{y.var="n"}, numeric scalar indicating the tolerance to 
  use in the \code{\link{uniroot}} search algorithm.  The default value is 
  \code{tol=1e-7}.
}
  \item{maxiter}{
  for the case when \code{y.var="n"}, positive integer indicating the maximum 
  number of iterations to use in the \code{\link{uniroot}} search algorithm.  
  The default value is \code{maxiter=1000}.
}
  \item{plot.it}{
  a logical scalar indicating whether to create a plot or add to the existing plot 
  (see explanation of the argument \code{add} below) on the current graphics device.  
  If \code{plot.it=FALSE}, no plot is produced, but a list of (x,y) values is returned 
  (see the section VALUE).  The default value is \code{plot.it=TRUE}.
}
  \item{add}{
  a logical scalar indicating whether to add the design plot to the existing plot (\code{add=TRUE}), 
  or to create a plot from scratch (\code{add=FALSE}).  The default value is \code{add=FALSE}.  
  This argument is ignored if \code{plot.it=FALSE}.
}
  \item{n.points}{
  a numeric scalar specifying how many (x,y) pairs to use to produce the plot.  
  There are \code{n.points} x-values evenly spaced between \code{range.x.var[1]} and \cr
  \code{range.x.var[2]}.  The default value is \code{n.points=100}.
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
  See the help files for \code{\link{ciNormHalfWidth}} and \code{\link{ciNormN}} 
  for information on how to compute a one-sample confidence interval for the mean of 
  a normal distribution or a two-sample confidence interval for the difference between 
  two means, how the half-width is computed when other quantities are fixed, and how the 
  sample size is computed when other quantities are fixed.
}
\value{
  \code{plotCiNormDesign} invisibly returns a list with components:

  \item{x.var}{x-coordinates of points that have been or would have been plotted.}
  \item{y.var}{y-coordinates of points that have been or would have been plotted.}
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). 
  \emph{Statistics for Environmental Engineers}.  Second Edition.   
  Lewis Publishers, Boca Raton, FL.

  Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution Monitoring}. 
  Van Nostrand Reinhold, New York, NY.

  Helsel, D.R., and R.M. Hirsch. (1992). 
  \emph{Statistical Methods in Water Resources Research}. 
  Elsevier, New York, NY, Chapter 7.

  Millard, S.P., and N. Neerchal. (2001).  \emph{Environmental Statistics with S-PLUS}.  
  CRC Press, Boca Raton, FL.

  Ott, W.R. (1995). \emph{Environmental Statistics and Data Analysis}. 
  Lewis Publishers, Boca Raton, FL.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C. p.21-3.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. 
  Fifth Edition. Prentice-Hall, Upper Saddle River, NJ, 
  Chapters 7 and 8.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The normal distribution and lognormal distribution are probably the two most frequently used 
  distributions to model environmental data.  In order to make any kind of probability 
  statement about a normally-distributed population (of chemical concentrations for example), 
  you have to first estimate the mean and standard deviation (the population parameters) of the 
  distribution.  Once you estimate these parameters, it is often useful to characterize the 
  uncertainty in the estimate of the mean.  This is done with confidence intervals.

  In the course of designing a sampling program, an environmental scientist may wish to determine 
  the relationship between sample size, confidence level, and half-width if one of the objectives 
  of the sampling program is to produce confidence intervals.  The functions 
  \code{\link{ciNormHalfWidth}}, \code{\link{ciNormN}}, and \code{plotCiNormDesign} 
  can be used to investigate these relationships for the case of normally-distributed observations.
}
\seealso{
  \code{\link{ciNormHalfWidth}}, \code{\link{ciNormN}}, \code{\link{Normal}}, 
  \code{\link{enorm}}, \code{\link{t.test}}, \cr
  \link{Estimating Distribution Parameters}.
}
\examples{
  # Look at the relationship between half-width and sample size 
  # for a one-sample confidence interval for the mean, assuming 
  # an estimated standard deviation of 1 and a confidence level of 95%.

  dev.new()
  plotCiNormDesign()

  #--------------------------------------------------------------------

  # Plot sample size vs. the estimated standard deviation for 
  # various levels of confidence, using a half-width of 0.5.

  dev.new()
  plotCiNormDesign(x.var = "sigma.hat", y.var = "n", main = "") 

  plotCiNormDesign(x.var = "sigma.hat", y.var = "n", conf.level = 0.9, 
    add = TRUE, plot.col = 2) 

  plotCiNormDesign(x.var = "sigma.hat", y.var = "n", conf.level = 0.8, 
    add = TRUE, plot.col = 3) 

  legend(0.25, 60, c("95\%", "90\%", "80\%"), lty = 1, lwd = 3, col = 1:3) 

  mtext("Sample Size vs. Estimated SD for Confidence Interval for Mean",
    font = 2, cex = 1.25, line = 2.75)
  mtext("with Half-Width=0.5 and Various Confidence Levels", font = 2, 
    cex = 1.25, line = 1.25)

  #--------------------------------------------------------------------

  # Modifying the example on pages 21-4 to 21-5 of USEPA (2009), 
  # look at the relationship between half-width and sample size for a 
  # 95% confidence interval for the mean level of Aldicarb at the 
  # first compliance well.  Use the estimated standard deviation from 
  # the first four months of data. 
  # (The data are stored in EPA.09.Ex.21.1.aldicarb.df.)

  EPA.09.Ex.21.1.aldicarb.df
  #   Month   Well Aldicarb.ppb
  #1      1 Well.1         19.9
  #2      2 Well.1         29.6
  #3      3 Well.1         18.7
  #4      4 Well.1         24.2
  #...

  mu.hat <- with(EPA.09.Ex.21.1.aldicarb.df, 
    mean(Aldicarb.ppb[Well=="Well.1"]))

  mu.hat 
  #[1] 23.1 

  sigma.hat <- with(EPA.09.Ex.21.1.aldicarb.df, 
    sd(Aldicarb.ppb[Well=="Well.1"]))

  sigma.hat 
  #[1] 4.93491 

  dev.new()
  plotCiNormDesign(sigma.hat = sigma.hat, digits = 2, 
    range.x.var = c(2, 25))

  #==========

  # Clean up
  #---------
  rm(mu.hat, sigma.hat)
  graphics.off()
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }

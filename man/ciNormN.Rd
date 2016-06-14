\name{ciNormN}
\alias{ciNormN}
\title{
  Sample Size for Specified Half-Width of Confidence Interval for Normal Distribution Mean or Difference Between Two Means
}
\description{
  Compute the sample size necessary to achieve a specified half-width of a 
  confidence interval for the mean of a normal distribution or the difference 
  between two means, given the estimated standard deviation and confidence level.
}
\usage{
  ciNormN(half.width, sigma.hat = 1, conf.level = 0.95, 
    sample.type = ifelse(is.null(n2), "one.sample", "two.sample"), 
    n2 = NULL, round.up = TRUE, n.max = 5000, tol = 1e-07, maxiter = 1000)
}
\arguments{
  \item{half.width}{
  numeric vector of (positive) half-widths.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
}
  \item{sigma.hat}{
  numeric vector specifying the value(s) of the estimated standard deviation(s).
}
  \item{conf.level}{
  numeric vector of numbers between 0 and 1 indicating the confidence level 
  associated with the confidence interval(s).  The default value is \code{conf.level=0.95}.
}
  \item{sample.type}{
  character string indicating whether this is a one-sample \cr
  (\code{sample.type="one.sample"}) or two-sample \cr 
  (\code{sample.type="two.sample"}) confidence interval.  \cr
  When \code{sample.type="one.sample"}, the computed sample size is based on 
  a confidence interval for a single mean.  \cr
  When \code{sample.type="two.sample"}, the computed sample size is based on 
  a confidence interval for the difference between two means.  \cr
  The default value is \code{sample.type="one.sample"} unless the argument 
  \code{n2} is supplied.
}
  \item{n2}{
  numeric vector of sample sizes for group 2.  The default value is \code{NULL}, 
  in which case it is assumed that the sample sizes for groups 1 and 2 are equal.  
  This argument is ignored when \code{sample.type="one.sample"}.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed. 
}
  \item{round.up}{
  logical scalar indicating whether to round up the values of the computed sample size(s) 
  to the next smallest integer.  The default value is \code{round.up=TRUE}.
}
  \item{n.max}{
  positive integer greater than 1 specifying the maximum sample size for the single 
  group when \code{sample.type="one.sample"} or for group 1 when \cr
  \code{sample.type="two.sample"}.  The default value is \code{n.max=5000}.
}
  \item{tol}{
  numeric scalar indicating the tolerance to use in the \code{\link{uniroot}} 
  search algorithm.  The default value is \code{tol=1e-7}.
}
  \item{maxiter}{
  positive integer indicating the maximum number of iterations to use in the 
  \code{\link{uniroot}} search algorithm.  The default value is 
  \code{maxiter=1000}.
}
}
\details{
  If the arguments \code{half.width}, \code{n2}, \code{sigma.hat}, and 
  \code{conf.level} are not all the same length, they are replicated to be the same length as 
  the length of the longest argument.

  The function \code{ciNormN} uses the formulas given in the help file for 
  \code{\link{ciNormHalfWidth}} for the half-width of the confidence interval 
  to iteratively solve for the sample size.  For the two-sample case, the default 
  is to assume equal sample sizes for each group unless the argument \code{n2} 
  is supplied.
}
\value{
  When \code{sample.type="one.sample"}, or \code{sample.type="two.sample"} and \code{n2} 
  is not supplied (so equal sample sizes for each group is assumed), 
  the function \code{ciNormN} returns a numeric vector of sample sizes.  
  When \code{sample.type="two.sample"} and \code{n2} is supplied, 
  the function \code{ciNormN} returns a list with two components called \code{n1} and \code{n2}, 
  specifying the sample sizes for each group.
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
  \code{\link{ciNormHalfWidth}}, \code{\link{ciNormN}}, and \code{\link{plotCiNormDesign}} 
  can be used to investigate these relationships for the case of normally-distributed observations.
}
\seealso{
  \code{\link{ciNormHalfWidth}}, \code{\link{plotCiNormDesign}}, \code{\link{Normal}}, 
  \code{\link{enorm}}, \code{\link{t.test}}, \cr
  \link{Estimating Distribution Parameters}.
}
\examples{
  # Look at how the required sample size for a one-sample 
  # confidence interval decreases with increasing half-width:

  seq(0.25, 1, by = 0.25) 
  #[1] 0.25 0.50 0.75 1.00 

  ciNormN(half.width = seq(0.25, 1, by = 0.25)) 
  #[1] 64 18 10 7 

  ciNormN(seq(0.25, 1, by=0.25), round = FALSE) 
  #[1] 63.897899 17.832337  9.325967  6.352717

  #----------------------------------------------------------------

  # Look at how the required sample size for a one-sample 
  # confidence interval increases with increasing estimated 
  # standard deviation for a fixed half-width:

  seq(0.5, 2, by = 0.5) 
  #[1] 0.5 1.0 1.5 2.0 

  ciNormN(half.width = 0.5, sigma.hat = seq(0.5, 2, by = 0.5)) 
  #[1] 7 18 38 64

  #----------------------------------------------------------------

  # Look at how the required sample size for a one-sample 
  # confidence interval increases with increasing confidence 
  # level for a fixed half-width:

  seq(0.5, 0.9, by = 0.1) 
  #[1] 0.5 0.6 0.7 0.8 0.9 

  ciNormN(half.width = 0.25, conf.level = seq(0.5, 0.9, by = 0.1)) 
  #[1] 9 13 19 28 46

  #----------------------------------------------------------------

  # Modifying the example on pages 21-4 to 21-5 of USEPA (2009), 
  # determine the required sample size in order to achieve a 
  # half-width that is 10% of the observed mean (based on the first 
  # four months of observations) for the Aldicarb level at the first 
  # compliance well.  Assume a 95% confidence level and use the 
  # estimated standard deviation from the first four months of data. 
  # (The data are stored in EPA.09.Ex.21.1.aldicarb.df.) 
  #
  # The required sample size is 20, so almost two years of data are 
  # required assuming observations are taken once per month.

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

  ciNormN(half.width = 0.1 * mu.hat, sigma.hat = sigma.hat) 
  #[1] 20

  #----------
  # Clean up
  rm(mu.hat, sigma.hat)
}
\keyword{ distribution }
\keyword{ design }
\keyword{ htest }

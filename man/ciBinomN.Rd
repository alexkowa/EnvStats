\name{ciBinomN}
\alias{ciBinomN}
\title{
  Sample Size for Specified Half-Width of Confidence Interval for Binomial Proportion or Difference Between Two Proportions
}
\description{
  Compute the sample size necessary to achieve a specified half-width of a 
  confidence interval for a binomial proportion or the difference between 
  two proportions, given the estimated proportion(s), and confidence level.
}
\usage{
  ciBinomN(half.width, p.hat.or.p1.hat = 0.5, p2.hat = 0.4, 
    conf.level = 0.95, sample.type = "one.sample", ratio = 1, 
    ci.method = "score", correct = TRUE, warn = TRUE, 
    n.or.n1.min = 2, n.or.n1.max = 10000, 
    tol.half.width = 5e-04, tol.p.hat = 5e-04, 
    tol = 1e-7, maxiter = 1000)
}
\arguments{
  \item{half.width}{
  numeric vector of (positive) half-widths.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
}
  \item{p.hat.or.p1.hat}{
  numeric vector of estimated proportions.  \cr
  When \code{sample.type="one.sample"}, \code{p.hat.or.p1.hat} denotes the 
  estimated value of \eqn{p}, the probability of \dQuote{success}. \cr 
  When \code{sample.type="two.sample"}, \code{p.hat.or.p1.hat} denotes the estimated value of 
  \eqn{p_1}, the probability of \dQuote{success} in group 1.  \cr
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
}
  \item{p2.hat}{
  numeric vector of estimated proportions for group 2. 
  This argument is ignored when \code{sample.type="one.sample"}.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
}
  \item{conf.level}{
  numeric vector of numbers between 0 and 1 indicating the confidence level associated with 
  the confidence interval(s).  The default value is \code{conf.level=0.95}.
}
  \item{sample.type}{
  character string indicating whether this is a one-sample or two-sample confidence interval. \cr
  When \code{sample.type="one.sample"}, the computed half-width is based on a confidence interval 
  for a single proportion.  \cr
  When \code{sample.type="two.sample"}, the computed half-width is based 
  on a confidence interval for the difference between two proportions. \cr
  The default value is \code{sample.type="one.sample"} unless the 
  argument \code{p2.hat} or \code{ratio} is supplied.
}
  \item{ratio}{
  numeric vector indicating the ratio of sample size in group 2 to 
  sample size in group 1 (\eqn{n_2/n_1}). The default value is \code{ratio=1}.  
  All values of \code{ratio} must be greater than or equal to 1.  
  This argument is ignored if \cr 
  \code{sample.type="one.sample"}.
}
  \item{ci.method}{
  character string indicating which method to use to construct the 
  confidence interval.  Possible values are:
  \itemize{
    \item \code{"score"} (the default),
    \item \code{"exact"},
    \item \code{"adjusted Wald"} and,
    \item \code{"Wald"} (the \code{"Wald"} method is \strong{never} recommended but 
      is included for historical purposes).
  }
  The exact method is only available for the one-sample case, i.e., when \cr
  \code{sample.type="one.sample"}.
}
  \item{correct}{
  logical scalar indicating whether to use the continuity correction when \cr 
  \code{ci.method="score"} or \code{ci.method="Wald"}.  
  The default value is \cr
  \code{correct=TRUE}. 
}
  \item{warn}{
  logical scalar indicating whether to issue a warning when \code{ci.method="Wald"} 
  for cases when the normal approximation to the binomial distribution probably is not accurate.  
  The default value is \code{warn=TRUE}.
}
  \item{n.or.n1.min}{
  integer indicating the minimum allowed value for \cr 
  \eqn{n} (\code{sample.type="one.sample"}) or \cr 
  \eqn{n_1} (\code{sample.type="two.sample"}).  \cr
  The default value is \code{n.or.n1.min=2}. 
}
  \item{n.or.n1.max}{
  integer indicating the maximum allowed value for \cr
  \eqn{n} (\code{sample.type="one.sample"}) or \cr
  \eqn{n_1} (\code{sample.type="two.sample"}).  \cr
  The default value is \code{n.or.n1.max=10000}. 
}
  \item{tol.half.width}{
  numeric scalar indicating the tolerance to use for the half width for
  the search algorithm.  The sample sizes are computed so that the actual 
  half width is less than or equal to \code{half.width + tol.half.width}.  
  The default value is \code{tol.half.width=5e-04}.
}
  \item{tol.p.hat}{
  numeric scalar indicating the tolerance to use for the estimated 
  proportion(s) for the search algorithm.  
  For the one-sample case, the sample sizes are computed so that 
  the absolute value of the difference between the user supplied 
  value of \code{p.hat.or.p1.hat} and the actual estimated proportion is 
  less than or equal to \code{tol.p.hat}.  
  For the two-sample case, the sample sizes are computed so that 
  the absolute value of the difference between the user supplied 
  value of \code{p.hat.or.p1.hat} and the actual estimated proportion 
  for group 1 is less than or equal to \code{tol.p.hat}, and the 
  absolute value of the difference between the user supplied 
  value of \code{p2.hat} and the actual estimated proportion 
  for group 2 is less than or equal to \code{tol.p.hat}.  
  The default value is \code{tol.p.hat=0.005}. 
}
  \item{tol}{
  positive scalar indicating the tolerance to use for the search algorithm 
  (passed to \code{uniroot}).  The default value is \code{tol=1e-7}.
}
  \item{maxiter}{
  integer indicating the maximum number of iterations to use for 
  the search algorithm (passed to \code{uniroot}).  
  The default value is \code{maxiter=1000}.
}
}
\details{
  If the arguments \code{half.width}, \code{p.hat.or.p1.hat}, \code{p2.hat}, 
  \code{conf.level} and \code{ratio} are not all the same length, they are 
  replicated to be the same length as the length of the longest argument.  

  For the one-sample case, the arguments \code{p.hat.or.p1.hat}, \code{tol.p.hat}, 
  \code{half.width}, and \cr 
  \code{tol.half.width} must satisfy: \cr  
  \code{(p.hat.or.p1.hat + tol.p.hat + half.width + tol.half.width) <= 1}, \cr
  and \cr 
  \code{(p.hat.or.p1.hat - tol.p.hat - half.width - tol.half.width) >= 0}.

  For the two-sample case, the arguments \code{p.hat.or.p1.hat}, \code{p2.hat}, 
  \code{tol.p.hat}, \cr
  \code{half.width}, and \code{tol.half.width} must satisfy:  \cr
  \code{((p.hat.or.p1.hat + tol.p.hat) - (p2.hat - tol.p.hat) + half.width + tol.half.width) <= 1}, and \cr
  \code{((p.hat.or.p1.hat - tol.p.hat) - (p2.hat + tol.p.hat) - half.width - tol.half.width) >= -1}.

  The function \code{ciBinomN} uses the search algorithm in the 
  function \code{\link{uniroot}} to call the function 
  \code{\link{ciBinomHalfWidth}} to find the values of 
  \eqn{n} (\code{sample.type="one.sample"}) or
  \eqn{n_1} and \eqn{n_2} \cr
  (\code{sample.type="two.sample"}) that satisfy the requirements for the half-width, 
  estimated proportions, and confidence level.  See the Details section of the help file for 
  \code{\link{ciBinomHalfWidth}} for more information.  
}
\value{
  a list with information about the sample sizes, estimated proportions, 
  and half-widths.  

  \strong{One-Sample Case (\code{sample.type="one.sample"})}. \cr  
  When \code{sample.type="one.sample"}, the function \code{ciBinomN} 
  returns a list with these components:

  \item{n}{the sample size(s) associated with the confidence interval(s)}
  \item{p.hat}{the estimated proportion(s)}
  \item{half.width}{the half-width(s) of the confidence interval(s)}
  \item{method}{the method used to construct the confidence interval(s)}


  \strong{Two-Sample Case (\code{sample.type="two.sample"})}.  \cr 
  When \code{sample.type="two.sample"}, the function \code{ciBinomN} 
  returns a list with these components:

  \item{n1}{the sample size(s) for group 1 associated with the confidence interval(s)}
  \item{n2}{the sample size(s) for group 2 associated with the confidence interval(s)}
  \item{p1.hat}{the estimated proportion(s) for group 1}
  \item{p2.hat}{the estimated proportion(s) for group 2}
  \item{half.width}{the half-width(s) of the confidence interval(s)}
  \item{method}{the method used to construct the confidence interval(s)}
}
\references{
  Agresti, A., and B.A. Coull. (1998). Approximate is Better than "Exact" for Interval Estimation 
  of Binomial Proportions. \emph{The American Statistician}, \bold{52}(2), 119--126.

  Agresti, A., and B. Caffo. (2000). Simple and Effective Confidence Intervals for Proportions 
  and Differences of Proportions Result from Adding Two Successes and Two Failures. \emph{The 
  American Statistician}, \bold{54}(4), 280--288.

  Berthouex, P.M., and L.C. Brown. (1994). \emph{Statistics for Environmental Engineers}. 
  Lewis Publishers, Boca Raton, FL, Chapters 2 and 15.

  Cochran, W.G. (1977). \emph{Sampling Techniques}. John Wiley and Sons, New York, Chapter 3.

  Fisher, R.A., and F. Yates. (1963). 
  \emph{Statistical Tables for Biological, Agricultural, and Medical Research}. 6th edition. 
  Hafner, New York, 146pp.

  Fleiss, J. L. (1981). \emph{Statistical Methods for Rates and Proportions}. Second Edition. 
  John Wiley and Sons, New York, Chapters 1-2.

  Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution Monitoring}. 
  Van Nostrand Reinhold, New York, NY, Chapter 11.

  Millard, S.P., and Neerchal, N.K. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton, Florida.

  Newcombe, R.G. (1998a). Two-Sided Confidence Intervals for the Single Proportion:  Comparison of 
  Seven Methods. \emph{Statistics in Medicine}, \bold{17}, 857--872.

  Newcombe, R.G. (1998b). Interval Estimation for the Difference Between Independent Proportions:  
  Comparison of Eleven Methods. \emph{Statistics in Medicine}, \bold{17}, 873--890.

  Ott, W.R. (1995). \emph{Environmental Statistics and Data Analysis}. 
  Lewis Publishers, Boca Raton, FL, Chapter 4.

  USEPA. (1989b). \emph{Statistical Analysis of Ground-Water Monitoring Data at RCRA Facilities, Interim Final Guidance}. 
  EPA/530-SW-89-026. Office of Solid Waste, U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C. p.6-38.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition. 
  Prentice-Hall, Upper Saddle River, NJ, Chapter 24. 
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The binomial distribution is used to model processes with binary 
  (Yes-No, Success-Failure, Heads-Tails, etc.) outcomes.  It is assumed that the outcome of any 
  one trial is independent of any other trial, and that the probability of \dQuote{success}, \eqn{p}, 
  is the same on each trial.  A binomial discrete random variable \eqn{X} is the number of 
  \dQuote{successes} in \eqn{n} independent trials.  A special case of the binomial distribution 
  occurs when \eqn{n=1}, in which case \eqn{X} is also called a Bernoulli random variable.

  In the context of environmental statistics, the binomial distribution is sometimes used to model 
  the proportion of times a chemical concentration exceeds a set standard in a given period of time 
  (e.g., Gilbert, 1987, p.143), or to compare the proportion of detects in a compliance well vs. a 
  background well (e.g., USEPA, 1989b, Chapter 8, p.3-7).  (However, USEPA 2009, p.8-27 
  recommends using the Wilcoxon rank sum test (\code{\link{wilcox.test}}) instead of 
  comparing proportions.)

  In the course of designing a sampling program, an environmental scientist may wish to determine 
  the relationship between sample size, confidence level, and half-width if one of the objectives of 
  the sampling program is to produce confidence intervals.  The functions \code{\link{ciBinomHalfWidth}}, 
  \code{\link{ciBinomN}}, and \code{\link{plotCiBinomDesign}} can be used to investigate these 
  relationships for the case of binomial proportions.
}
\seealso{
  \code{\link{ciBinomHalfWidth}}, \code{\link{uniroot}}, 
  \code{\link{plotCiBinomDesign}}, \code{\link{ebinom}}, \cr
  \code{\link{binom.test}}, \code{\link{prop.test}}.
}
\examples{
  # Look at how the required sample size of a one-sample 
  # confidence interval increases with decreasing 
  # required half-width:

  ciBinomN(half.width = c(0.1, 0.05, 0.03))
  #$n
  #[1]   92  374 1030
  #
  #$p.hat
  #[1] 0.5 0.5 0.5
  #
  #$half.width
  #[1] 0.10010168 0.05041541 0.03047833
  #
  #$method
  #[1] "Score normal approximation, with continuity correction"

  #----------

  # Note that the required sample size decreases if we are less 
  # stringent about how much the confidence interval width can 
  # deviate from the supplied value of the 'half.width' argument:

  ciBinomN(half.width = c(0.1, 0.05, 0.03), tol.half.width = 0.005)
  #$n
  #[1]  84 314 782
  #
  #$p.hat
  #[1] 0.5 0.5 0.5
  #
  #$half.width
  #[1] 0.10456066 0.05496837 0.03495833
  #
  #$method
  #[1] "Score normal approximation, with continuity correction"

  #--------------------------------------------------------------------

  # Look at how the required sample size for a one-sample 
  # confidence interval tends to decrease as the estimated 
  # value of p decreases below 0.5 or increases above 0.5:

  seq(0.2, 0.8, by = 0.1) 
  #[1] 0.2 0.3 0.4 0.5 0.6 0.7 0.8 

  ciBinomN(half.width = 0.1, p.hat = seq(0.2, 0.8, by = 0.1)) 
  #$n
  #[1]  70  90 100  92 100  90  70
  #
  #$p.hat
  #[1] 0.2 0.3 0.4 0.5 0.6 0.7 0.8
  #
  #$half.width
  #[1] 0.09931015 0.09839843 0.09910818 0.10010168 0.09910818 0.09839843
  #[7] 0.09931015
  #
  #$method
  #[1] "Score normal approximation, with continuity correction"

  #----------------------------------------------------------------

  # Look at how the required sample size for a one-sample 
  # confidence interval increases with increasing confidence level:

  ciBinomN(half.width = 0.05, conf.level = c(0.8, 0.9, 0.95, 0.99)) 
  #$n
  #[1] 160 264 374 644
  #
  #$p.hat
  #[1] 0.5 0.5 0.5 0.5
  #
  #$half.width
  #[1] 0.05039976 0.05035948 0.05041541 0.05049152
  #
  #$method
  #[1] "Score normal approximation, with continuity correction"

  #----------------------------------------------------------------

  # Compare required sample size for a one-sample 
  # confidence interval based on the different methods:

  ciBinomN(half.width = 0.05, ci.method = "score")
  #$n
  #[1] 374
  #
  #$p.hat
  #[1] 0.5
  #
  #$half.width
  #[1] 0.05041541
  #
  #$method
  #[1] "Score normal approximation, with continuity correction"

  ciBinomN(half.width = 0.05, ci.method = "exact")
  #$n
  #[1] 394
  #
  #$p.hat
  #[1] 0.5
  #
  #$half.width
  #[1] 0.05047916
  #
  #$method
  #[1] "Exact"

  ciBinomN(half.width = 0.05, ci.method = "adjusted Wald")
  #$n
  #[1] 374
  #
  #$p.hat
  #[1] 0.5
  #
  #$half.width
  #[1] 0.05041541
  #
  #$method
  #[1] "Adjusted Wald normal approximation"

  ciBinomN(half.width = 0.05, ci.method = "Wald")
  #$n
  #[1] 398
  #
  #$p.hat
  #[1] 0.5
  #
  #$half.width
  #[1] 0.05037834
  #
  #$method
  #[1] "Wald normal approximation, with continuity correction"

  #----------------------------------------------------------------

  \dontrun{
  # Look at how the required sample size of a two-sample 
  # confidence interval increases with decreasing 
  # required half-width:

  ciBinomN(half.width = c(0.1, 0.05, 0.03), sample.type = "two")  
  #$n1
  #[1]  210  778 2089
  #
  #$n2
  #[1]  210  778 2089
  #
  #$p1.hat
  #[1] 0.5000000 0.5000000 0.4997607
  #
  #$p2.hat
  #[1] 0.4000000 0.3997429 0.4001915
  #
  #$half.width
  #[1] 0.09943716 0.05047044 0.03049753
  #
  #$method
  #[1] "Score normal approximation, with continuity correction"
  }
}
\keyword{ design }
\keyword{ distribution }
\keyword{ htest }

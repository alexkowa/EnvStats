\name{gpqTolIntNormCensored}
\alias{gpqTolIntNormCensored}
\alias{gpqTolIntNormSinglyCensored}
\alias{gpqTolIntNormMultiplyCensored}
\title{
  Generalized Pivotal Quantity for Tolerance Interval for a Normal Distribution Based on Censored Data
}
\description{
  Generate a generalized pivotal quantity (GPQ) for a tolerance interval for a Normal 
  distribution based on singly or multiply censored data.
}
\usage{
  gpqTolIntNormSinglyCensored(n, n.cen, p, probs, nmc, method = "mle", 
    censoring.side = "left", seed = NULL, names = TRUE)

  gpqTolIntNormMultiplyCensored(n, cen.index, p, probs, nmc, method = "mle", 
    censoring.side = "left", seed = NULL, names = TRUE)
}

\arguments{
  \item{n}{
  positive integer \eqn{\ge 3} indicating the sample size.
}
  \item{n.cen}{
  for the case of singly censored data, a positive integer indicating the number of 
  censored observations.  The value of \code{n.cen} must be between \code{1} and 
  \code{n-2}, inclusive.
}
  \item{cen.index}{
  for the case of multiply censored data, a sorted vector of unique integers indicating the 
  indices of the censored observations when the observations are \dQuote{ordered}.  
  The length of \code{cen.index} must be between \code{1} and \code{n-2}, inclusive, and 
  the values of \code{cen.index} must be between \code{1} and \code{n}.
}
  \item{p}{
  numeric scalar strictly greater than 0 and strictly less than 1 indicating the quantile 
  for which to generate the GPQ(s) (i.e., the coverage associated with a one-sided 
  tolerance interval).
}
  \item{probs}{
  numeric vector of values between 0 and 1 indicating the confidence level(s) associated 
  with the GPQ(s).
}
  \item{nmc}{
  positive integer \eqn{\ge 10} indicating the number of Monte Carlo trials to run in order 
  to compute the GPQ(s). 
}
  \item{method}{
  character string indicating the method to use for parameter estimation.  \cr
  \cr
  For singly censored data, possible values are \code{"mle"} (the default), \code{"bcmle"}, 
  \code{"qq.reg"}, \code{"qq.reg.w.cen.level"}, \code{"impute.w.qq.reg"}, \cr
  \code{"impute.w.qq.reg.w.cen.level"}, \code{"impute.w.mle"}, \cr
  \code{"iterative.impute.w.qq.reg"}, 
  \code{"m.est"}, and \code{"half.cen.level"}.  See the help file for \code{\link{enormCensored}} 
  for details. \cr
  \cr
  For multiply censored data, possible values are \code{"mle"} (the default), \code{"qq.reg"}, 
  \code{"impute.w.qq.reg"}, and \code{"half.cen.level"}.  See the help file for \cr
  \code{\link{enormCensored}} for details.
}
  \item{censoring.side}{
  character string indicating on which side the censoring occurs.  The possible values are 
  \code{"left"} (the default) and \code{"right"}. 
}
  \item{seed}{
  positive integer to pass to the function \code{\link{set.seed}}.  This argument is 
  ignored if \code{seed=NULL} (the default).  Using the \code{seed} argument lets you 
  reproduce the exact same result if all other arguments stay the same.
}
  \item{names}{
  a logical scalar passed to \code{\link{quantile}} indicating whether to add a 
  names attribute to the resulting GPQ(s).  The default value is \code{names=TRUE}.
}
}
\details{
  The function \code{gpqTolIntNormSinglyCensored} generates GPQs as described in Algorithm 12.1 
  of Krishnamoorthy and Mathew (2009, p. 329).  The function 
  \code{gpqTolIntNormMultiplyCensored} is an extension of this idea to multiply censored data.  
  These functions are called by \cr
  \code{\link{tolIntNormCensored}} when \code{ti.method="gpq"}, 
  and also by \code{\link{eqnormCensored}} when \code{ci=TRUE} and \code{ci.method="gpq"}.  See 
  the help files for these functions for an explanation of GPQs.

  Note that technically these are only GPQs if the data are Type II censored.  However, 
  Krishnamoorthy and Mathew (2009, p. 328) state that in the case of Type I censored data these 
  quantities should approximate the true GPQs and the results appear to be satisfactory, even 
  for small sample sizes.

  The functions \code{gpqTolIntNormSinglyCensored} and \code{gpqTolIntNormMultiplyCensored} are 
  computationally intensive and provided to the user to allow you to create your own tables.
}
\value{
  a numeric vector containing the GPQ(s).
}
\references{
  Krishnamoorthy K., and T. Mathew. (2009). 
  \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}. 
  John Wiley and Sons, Hoboken.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Tolerance intervals have long been applied to quality control and 
  life testing problems (Hahn, 1970b,c; Hahn and Meeker, 1991; Krishnamoorthy and Mathew, 2009).  
  References that discuss tolerance intervals in the context of environmental monitoring include:  
  Berthouex and Brown (2002, Chapter 21), Gibbons et al. (2009), 
  Millard and Neerchal (2001, Chapter 6), Singh et al. (2010b), and USEPA (2009).
}
\seealso{
  \code{\link{tolIntNormCensored}}, \code{\link{eqnormCensored}},  
  \code{\link{enormCensored}}, \code{\link{estimateCensored.object}}.
}
\examples{
  # Reproduce the entries for n=10 observations with n.cen=1 in Table 12.2 
  # of Krishnamoorthy and Mathew (2009, p.331).
  #
  # (Note: passing a value for the argument "seed" simply allows you to 
  # reproduce this example.)  
  #
  # NOTE:  Here to save computing time for the sake of example, we will specify 
  #        just 100 Monte Carlos, whereas Krishnamoorthy and Mathew (2009) 
  #        suggest *10,000* Monte Carlos.

  gpqTolIntNormSinglyCensored(n = 10, n.cen = 1, p = 0.05, probs = 0.05, 
    nmc = 100, seed = 529)
  #       5% 
  #-3.483403


  gpqTolIntNormSinglyCensored(n = 10, n.cen = 1, p = 0.1, probs = 0.05, 
    nmc = 100, seed = 497)
  #      5% 
  #-2.66705


  gpqTolIntNormSinglyCensored(n = 10, n.cen = 1, p = 0.9, probs = 0.95, 
    nmc = 100, seed = 623)
  #     95% 
  #2.478654

  gpqTolIntNormSinglyCensored(n = 10, n.cen = 1, p = 0.95, probs = 0.95, 
    nmc = 100, seed = 623)
  #     95% 
  #3.108452

  #==========

  # Example of using gpqTolIntNormMultiplyCensored
  #-----------------------------------------------

  # Consider the following set of multiply left-censored data:
  dat <- 12:16
  censored <- c(TRUE, FALSE, TRUE, FALSE, FALSE)

  # Since the data are "ordered" we can identify the indices of the 
  # censored observations in the ordered data as follow:

  cen.index <- (1:length(dat))[censored]
  cen.index
  #[1] 1 3

  # Now we can generate a GPQ using gpqTolIntNormMultiplyCensored.
  # Here we'll generate a GPQ corresponding to an upper tolerance 
  # interval with coverage 90% with 95% confidence for 
  # left-censored data.
  # NOTE:  Here to save computing time for the sake of example, we will specify 
  #        just 100 Monte Carlos, whereas Krishnamoorthy and Mathew (2009) 
  #        suggest *10,000* Monte Carlos.

  gpqTolIntNormMultiplyCensored(n = 5, cen.index = cen.index, p = 0.9, 
    probs = 0.95, nmc = 100, seed = 237)
  #     95% 
  #3.952052

  #==========

  # Clean up
  #---------
  rm(dat, censored, cen.index)
}
\keyword{ distribution }
\keyword{ htest }

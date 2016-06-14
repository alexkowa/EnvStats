\name{gpqCiNormCensored}
\alias{gpqCiNormCensored}
\alias{gpqCiNormSinglyCensored}
\alias{gpqCiNormMultiplyCensored}
\title{
  Generalized Pivotal Quantity for Confidence Interval for the Mean of a Normal Distribution Based on Censored Data
}
\description{
  Generate a generalized pivotal quantity (GPQ) for a confidence interval for the 
  mean of a \link[stats:Normal]{Normal distribution} based on singly or multiply 
  censored data.
}
\usage{
  gpqCiNormSinglyCensored(n, n.cen, probs, nmc, method = "mle", 
    censoring.side = "left", seed = NULL, names = TRUE)

  gpqCiNormMultiplyCensored(n, cen.index, probs, nmc, method = "mle", 
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
  for the case of multiply censored data, a sorted vector of unique integers 
  indicating the indices of the censored observations when the observations are 
  \dQuote{ordered}.  The length of \code{cen.index} must be between \code{1} and 
  \code{n-2}, inclusive, and the values of \code{cen.index} must be between 
  \code{1} and \code{n}.
}
  \item{probs}{
  numeric vector of values between 0 and 1 indicating the confidence level(s) 
  associated with the GPQ(s).
}
  \item{nmc}{
  positive integer \eqn{\ge 10} indicating the number of Monte Carlo trials to run 
  in order to compute the GPQ(s). 
}
  \item{method}{
  character string indicating the method to use for parameter estimation.  \cr
  \cr
  For singly censored data, possible values are \code{"mle"} (the default), 
  \code{"bcmle"}, \code{"qq.reg"}, \code{"qq.reg.w.cen.level"}, 
  \code{"impute.w.qq.reg"}, \cr
  \code{"impute.w.qq.reg.w.cen.level"}, 
  \code{"impute.w.mle"}, \cr
  \code{"iterative.impute.w.qq.reg"}, 
  \code{"m.est"}, and \code{"half.cen.level"}.  See the help file for 
  \code{\link{enormCensored}} for details. \cr
  \cr
  For multiply censored data, possible values are \code{"mle"} (the default), 
  \code{"qq.reg"}, \code{"impute.w.qq.reg"}, and \code{"half.cen.level"}.  
  See the help file for \code{\link{enormCensored}} for details.
}
  \item{censoring.side}{
  character string indicating on which side the censoring occurs.  The possible 
  values are \code{"left"} (the default) and \code{"right"}. 
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
  The functions \code{gpqCiNormSinglyCensored} and \code{gpqCiNormMultiplyCensored} 
  are called by \cr
  \code{\link{enormCensored}} when \code{ci.method="gpq"}.  They are 
  used to construct generalized pivotal quantities to create confidence intervals 
  for the mean \eqn{\mu} of an assumed normal distribution.

  This idea was introduced by Schmee et al. (1985) in the context of Type II singly 
  censored data.  The function 
  \code{gpqCiNormSinglyCensored} generates GPQs using a modification of 
  Algorithm 12.1 of Krishnamoorthy and Mathew (2009, p. 329).  Algorithm 12.1 is 
  used to generate GPQs for a tolerance interval.  The modified algorithm for 
  generating GPQs for confidence intervals for the mean \eqn{\mu} is as follows:
  \enumerate{
    \item Generate a random sample of \eqn{n} observations from a standard normal 
      (i.e., N(0,1)) distribution and let \eqn{z_{(1)}, z_{(2)}, \ldots, z_{(n)}} 
      denote the ordered (sorted) observations.
    \item Set the smallest \code{n.cen} observations as censored.
    \item Compute the estimates of \eqn{\mu} and \eqn{\sigma} by calling 
      \code{\link{enormCensored}} using the method 
      specified by the \code{method} argument, and denote these estimates as 
      \eqn{\hat{\mu}^*, \; \hat{\sigma}^*}.
    \item Compute the t-like pivotal quantity 
      \eqn{\hat{t} = \hat{\mu}^*/\hat{\sigma}^*}.
    \item Repeat steps 1-4 \code{nmc} times to produce an empirical distribution of 
      the t-like pivotal quantity.
  }
  A two-sided \eqn{(1-\alpha)100\%} confidence interval for \eqn{\mu} is then 
  computed as:
  \deqn{[\hat{\mu} - \hat{t}_{1-(\alpha/2)} \hat{\sigma}, \; \hat{\mu} - \hat{t}_{\alpha/2} \hat{\sigma}]}
  where \eqn{\hat{t}_p} denotes the \eqn{p}'th empirical quantile of the 
  \code{nmc} generated \eqn{\hat{t}} values.

  Schmee at al. (1985) derived this method in the context of Type II singly censored 
  data (for which these limits are exact within Monte Carlo error), but state that 
  according to Regal (1982) this method produces confidence intervals that are 
  close apporximations to the correct limits for Type I censored data.

  The function 
  \code{gpqCiNormMultiplyCensored} is an extension of this idea to multiply censored 
  data.  The algorithm is the same as for singly censored data, except 
  Step 2 changes to: \cr

  2. Set observations as censored for elements of the argument \code{cen.index} 
     that have the value \code{TRUE}.

  The functions \code{gpqCiNormSinglyCensored} and \code{gpqCiNormMultiplyCensored} are 
  computationally intensive and provided to the user to allow you to create your own 
  tables.
}
\value{
  a numeric vector containing the GPQ(s).
}
\references{
  Krishnamoorthy K., and T. Mathew. (2009). 
  \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}. 
  John Wiley and Sons, Hoboken.

  Regal, R. (1982).  Applying Order Statistic Censored Normal Confidence Intervals 
  to Time Censored Data.  Unpublished manuscript, University of Minnesota, Duluth, 
  Department of Mathematical Sciences.

  Schmee, J., D.Gladstein, and W. Nelson. (1985).  Confidence Limits for Parameters 
  of a Normal Distribution from Singly Censored Samples, Using Maximum Likelihood.  
  \emph{Technometrics} \bold{27}(2) 119--128.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\seealso{
  \code{\link{enormCensored}}, \code{\link{estimateCensored.object}}.
}
\examples{
  # Reproduce the entries for n=10 observations with n.cen=6 in Table 4 
  # of Schmee et al. (1985, p.122).
  #
  # Notes: 
  # 1. This table applies to right-censored data, and the 
  #    quantity "r" in this table refers to the number of 
  #    uncensored observations.
  #
  # 2. Passing a value for the argument "seed" simply allows 
  #    you to reproduce this example.  

  # NOTE:  Here to save computing time for the sake of example, we will specify 
  #        just 100 Monte Carlos, whereas Krishnamoorthy and Mathew (2009) 
  #        suggest *10,000* Monte Carlos.

  # Here are the values given in Schmee et al. (1985):
  Schmee.values <- c(-3.59, -2.60, -1.73, -0.24, 0.43, 0.58, 0.73)
  probs <- c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975)
  names(Schmee.values) <- paste(probs * 100, "\%", sep = "")

  Schmee.values
  # 2.5%    5%   10%   50%   90%   95% 97.5% 
  #-3.59 -2.60 -1.73 -0.24  0.43  0.58  0.73

  gpqs <- gpqCiNormSinglyCensored(n = 10, n.cen = 6, probs = probs, 
    nmc = 100, censoring.side = "right", seed = 529)

  round(gpqs, 2)
  # 2.5%    5%   10%   50%   90%   95% 97.5% 
  #-2.46 -2.03 -1.38 -0.14  0.54  0.65  0.84 

  # This is what you get if you specify nmc = 1000 with the 
  # same value for seed:
  #-----------------------------------------------
  # 2.5%    5%   10%   50%   90%   95% 97.5% 
  #-3.50 -2.49 -1.67 -0.25  0.41  0.57  0.71


  # Clean up
  #---------
  rm(Schmee.values, probs, gpqs)

  #==========

  # Example of using gpqCiNormMultiplyCensored
  #-------------------------------------------

  # Consider the following set of multiply left-censored data:
  dat <- 12:16
  censored <- c(TRUE, FALSE, TRUE, FALSE, FALSE)

  # Since the data are "ordered" we can identify the indices of the 
  # censored observations in the ordered data as follow:

  cen.index <- (1:length(dat))[censored]
  cen.index
  #[1] 1 3

  # Now we can generate a GPQ using gpqCiNormMultiplyCensored.
  # Here we'll generate a GPQs to use to create a  
  # 95% confidence interval for left-censored data.

  # NOTE:  Here to save computing time for the sake of example, we will specify 
  #        just 100 Monte Carlos, whereas Krishnamoorthy and Mathew (2009) 
  #        suggest *10,000* Monte Carlos.

  gpqCiNormMultiplyCensored(n = 5, cen.index = cen.index,  
    probs = c(0.025, 0.975), nmc = 100, seed = 237)
  #     2.5%     97.5% 
  #-1.315592  1.848513 

  #----------

  # Clean up
  #---------
  rm(dat, censored, cen.index)
}
\keyword{ distribution }
\keyword{ htest }

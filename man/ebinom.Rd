\name{ebinom}
\alias{ebinom}
\title{
  Estimate Parameter of a Binomial Distribution
}
\description{
  Estimate \eqn{p} (the probability of \dQuote{success}) for a binomial distribution, 
  and optionally construct a confidence interval for \eqn{p}.
}
\usage{
  ebinom(x, size = NULL, method = "mle/mme/mvue", ci = FALSE, 
  ci.type = "two-sided", ci.method = "score", correct = TRUE, 
  var.denom = "n", conf.level = 0.95, warn = TRUE)
}
\arguments{
  \item{x}{
  numeric or logical vector of observations.  When \code{size} is not supplied, \code{x} must be 
  a numeric vector of 0s (\dQuote{failures}) and 1s (\dQuote{successes}), or else a logical vector 
  of \code{FALSE} values (\dQuote{failures}) and \code{TRUE} values (\dQuote{successes}).  When 
  \code{size} is supplied, \code{x} must be a non-negative integer containing the number of 
  \dQuote{successes} out of the number of trials indicated by \code{size}.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are 
  allowed but will be removed.
}
  \item{size}{
  positive integer indicating the of number of trials; \code{size} must be at least as 
  large as the value of \code{x}.
}
  \item{method}{
  character string specifying the method of estimation.  The only possible value is 
  \code{"mle/mme/mvue"} (maximum likelihood, method of moments, and minimum variance unbiased).  
  See the DETAILS section for more information.
}
  \item{ci}{
  logical scalar indicating whether to compute a confidence interval for the mean.  The default value 
  is \code{ci=FALSE}.
}
  \item{ci.type}{
  character string indicating what kind of confidence interval to compute.  The possible values are 
  \code{"two-sided"} (the default), \code{"lower"}, and \code{"upper"}.  This 
  argument is ignored if \code{ci=FALSE}.
}
  \item{ci.method}{
  character string indicating which method to use to construct the confidence interval.  Possible values 
  are \code{"score"} (the default), \code{"exact"}, \code{"adjusted Wald"}, and \code{"Wald"}.  
  This argument is ignored if \code{ci=FALSE}.
}
  \item{correct}{
  logical scalar indicating whether to use the continuity correction when \cr
  \code{ci.method="score"} or \code{ci.method="Wald"}.  \cr
  The default value is \code{correct=TRUE}. 
}
  \item{var.denom}{
  character string indicating what value to use in the denominator of the variance estimator when 
  \code{ci.method="Wald"}.  Possible values are \code{"n"} (the default) and \code{"n-1"}.  
  This argument is ignored if \code{ci=FALSE}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level of the confidence interval.  The default 
  value is \code{conf.level=0.95}.  This argument is ignored if \code{ci=FALSE}.
}
  \item{warn}{
  a logical scalar indicating whether to issue a waning in the case when \code{ci=TRUE}, 
  \code{ci.method="Wald"}, and any of the following conditions is true:  the estimated 
  proportion is less than 0.2, the estimated proportion is greater than 0.8, the number of 
  successes or failures is less than 5.  The default value is \code{warn=TRUE}.
}
}
\details{
  If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or 
  infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to performing the estimation.

  If \eqn{\underline{x}} is a vector of \eqn{n} observations from a binomial distribution with 
  parameters \code{size=}\eqn{1} and \code{prob=}\eqn{p}, then the sum of all the values in 
  \eqn{\underline{x}} is an observation from a binomial distribution with parameters 
  \code{size=}\eqn{n} and \code{prob=}\eqn{p}.

  If \eqn{x} is an observation from a binomial distribution with parameters \code{size=}\eqn{n} 
  and \code{prob=}\eqn{p}, the maximum likelihood estimator (mle), method of moments estimator (mme), 
  and minimum variance unbiased estimator (mvue) of \eqn{p} is simply \eqn{x/n}.

  \strong{Confidence Intervals}.  

  \describe{

  \item{\code{ci.method="score"}}{ The confidence interval for \eqn{p} based on the 
  score method was developed by Wilson (1927) and is discussed by Newcombe (1998a), 
  Agresti and Coull (1998), and Agresti and Caffo (2000).  When \code{ci=TRUE} and 
  \code{ci.method="score"}, the function \code{ebinom} calls the R function 
  \code{\link{prop.test}} to compute the confidence interval.  This method 
  has been shown to provide the best performance (in terms of actual coverage matching assumed 
  coverage) of all the methods provided here, although unlike the exact method, the actual 
  coverage can fall below the assumed coverage.
  }

  \item{\code{ci.method="exact"}}{ The confidence interval for \eqn{p} based on the 
  exact (Clopper-Pearson) method is discussed by Newcombe (1998a), Agresti and Coull (1998), 
  and Zar (2010, pp.543-547).  This is the method used in the R function 
  \code{\link{binom.test}}.  This method ensures the actual coverage is greater than or 
  equal to the assumed coverage.
  }

  \item{\code{ci.method="Wald"}}{ The confidence interval for \eqn{p} based on the Wald method 
  (with or without a correction for continuity) is the usual \dQuote{normal approximation} 
  method and is discussed by Newcombe (1998a), Agresti and Coull (1998), Agresti and Caffo (2000), 
  and Zar (2010, pp.543-547).  This method is \strong{never} recommended but is included 
  for historical purposes.
  }

  \item{\code{ci.method="adjusted Wald"}}{ The confidence interval for \eqn{p} based on the 
  adjusted Wald method is discussed by Agresti and Coull (1998), Agresti and Caffo (2000), and 
  Zar (2010, pp.543-547).  This is a simple modification of the Wald method and 
  performs surpringly well.
  }
  }
}
\value{
  a list of class \code{"estimate"} containing the estimated parameters and other information.  
  See \cr
  \code{\link{estimate.object}} for details.
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

  Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions. 
  Fourth Edition. John Wiley and Sons, Hoboken, NJ.

  Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution Monitoring}. 
  Van Nostrand Reinhold, New York, NY, Chapter 11.

  Johnson, N. L., S. Kotz, and A.W. Kemp. (1992).  \emph{Univariate 
  Discrete Distributions}.  Second Edition.  John Wiley and Sons, New York, 
  Chapter 3.

  Millard, S.P., and Neerchal, N.K. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton, Florida.

  Newcombe, R.G. (1998a). Two-Sided Confidence Intervals for the Single Proportion:  Comparison of 
  Seven Methods. \emph{Statistics in Medicine}, \bold{17}, 857--872.

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
  The binomial distribution is used to model processes with binary (Yes-No, Success-Failure, 
  Heads-Tails, etc.) outcomes.  It is assumed that the outcome of any one trial is independent 
  of any other trial, and that the probability of \dQuote{success}, \eqn{p}, is the same on 
  each trial.  A binomial discrete random variable \eqn{X} is the number of \dQuote{successes} in 
  \eqn{n} independent trials.  A special case of the binomial distribution occurs when \eqn{n=1}, 
  in which case \eqn{X} is also called a Bernoulli random variable.

  In the context of environmental statistics, the binomial distribution is sometimes used to model 
  the proportion of times a chemical concentration exceeds a set standard in a given period of 
  time (e.g., Gilbert, 1987, p.143).  The binomial distribution is also used to compute an upper 
  bound on the overall Type I error rate for deciding whether a facility or location is in 
  compliance with some set standard.  Assume the null hypothesis is that the facility is in compliance.  
  If a test of hypothesis is conducted periodically over time to test compliance and/or several tests 
  are performed during each time period, and the facility or location is always in compliance, and 
  each single test has a Type I error rate of \eqn{\alpha}, and the result of each test is 
  independent of the result of any other test (usually not a reasonable assumption), then the number 
  of times the facility is declared out of compliance when in fact it is in compliance is a 
  binomial random variable with probability of \dQuote{success} \eqn{p=\alpha} being the 
  probability of being declared out of compliance (see USEPA, 2009).
}
\seealso{
  \link[stats:Binomial]{Binomial}, \code{\link{prop.test}}, \code{\link{binom.test}}, 
  \code{\link{ciBinomHalfWidth}}, \code{\link{ciBinomN}}, 
  \code{\link{plotCiBinomDesign}}.
}
\examples{
  # Generate 20 observations from a binomial distribution with 
  # parameters size=1 and prob=0.2, then estimate the 'prob' parameter. 
  # (Note: the call to set.seed simply allows you to reproduce this 
  # example. Also, the only parameter estimated is 'prob'; 'size' is
  # specified in the call to ebinom.  The parameter 'size' is printed
  # inorder to show all of the parameters associated with the 
  # distribution.)

  set.seed(251) 
  dat <- rbinom(20, size = 1, prob = 0.2) 
  ebinom(dat) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Binomial
  #
  #Estimated Parameter(s):          size = 20.0
  #                                 prob =  0.1
  #
  #Estimation Method:               mle/mme/mvue for 'prob'
  #
  #Data:                            dat
  #
  #Sample Size:                     20

  #----------------------------------------------------------------

  # Generate one observation from a binomial distribution with 
  # parameters size=20 and prob=0.2, then estimate the "prob" 
  # parameter and compute a confidence interval:

  set.seed(763) 
  dat <- rbinom(1, size=20, prob=0.2) 
  ebinom(dat, size = 20, ci = TRUE) 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Binomial
  #
  #Estimated Parameter(s):          size = 20.00
  #                                 prob =  0.35
  #
  #Estimation Method:               mle/mme/mvue for 'prob'
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Confidence Interval for:         prob
  #
  #Confidence Interval Method:      Score normal approximation
  #                                 (With continuity correction)
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 0.1630867
  #                                 UCL = 0.5905104

  #----------------------------------------------------------------

  # Using the data from the last example, compare confidence 
  # intervals based on the various methods

  ebinom(dat, size = 20, ci = TRUE, 
    ci.method = "score", correct = TRUE)$interval$limits
  #      LCL       UCL 
  #0.1630867 0.5905104


  ebinom(dat, size = 20, ci = TRUE, 
    ci.method = "score", correct = FALSE)$interval$limits
  #      LCL       UCL 
  #0.1811918 0.5671457 


  ebinom(dat, size = 20, ci = TRUE, 
    ci.method = "exact")$interval$limits
  #      LCL       UCL 
  #0.1539092 0.5921885 

  ebinom(dat, size = 20, ci = TRUE, 
    ci.method = "adjusted Wald")$interval$limits
  #      LCL       UCL 
  #0.1799264 0.5684112 


  ebinom(dat, size = 20, ci = TRUE, 
    ci.method = "Wald", correct = TRUE)$interval$limits
  #      LCL       UCL 
  #0.1159627 0.5840373 


  ebinom(dat, size = 20, ci = TRUE, 
    ci.method = "Wald", correct = FALSE)$interval$limits
  #      LCL       UCL 
  #0.1409627 0.5590373 


  #----------------------------------------------------------------

  # Use the cadmium data on page 8-6 of USEPA (1989b) to compute 
  # two-sided 95% confidence intervals for the probability of 
  # detection at background and compliance wells.  The data are 
  # stored in EPA.89b.cadmium.df.

  EPA.89b.cadmium.df
  #   Cadmium.orig Cadmium Censored  Well.type
  #1           0.1   0.100    FALSE Background
  #2          0.12   0.120    FALSE Background
  #3           BDL   0.000     TRUE Background
  #...
  #86          BDL   0.000     TRUE Compliance
  #87          BDL   0.000     TRUE Compliance
  #88          BDL   0.000     TRUE Compliance

  attach(EPA.89b.cadmium.df)

  # Probability of detection at Background well:
  #--------------------------------------------

  ebinom(!Censored[Well.type=="Background"], ci=TRUE)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Binomial
  #
  #Estimated Parameter(s):          size = 24.0000000
  #                                 prob =  0.3333333
  #
  #Estimation Method:               mle/mme/mvue for 'prob'
  #
  #Data:                            !Censored[Well.type == "Background"]
  #
  #Sample Size:                     24
  #
  #Confidence Interval for:         prob
  #
  #Confidence Interval Method:      Score normal approximation
  #                                 (With continuity correction)
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 0.1642654
  #                                 UCL = 0.5530745


  # Probability of detection at Compliance well:
  #--------------------------------------------

  ebinom(!Censored[Well.type=="Compliance"], ci=TRUE)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Binomial
  #
  #Estimated Parameter(s):          size = 64.000
  #                                 prob =  0.375
  #
  #Estimation Method:               mle/mme/mvue for 'prob'
  #
  #Data:                            !Censored[Well.type == "Compliance"]
  #
  #Sample Size:                     64
  #
  #Confidence Interval for:         prob
  #
  #Confidence Interval Method:      Score normal approximation
  #                                 (With continuity correction)
  #
  #Confidence Interval Type:        two-sided
  #
  #Confidence Level:                95%
  #
  #Confidence Interval:             LCL = 0.2597567
  #                                 UCL = 0.5053034

  #----------------------------------------------------------------

  # Clean up
  rm(dat)
  detach("EPA.89b.cadmium.df")
}
\keyword{ distribution }
\keyword{ htest }

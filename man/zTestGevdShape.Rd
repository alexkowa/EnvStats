\name{zTestGevdShape}
\alias{zTestGevdShape}
\title{
  Test Whether the Shape Parameter of a Generalized Extreme Value Distribution is Equal to 0
}
\description{
  Estimate the shape parameter of a \link[=GEVD]{generalized extreme value distribution} 
  and test the null hypothesis that the true value is equal to 0.
}
\usage{
  zTestGevdShape(x, pwme.method = "unbiased", 
    plot.pos.cons = c(a = 0.35, b = 0), alternative = "two.sided")
}
\arguments{
  \item{x}{
  numeric vector of observations.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{pwme.method}{
  character string specifying the method of estimating the probability-weighted 
  moments.  Possible values are \code{"unbiased"} (method based on the U-statistic; 
  the default), and \code{"plotting.position"} (plotting position).  See the help file 
  for \code{\link{egevd}} for more information on these estimation methods. 
}
  \item{plot.pos.cons}{
  numeric vector of length 2 specifying the constants used in the formula for the 
  plotting positions.  The default value is \code{plot.pos.cons=c(a=0.35, b=0)}.  
  If this vector has a names attribute with the value \code{c("a","b")} or 
  \code{c("b","a")}, then the elements will be matched by name in the formula for 
  computing the plotting positions.  Otherwise, the first element is mapped to the 
  name \code{"a"} and the second element to the name \code{"b"}.  See the help file 
  for \code{\link{egevd}} for more information.  This argument is ignored if 
  \code{pwme.method} is not equal to \cr
  \code{"plotting.position"}.
}
  \item{alternative}{
  character string indicating the kind of alternative hypothesis.  The possible values 
  are \code{"two.sided"} (shape not equal to 0; the default), \code{"less"} 
  (shape less than 0), and \code{"greater"} (shape greater than 0). 
}
}
\details{
  Let \eqn{\underline{x} = x_1, x_2, \ldots, x_n} be a vector of \eqn{n} observations 
  from a \link[=GEVD]{generalized extreme value distribution} with parameters 
  \code{location=}\eqn{\eta}, \code{scale=}\eqn{\theta}, and \code{shape=}\eqn{\kappa}.  
  Furthermore, let \eqn{\hat{\kappa}_{pwme}} denote the probability-weighted moments 
  estimator (PWME) of the shape parameter \eqn{\kappa} (see the help file for 
  \code{\link{egevd}}).  Then the statistic
  \deqn{z = \frac{\hat{\kappa}_{pwme}}{\sqrt{0.5633/n}} \;\;\;\;\;\; (1)}
  is asymptotically distributed as a N(0,1) random variable under the null hypothesis 
  \eqn{H_0: \kappa = 0} (Hosking et al., 1985).  The function \code{zTestGevdShape} 
  performs the usual one-sample z-test using the statistic computed in Equation (1).  
  The PWME of \eqn{\kappa} may be computed using either U-statistic type 
  probability-weighted moments estimators or plotting-position type estimators 
  (see \code{\link{egevd}}).  Although Hosking et al. (1985) base their statistic on 
  plotting-position type estimators, Hosking and Wallis (1995) recommend using the 
  U-statistic type estimators for almost all applications.

  This test is only asymptotically correct.  Hosking et al. (1985), however, found 
  that the \eqn{\alpha}-level is adequately maintained for samples as small as 
  \eqn{n = 25}.
}
\value{
  A list of class \code{"htestEnvStats"} containing the results of the hypothesis test.  
  See the help file for \code{\link{htestEnvStats.object}} for details.
}
\references{
  Chowdhury, J.U., J.R. Stedinger, and L. H. Lu. (1991).  Goodness-of-Fit Tests for 
  Regional Generalized Extreme Value Flood Distributions.  
  \emph{Water Resources Research} \bold{27}(7), 1765--1776.

  Fill, H.D., and J.R. Stedinger. (1995).  L Moment and Probability Plot Correlation 
  Coefficient Goodness-of-Fit Tests for the Gumbel Distribution and Impact of 
  Autocorrelation.  \emph{Water Resources Research} \bold{31}(1), 225--229.

  Hosking, J.R.M. (1984).  Testing Whether the Shape Parameter is Zero in the 
  Generalized Extreme-Value Distribution.  \emph{Biometrika} \bold{71}(2), 367--374.

  Hosking, J.R.M., and J.R. Wallis (1995).  A Comparison of Unbiased and 
  Plotting-Position Estimators of L Moments.  
  \emph{Water Resources Research} \bold{31}(8), 2019--2025.

  Hosking, J.R.M., J.R. Wallis, and E.F. Wood. (1985).  Estimation of the 
  Generalized Extreme-Value Distribution by the Method of Probability-Weighted 
  Moments.  \emph{Technometrics} \bold{27}(3), 251--261.

  Jenkinson, A.F. (1955).  The Frequency Distribution of the Annual Maximum 
  (or Minimum) of Meteorological Events.  \emph{Quarterly Journal of the Royal 
  Meteorological Society} \bold{81}, 158--171.

  Vogel, R.M. (1986).  The Probability Plot Correlation Coefficient Test for the 
  Normal, Lognormal, and Gumbel Distributional Hypotheses.  
  \emph{Water Resources Research} \bold{22}(4), 587--590.  
  (Correction, \emph{Water Resources Research} \bold{23}(10), 2013, 1987.)
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  \link[=EVD]{Two-parameter extreme value distributions (EVD)} have been applied 
  extensively since the 1930's to several fields of study, including the 
  distributions of hydrological and meteorological variables, human lifetimes, and 
  strength of materials.  The 
  \link[=GEVD]{three-parameter generalized extreme value distribution (GEVD)} was 
  introduced by Jenkinson (1955) to model annual maximum and minimum values of 
  meteorological events.  Since then, it has been used extensively in the hydological 
  and meteorological fields.

  The three families of EVDs are all special kinds of GEVDs.  When the shape parameter 
  \eqn{\kappa = 0}, the GEVD reduces to the Type I extreme value (Gumbel) distribution.  
  When \eqn{\kappa > 0}, the GEVD is the same as the Type II extreme value 
  distribution, and when \eqn{\kappa < 0} it is the same as the Type III extreme value 
  distribution.

  Hosking et al. (1985) introduced the test used by the function \code{zTestGevdShape} 
  to test the null hypothesis \eqn{H_0: \kappa = 0}.  They found this test has power 
  comparable to the modified likelihood-ratio test, which was found by Hosking (1984) 
  to be the best overall test the thirteen tests he considered.

  Fill and Stedinger (1995) denote this test the \dQuote{kappa test} and compare it 
  with the \emph{L-C}s test suggested by Chowdhury et al. (1991) and the probability 
  plot correlation coefficient goodness-of-fit test for the Gumbel distribution given 
  by Vogel (1986) (see the sub-section for \code{test="ppcc"} under the Details section 
  of the help file for \code{\link{gofTest}}).
}
\seealso{
  \link{GEVD}, \code{\link{egevd}}, \link{EVD}, \code{\link{eevd}}, 
  \link{Goodness-of-Fit Tests}, \code{\link{htestEnvStats.object}}.
}
\examples{
  # Generate 25 observations from a generalized extreme value distribution with 
  # parameters location=2, scale=1, and shape=1, and test the null hypothesis 
  # that the shape parameter is equal to 0. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 

  dat <- rgevd(25, location = 2, scale = 1, shape = 1) 

  zTestGevdShape(dat) 

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 shape = 0
  #
  #Alternative Hypothesis:          True shape is not equal to 0
  #
  #Test Name:                       Z-test of shape=0 for GEVD
  #
  #Estimated Parameter(s):          shape = 0.6623014
  #
  #Estimation Method:               Unbiased pwme
  #
  #Data:                            dat
  #
  #Sample Size:                     25
  #
  #Test Statistic:                  z = 4.412206
  #
  #P-value:                         1.023225e-05

  #----------

  # Clean up
  #---------
  rm(dat)
}
\keyword{htestEnvStats}
\keyword{models}

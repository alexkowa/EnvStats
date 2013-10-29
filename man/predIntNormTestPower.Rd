\name{predIntNormTestPower}
\alias{predIntNormTestPower}
\title{
  Probability That at Least One Future Observation Falls Outside a Prediction Interval for a Normal Distribution
}
\description{
  Compute the probability that at least one out of \eqn{k} future observations 
  (or means) falls outside a prediction interval for \eqn{k} future observations 
  (or means) for a normal distribution.
}
\usage{
  predIntNormTestPower(n, df = n - 1, n.mean = 1, k = 1, delta.over.sigma = 0, 
    pi.type = "upper", conf.level = 0.95)
}
\arguments{
  \item{n}{
  vector of positive integers greater than 2 indicating the sample size upon which 
  the prediction interval is based.
}
  \item{df}{
  vector of positive integers indicating the degrees of freedom associated with 
  the sample size.  The default value is \code{df=n-1}.
}
  \item{n.mean}{
  positive integer specifying the sample size associated with the future averages.  
  The default value is \code{n.mean=1} (i.e., individual observations).  Note that all 
  future averages must be based on the same sample size.
}
  \item{k}{
  vector of positive integers specifying the number of future observations that the 
  prediction interval should contain with confidence level \code{conf.level}.  The 
  default value is \code{k=1}.
}
  \item{delta.over.sigma}{
  vector of numbers indicating the ratio \eqn{\Delta/\sigma}.  The quantity 
  \eqn{\Delta} (delta) denotes the difference between the mean of the population 
  that was sampled to construct the prediction interval, and the mean of the 
  population that will be sampled to produce the future observations.  The quantity 
  \eqn{\sigma} (sigma) denotes the population standard deviation for both populations.  
  See the DETAILS section below for more information.  The default value is 
  \code{delta.over.sigma=0}.
}
  \item{pi.type}{
  character string indicating what kind of prediction interval to compute.  
  The possible values are \code{pi.type="upper"} (the default), and 
  \code{pi.type="lower"}.
}
  \item{conf.level}{
  numeric vector of values between 0 and 1 indicating the confidence level of the 
  prediction interval.  The default value is \code{conf.level=0.95}.
}
}
\details{
  \emph{What is a Prediction Interval?} \cr
  A prediction interval for some population is an interval on the real line 
  constructed so that it will contain \eqn{k} future observations or averages 
  from that population with some specified probability \eqn{(1-\alpha)100\%}, 
  where \eqn{0 < \alpha < 1} and \eqn{k} is some pre-specified positive integer.  
  The quantity \eqn{(1-\alpha)100\%} is call the confidence coefficient or 
  confidence level associated with the prediction interval.  The function 
  \code{\link{predIntNorm}} computes a standard prediction interval based on a 
  sample from a normal distribution.  The function \code{predIntNormTestPower} 
  computes the probability that at least one out of \eqn{k} future observations or 
  averages will \bold{not} be contained in the prediction interval, 
  where the population mean for the future observations is allowed to differ from 
  the population mean for the observations used to construct the prediction interval.
  \cr

  \emph{The Form of a Prediction Interval} \cr
  Let \eqn{\underline{x} = x_1, x_2, \ldots, x_n} denote a vector of \eqn{n} 
  observations from a \link[stats:Normal]{normal distribution} with parameters 
  \code{mean=}\eqn{\mu} and \code{sd=}\eqn{\sigma}.  Also, let \eqn{m} denote the 
  sample size associated with the \eqn{k} future averages (i.e., \code{n.mean=}\eqn{m}).  
  When \eqn{m=1}, each average is really just a single observation, so in the rest of 
  this help file the term \dQuote{averages} will replace the phrase 
  \dQuote{observations or averages}.

  For a normal distribution, the form of a two-sided \eqn{(1-\alpha)100\%} prediction 
  interval is: 
  \deqn{[\bar{x} - Ks, \bar{x} + Ks] \;\;\;\;\;\; (1)} 
  where \eqn{\bar{x}} denotes the sample mean:
  \deqn{\bar{x} = \frac{1}{n} \sum_{i=1}^n x_i \;\;\;\;\;\; (2)}
  \eqn{s} denotes the sample standard deviation:
  \deqn{s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\;\;\; (3)}
  and \eqn{K} denotes a constant that depends on the sample size \eqn{n}, the 
  confidence level, the number of future averages \eqn{k}, and the 
  sample size associated with the future averages, \eqn{m}.  Do not confuse the 
  constant \eqn{K} (uppercase K) with the number of future averages \eqn{k} 
  (lowercase k).  The symbol \eqn{K} is used here to be consistent with the 
  notation used for tolerance intervals (see \code{\link{tolIntNorm}}).  

  Similarly, the form of a one-sided lower prediction interval is: 
  \deqn{[\bar{x} - Ks, \infty] \;\;\;\;\;\; (4)} 
  and the form of a one-sided upper prediction interval is:
  \deqn{[-\infty, \bar{x} + Ks] \;\;\;\;\;\; (5)} 
  but \eqn{K} differs for one-sided versus two-sided prediction intervals.  
  The derivation of the constant \eqn{K} is explained in the help file for 
  \code{\link{predIntNormK}}.
  \cr

  \emph{Computing Power} \cr
  The "power" of the prediction interval is defined as the probability that at 
  least one out of the \eqn{k} future observations or averages 
  will \bold{not} be contained in the prediction interval, where the population mean 
  for the future observations is allowed to differ from the population mean for the 
  observations used to construct the prediction interval.  The probability \eqn{p} 
  that all \eqn{k} future observations will be contained in a one-sided upper 
  prediction interval (\code{pi.type="upper"}) is given in Equation (6) of the help 
  file for 
  \code{\link{predIntNormSimultaneousK}}, where \eqn{k=m} and \eqn{r=1}:
  \deqn{p = \int_0^1 T(\sqrt{n}K; n-1, \sqrt{n}[\Phi^{-1}(v) + \frac{\Delta}{\sigma}]) [\frac{v^{k-1}}{B(k, 1)}] dv \;\;\;\;\;\; (6)}
  where \eqn{T(x; \nu, \delta)} denotes the cdf of the 
  \link[stats:TDist]{non-central Student's t-distribution} with parameters 
  \code{df=}\eqn{\nu} and \code{ncp=}\eqn{\delta} evaluated at \eqn{x}; 
  \eqn{\Phi(x)} denotes the cdf of the standard \link[stats:Normal]{normal distribution} 
  evaluated at \eqn{x}; and \eqn{B(\nu, \omega)} denotes the value of the 
  \link[base:Special]{beta function} with parameters \code{a=}\eqn{\nu} and 
  \code{b=}\eqn{\omega}.

  The quantity \eqn{\Delta} (upper case delta) denotes the difference between the 
  mean of the population that was sampled to construct the prediction interval, and 
  the mean of the population that will be sampled to produce the future observations.  
  The quantity \eqn{\sigma} (sigma) denotes the population standard deviation of both 
  of these populations.  Usually you assume \eqn{\Delta=0} unless you are interested 
  in computing the power of the rule to detect a change in means between the 
  populations, as we are here.

  If we are interested in using averages instead of single observations, with 
  \eqn{w \ge 1} (i.e., \code{n.mean}\eqn{\ge 1}), the first 
  term in the integral in Equation (6) that involves the cdf of the 
  \link[stats:TDist]{non-central Student's t-distribution} becomes:
  \deqn{T(\sqrt{n}K; n-1, \frac{\sqrt{n}}{\sqrt{w}}[\Phi^{-1}(v) + \frac{\sqrt{w}\Delta}{\sigma}]) \;\;\;\;\;\; (7)}
  
  For a given confidence level \eqn{(1-\alpha)100\%}, the power of the rule to detect 
  a change in means is simply given by:
  \deqn{Power = 1 - p \;\;\;\;\;\; (8)}
  where \eqn{p} is defined in Equation (6) above using the value of \eqn{K} that 
  corresponds to \eqn{\Delta/\sigma = 0}.  Thus, when the argument 
  \code{delta.over.sigma=0}, the value of \eqn{p} is \eqn{1-\alpha} and the power is 
  simply \eqn{\alpha 100\%}.  As \code{delta.over.sigma} increases above 0, the power 
  increases.

  When \code{pi.type="lower"}, the same value of \code{K} is used as when 
  \code{pi.type="upper"}, but Equation (4) is used to construct the prediction 
  interval.  Thus, the power increases as \code{delta.over.sigma} decreases below 0.
}
\value{
  vector of values between 0 and 1 equal to the probability that at least one of 
  \eqn{k} future observations or averages will fall outside the prediction interval.
}
\references{
  See the help files for \code{\link{predIntNorm}} and 
  \code{\link{predIntNormSimultaneous}}.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  See the help files for \code{\link{predIntNorm}} and 
  \code{\link{predIntNormSimultaneous}}.

  In the course of designing a sampling program, an environmental scientist may wish 
  to determine the relationship between sample size, significance level, power, and 
  scaled difference if one of the objectives of the sampling program is to determine 
  whether two distributions differ from each other.  The functions 
  \code{predIntNormTestPower} and \code{\link{plotPredIntNormTestPowerCurve}} can be 
  used to investigate these relationships for the case of normally-distributed 
  observations.  In the case of a simple shift between the two means, the test based 
  on a prediction interval is not as powerful as the two-sample t-test.  However, the 
  test based on a prediction interval is more efficient at detecting a shift in the 
  tail.
}
\seealso{
  \code{\link{predIntNorm}}, \code{\link{predIntNormK}}, 
  \code{\link{plotPredIntNormTestPowerCurve}}, \code{\link{predIntNormSimultaneous}}, 
  \code{\link{predIntNormSimultaneousK}}, 
  \code{\link{predIntNormSimultaneousTestPower}}, \link{Prediction Intervals}, 
  \link{Normal}.
}
\examples{
  # Show how the power increases as delta.over.sigma increases.  
  # Assume a 95% upper prediction interval.

  predIntNormTestPower(n = 4, delta.over.sigma = 0:2) 
  #[1] 0.0500000 0.1743014 0.3990892

  #----------

  # Look at how the power increases with sample size for a one-sided upper 
  # prediction interval with k=3, delta.over.sigma=2, and a confidence level 
  # of 95%.

  predIntNormTestPower(n = c(4, 8), k = 3, delta.over.sigma = 2) 
  #[1] 0.3578250 0.5752113

  #----------

  # Show how the power for an upper 95% prediction limit increases as the 
  # number of future observations k increases.  Here, we'll use n=20 and 
  # delta.over.sigma=1.

  predIntNormTestPower(n = 20, k = 1:3, delta.over.sigma = 1) 
  #[1] 0.2408527 0.2751074 0.2936486
}
\keyword{ distribution }
\keyword{ htest }

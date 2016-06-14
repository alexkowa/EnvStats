\name{predIntPois}
\alias{predIntPois}
\title{
  Prediction Interval for a Poisson Distribution
}
\description{
  Estimate the mean of a \code{\link[stats:Poisson]{Poisson distribution}}, and 
  construct a prediction interval for the next \eqn{k} observations or 
  next set of \eqn{k} sums.
}
\usage{
  predIntPois(x, k = 1, n.sum = 1, method = "conditional", 
    pi.type = "two-sided", conf.level = 0.95, round.limits = TRUE)
}
\arguments{
  \item{x}{
  numeric vector of observations, or an object resulting from a call to an 
  estimating function that assumes a Poisson distribution 
  (i.e., \code{\link{epois}} or \code{\link{epoisCensored}}).    
  If \code{x} is a numeric vector, 
  missing (\code{NA}), undefined (\code{NaN}), and 
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{k}{
  positive integer specifying the number of future observations or sums the 
  prediction interval should contain with confidence level \code{conf.level}.  
  The default value is \code{k=1}.
}
  \item{n.sum}{
  positive integer specifying the sample size associated with the \eqn{k} future 
  sums.  The default value is \code{n.sum=1} (i.e., individual observations).  
  Note that all future sums must be based on the same sample size.
}
  \item{method}{
  character string specifying the method to use.  The possible values are: \cr
  \code{"conditional"} (based on a conditional distribution; the default), \cr
  \code{"conditional.approx.normal"} (method based on approximating a conditional 
  distribution with the standard normal distribution), \cr
  \code{"conditional.approx.t"} (method based on approximating a conditional 
  distribution with Student's t-distribution), and \cr
  \code{"normal.approx"} (approximate method based on the fact that the 
  mean and varaince of a Poisson distribution are the same). \cr

  See the DETAILS section for more information on these methods.  The \cr
  \code{"conditional"} method 
  is only implemented for \code{k=1}; when \code{k} is bigger than 1, the value of 
  \code{method} cannot be \code{"conditional"}. 
}
  \item{pi.type}{
  character string indicating what kind of prediction interval to compute.  
  The possible values are \code{pi.type="two-sided"} (the default), 
  \code{pi.type="lower"}, and \code{pi.type="upper"}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level of the prediction interval.  
  The default value is \code{conf.level=0.95}.
}
  \item{round.limits}{
  logical scalar indicating whether to round the computed prediction limits to the 
  nearest integer.  The default value is \code{round.limits=TRUE}.
}
}
\details{
  A prediction interval for some population is an interval on the real line constructed so 
  that it will contain \eqn{k} future observations or averages from that population with 
  some specified probability \eqn{(1-\alpha)100\%}, where 0 < \eqn{\alpha} < 1 and \eqn{k} 
  is some pre-specified positive integer.  The quantity \eqn{(1-\alpha)100\%} is call 
  the confidence coefficient or confidence level associated with the prediction interval.

  In the case of a \link[stats:Poisson]{Poisson distribution}, we have modified the 
  usual meaning of a prediction interval and instead construct an interval that will 
  contain \eqn{k} future observations or \eqn{k} future \emph{sums} with a certain 
  confidence level.

  A prediction interval is a \emph{random} interval; that is, the lower and/or 
  upper bounds are random variables computed based on sample statistics in the 
  baseline sample.  Prior to taking one specific baseline sample, the probability 
  that the prediction interval will contain the next \eqn{k} averages is 
  \eqn{(1-\alpha)100\%}.  Once a specific baseline sample is taken and the 
  prediction interval based on that sample is computed, the probability that that 
  prediction interval will contain the next \eqn{k} averages is not necessarily 
  \eqn{(1-a)100\%}, but it should be close.

  If an experiment is repeated \eqn{N} times, and for each experiment:
  \enumerate{
  \item A sample is taken and a \eqn{(1-a)100\%} prediction interval for \eqn{k=1} 
  future observation is computed, and 
  \item One future observation is generated and compared to the prediction interval,
  }
  then the number of prediction intervals that actually contain the future observation 
  generated in step 2 above is a binomial random variable with parameters 
  \code{size=}\eqn{N} and \code{prob=}\eqn{(1-\alpha)100\%} (see \link{Binomial}).

  If, on the other hand, only one baseline sample is taken and only one prediction 
  interval for \eqn{k=1} future observation is computed, then the number of 
  future observations out of a total of \eqn{N} future observations that will be 
  contained in that one prediction interval is a binomial random variable with 
  parameters \code{size=}\eqn{N} and \code{prob=}\eqn{(1-\alpha^*)100\%}, where 
  \eqn{\alpha^*} depends on the true population parameters and the computed 
  bounds of the prediction interval.

  Because of the discrete nature of the \link[stats:Poisson]{Poisson distribution}, 
  even if the true mean of the distribution \eqn{\lambda} were known exactly, the 
  actual confidence level associated with a prediction limit will usually not be exactly equal to 
  \eqn{(1-\alpha)100\%}.  For example, for the Poisson distribution with parameter 
  \code{lambda=2}, the interval [0, 4] contains 94.7\% of this distribution and 
  the interval [0,5] contains 98.3\% of this distribution.  Thus, no interval can 
  contain exactly 95\% of this distribution, so it is impossible to construct an 
  exact 95\% prediction interval for the next \eqn{k=1} observation for a 
  Poisson distribution with parameter \code{lambda=2}.
  \cr

  \bold{The Form of a Poisson Prediction Interval} \cr
  Let \eqn{\underline{x} = x_1, x_2, \ldots, x_n} denote a vector of \eqn{n} 
  observations from a \link[stats:Poisson]{Poisson distribution} with parameter 
  \code{lambda=}\eqn{\lambda}.  Also, let \eqn{X} denote the sum of these 
  \eqn{n} random variables, i.e.,
  \deqn{X = \sum_{i=1}^n x_i \;\;\;\;\;\; (1)}
  Finally, let \eqn{m} denote the sample size associated with the \eqn{k} future 
  sums (i.e., \code{n.sum=}\eqn{m}).  When \eqn{m=1}, each sum is really just a 
  single observation, so in the rest of this help file the term \dQuote{sums} 
  replaces the phrase \dQuote{observations or sums}.

  Let \eqn{\underline{y} = y_1, y_2, \ldots, y_m} denote a vector of \eqn{m} 
  future observations from a Poisson distribution with parameter 
  \code{lambda=}\eqn{\lambda^{*}}, and set \eqn{Y} equal to the sum of these 
  \eqn{m} random variables, i.e.,
  \deqn{Y = \sum_{i=1}^m y_i \;\;\;\;\;\; (2)}
  Then \eqn{Y} has a Poisson distribution with parameter 
  \code{lambda=}\eqn{m\lambda^{*}} (Johnson et al., 1992, p.160).  We are interested 
  in constructing a prediction limit for the next value of \eqn{Y}, or else the next 
  \eqn{k} sums of \eqn{m} Poisson random variables, based on the observed value of 
  \eqn{X} and assuming \eqn{\lambda^{*} = \lambda}.

  For a Poisson distribution, the form of a two-sided prediction interval is:
  \deqn{[m\bar{x} - K, m\bar{x} + K] = [cX - K, cX + K] \;\;\;\;\;\; (3)}
  where 
  \deqn{\bar{x} = \frac{X}{n} = \sum_{i=1}^n x_i \;\;\;\;\;\; (4)}
  \deqn{c = \frac{m}{n} \;\;\;\;\;\; (5)}
  and \eqn{K} is a constant that depends on the sample size \eqn{n}, the 
  confidence level \eqn{(1-\alpha)100\%}, the number of future sums \eqn{k}, 
  and the sample size associated with the future sums \eqn{m}. Do not confuse 
  the constant \eqn{K} (uppercase \eqn{K}) with the number of future sums 
  \eqn{k} (lowercase \eqn{k}).  The symbol \eqn{K} is used here to be consistent 
  with the notation used for prediction intervals for the normal distribution 
  (see \code{\link{predIntNorm}}).

  Similarly, the form of a one-sided lower prediction interval is:
  \deqn{[m\bar{x} - K, \infty] = [cX - K, \infty] \;\;\;\;\;\; (6)}
  and the form of a one-sided upper prediction interval is:
  \deqn{[0, m\bar{x} + K] = [0, cX + K] \;\;\;\;\;\; (7)}
  The derivation of the constant \eqn{K} is explained below.
  \cr

  \bold{Conditional Distribution} (\code{method="conditional"}) \cr
  Nelson (1970) derives a prediction interval for the case \eqn{k=1} based on the 
  conditional distribution of \eqn{Y} given \eqn{X+Y}.  He notes that the conditional 
  distribution of \eqn{Y} given the quantity \eqn{X+Y=w} is 
  \link[stats:Binomial]{binomial} with parameters 
  \code{size=}\eqn{w} and \code{prob=}\eqn{[m\lambda^{*} / (m\lambda^{*} + n\lambda)]} 
  (Johnson et al., 1992, p.161).  When \eqn{k=1}, the prediction limits are computed 
  as those most extreme values of \eqn{Y} that still yield a non-significant test of 
  the hypothesis \eqn{H_0: \lambda^{*} = \lambda}, which for the conditional 
  distribution of \eqn{Y} is equivalent to the hypothesis 
  \eqn{H_0}: \code{prob=[m /(m + n)]}.

  Using the relationship between the \link[stats:Binomial]{binomial} and 
  \link[stats:FDist]{F-distribution} (see the explanation of exact confidence 
  intervals in the help file for \code{\link{ebinom}}), Nelson (1982, p. 203) states 
  that exact two-sided \eqn{(1-\alpha)100\%} prediction limits [LPL, UPL] are the 
  closest integer solutions to the following equations:
  \deqn{\frac{m}{LPL + 1} = \frac{n}{X} F(2 LPL + 2, 2X, 1 - \alpha/2) \;\;\;\;\;\; (8)}
  \deqn{\frac{UPL}{n} = \frac{X+1}{n} F(2X + 2, 2 UPL, 1 - \alpha/2) \;\;\;\;\;\; (9)}
  where \eqn{F(\nu_1, \nu_2, p)} denotes the \eqn{p}'th quantile of the 
  \link[stats:FDist]{F-distribution} with \eqn{\nu_1} and \eqn{\nu_2} degrees of 
  freedom.

  If \code{ci.type="lower"}, \eqn{\alpha/2} is replaced with \eqn{\alpha} in 
  Equation (8) above for \eqn{LPL}, and \eqn{UPL} is set to \eqn{\infty}.

  If \code{ci.type="upper"}, \eqn{\alpha/2} is replaced with \eqn{\alpha} in 
  Equation (9) above for \eqn{UPL}, and \eqn{LPL} is set to 0.

  \bold{NOTE:} This method is not extended to the case \eqn{k > 1}.
  \cr

  \bold{Conditional Distribution Approximation Based on Normal Distribution} \cr
  (\code{method="conditional.approx.normal"}) \cr
  Cox and Hinkley (1974, p.245) derive an approximate prediction interval for the case 
  \eqn{k=1}.  Like Nelson (1970), they note that the conditional distribution of 
  \eqn{Y} given the quantity \eqn{X+Y=w} is  \link[stats:Binomial]{binomial} with 
  parameters \code{size=}\eqn{w} and 
  \code{prob=}\eqn{[m\lambda^{*} / (m\lambda^{*} + n\lambda)]}, and that the 
  hypothesis \eqn{H_0: \lambda^{*} = \lambda} is equivalent to the hypothesis 
  \eqn{H_0}: \code{prob=[m /(m + n)]}.

  Cox and Hinkley (1974, p.245) suggest using the normal approximation to the 
  binomial distribution (in this case, without the continuity correction; 
  see Zar, 2010, pp.534-536 for information on the continuity correction associated 
  with the normal approximation to the binomial distribution).  Under the null 
  hypothesis \eqn{H_0: \lambda^{*} = \lambda}, the quantity
  \deqn{z = [Y - \frac{c(X+Y)}{1+c}] / \{ [\frac{c(X+Y)}{(1+c)^2}]^{1/2} \} \;\;\;\;\;\; (10)}
  is approximately distributed as a standard normal random variable.

  \emph{The Case When k = 1} \cr
  When \eqn{k = 1} and \code{pi.type="two-sided"}, the prediction limits are computed 
  by solving the equation
  \deqn{z^2 \le z_{1 - \alpha/2}^2 \;\;\;\;\;\; (11)}
  where \eqn{z_p} denotes the \eqn{p}'th quantile of the standard normal distribution.  
  In this case, Gibbons (1987b) notes that the quantity \eqn{K} in Equation (3) above 
  is given by:
  \deqn{K = \frac{t^2c}{2} tc[X (1 + \frac{1}{c}) + \frac{t^2}{4}]^{1/2} \;\;\;\;\;\; (12)}
  where \eqn{t = z_{1-\alpha/2}}.

  When \code{pi.type="lower"} or \code{pi.type="upper"}, \eqn{K} is computed exactly 
  as above, except \eqn{t} is set to \eqn{t = z_{1-\alpha}}.

  \emph{The Case When k > 1} \cr
  When \eqn{k > 1}, Gibbons (1987b) suggests using the Bonferroni inequality.  
  That is, the value of \eqn{K} is computed exactly as for the case \eqn{k=1} 
  described above, except that the Bonferroni value of \eqn{t} is used in place of the 
  usual value of \eqn{t}:

  When \code{pi.type="two-side"}, \eqn{t = z_{1 - (\alpha/k)/2}}.

  When \code{pi.type="lower"} or \code{pi.type="upper"}, \eqn{t = z_{1 - \alpha/k}}.
  \cr

  \bold{Conditional Distribution Approximation Based on Student's t-Distribution} \cr
  (\code{method="conditional.approx.t"}) \cr
  When \code{method="conditional.approx.t"}, the exact same procedure is used as when \cr
  \code{method="conditional.approx.normal"}, except that the quantity in 
  Equation (10) is assumed to follow a Student's t-distribution with \eqn{n-1} 
  degrees of freedom.  Thus, all occurrences of \eqn{z_p} are replaced with 
  \eqn{t_{n-1, p}}, where \eqn{t_{\nu, p}} denotes the \eqn{p}'th quantile of 
  \link[stats:TDist]{Student's t-distribution} with \eqn{\nu} degrees of freedom.
  \cr

  \bold{Normal Approximation} (\code{method="normal.approx"}) \cr
  The normal approximation for Poisson prediction limits was given by 
  Nelson (1970; 1982, p.203) and is based on the fact that the mean and variance of a 
  Poisson distribution are the same (Johnson et al, 1992, p.157), and for 
  \dQuote{large} values of \eqn{n} and \eqn{m}, both \eqn{X} and \eqn{Y} are 
  approximately normally distributed.

  \emph{The Case When k = 1} \cr
  The quantity \eqn{Y - cX} is approximately normally distributed with expectation and 
  variance given by:
  \deqn{E(Y - cX) = E(Y) - cE(X) = m\lambda - cn\lambda = 0 \;\;\;\;\;\; (13)}
  \deqn{Var(Y - cX) = Var(Y) + c^2 Var(X) = m\lambda + c^2 n\lambda = m\lambda (1 + \frac{m}{n}) \;\;\;\;\;\; (14)}
  so the quantity 
  \deqn{z = \frac{Y-cX}{\sqrt{m\hat{\lambda}(1 + \frac{m}{n})}} = \frac{Y-cX}{\sqrt{m\bar{x}(1 + \frac{m}{n})}} \;\;\;\;\;\; (15)}
  is approximately distributed as a standard normal random variable.  The function 
  \code{predIntPois}, however, assumes this quantity is distributed as approximately 
  a \link[stats:TDist]{Student's t-distribution} with \eqn{n-1} degrees of freedom.

  Thus, following the idea of prediction intervals for a normal distribution 
  (see \code{\link{predIntNorm}}), when \code{pi.type="two-sided"}, the constant 
  \eqn{K} for a \eqn{(1-\alpha)100\%} prediction interval for the next \eqn{k=1} sum 
  of \eqn{m} observations is computed as:
  \deqn{K = t_{n-1, 1-\alpha/2} \sqrt{m\bar{x} (1 + \frac{m}{n})} \;\;\;\;\;\; (16)}
  where \eqn{t_{\nu, p}} denotes the \eqn{p}'th quantile of a 
  \link[stats:TDist]{Student's t-distribution} with \eqn{\nu} degrees of freedom. 
 
  Similarly, when \code{pi.type="lower"} or \code{pi.type="upper"}, the constant 
  \eqn{K} is computed as:
  \deqn{K = t_{n-1, 1-\alpha} \sqrt{m\bar{x} (1 + \frac{m}{n})} \;\;\;\;\;\; (17)}

  \emph{The Case When k > 1} \cr
  When \eqn{k > 1}, the value of \eqn{K} is computed exactly as for the case 
  \eqn{k = 1} described above, except that the Bonferroni value of \eqn{t} is used 
  in place of the usual value of \eqn{t}:

  When \code{pi.type="two-sided"}, 
  \deqn{K = t_{n-1, 1-(\alpha/k)/2} \sqrt{m\bar{x} (1 + \frac{m}{n})} \;\;\;\;\;\; (18)}

  When \code{pi.type="lower"} or \code{pi.type="upper"}, 
  \deqn{K = t_{n-1, 1-(\alpha/k)} \sqrt{m\bar{x} (1 + \frac{m}{n})} \;\;\;\;\;\; (19)}

  Hahn and Nelson (1973, p.182) discuss another method of computing \eqn{K} when 
  \eqn{k > 1}, but this method is not implemented here.
}
\value{
  If \code{x} is a numeric vector, \code{predIntPois} returns a list of class 
  \code{"estimate"} containing the estimated parameter, the prediction interval, 
  and other information.  See the help file for \cr
  \code{\link{estimate.object}} for details.

  If \code{x} is the result of calling an estimation function, 
  \code{predIntPois} returns a list whose class is the same as \code{x}.  
  The list contains the same components as \code{x}, as well as a component called 
  \code{interval} containing the prediction interval information.  
  If \code{x} already has a component called \code{interval}, this component is 
  replaced with the prediction interval information.
}
\references{
  Cox, D.R., and D.V. Hinkley. (1974).  \emph{Theoretical Statistics}.  
  Chapman and Hall, New York, pp.242--245.

  Gibbons, R.D. (1987b).  Statistical Models for the Analysis of Volatile Organic 
  Compounds in Waste Disposal Sites.  \emph{Ground Water} \bold{25}, 572--580.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009). 
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.  
  John Wiley & Sons, Hoboken, pp. 72--76.

  Hahn, G.J., and W.Q. Meeker. (1991). \emph{Statistical Intervals: A Guide for Practitioners}. 
  John Wiley and Sons, New York.

  Hahn, G., and W. Nelson. (1973).  A Survey of Prediction Intervals and Their 
  Applications.  \emph{Journal of Quality Technology} \bold{5}, 178--188.

  Johnson, N. L., S. Kotz, and A. Kemp. (1992).  \emph{Univariate Discrete 
  Distributions}.  Second Edition.  John Wiley and Sons, New York, Chapter 4.

  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton.

  Miller, R.G. (1981a).  \emph{Simultaneous Statistical Inference}.  
  McGraw-Hill, New York, pp.8, 76--81.

  Nelson, W.R. (1970).  Confidence Intervals for the Ratio of Two Poisson Means and 
  Poisson Predictor Intervals.  \emph{IEEE Transactions of Reliability} \bold{R-19}, 
  42--49.

  Nelson, W.R. (1982).  \emph{Applied Life Data Analysis}.  John Wiley and Sons, 
  New York, pp.200--204.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. 
  Fifth Edition. Prentice-Hall, Upper Saddle River, NJ, pp. 585--586.  
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Prediction and tolerance intervals have long been applied to quality control and 
  life testing problems.  Nelson (1970) notes that his development of confidence and 
  prediction limits for the Poisson distribution is based on well-known results 
  dating back to the 1950's.  Hahn and Nelson (1973) review predicion intervals for 
  several distributions, including Poisson prediction intervals.  The mongraph by 
  Hahn and Meeker (1991) includes a discussion of Poisson prediction intervals.

  Gibbons (1987b) uses the Poisson distribution to model the number of detected 
  compounds per scan of the 32 volatile organic priority pollutants (VOC), and also 
  to model the distribution of chemical concentration (in ppb), and presents formulas 
  for prediction and tolerance intervals.  The formulas for prediction intervals are 
  based on Cox and Hinkley (1974, p.245).  Gibbons (1987b) only deals with 
  the case where \code{n.sum=1}.

  Gibbons et al. (2009, pp. 72--76) discuss methods for Poisson prediction limits.
}
\seealso{
  \code{\link{Poisson}}, \code{\link{epois}},  
  \code{\link{estimate.object}}, \link{Prediction Intervals}, 
  \code{\link{tolIntPois}}, \link{Estimating Distribution Parameters}.
}
\examples{
  # Generate 20 observations from a Poisson distribution with parameter 
  # lambda=2.  The interval [0, 4] contains 94.7% of this distribution and 
  # the interval [0,5] contains 98.3% of this distribution.  Thus, because 
  # of the discrete nature of the Poisson distribution, no interval contains 
  # exactly 95% of this distribution.  Use predIntPois to estimate the mean 
  # parameter of the true distribution, and construct a one-sided upper 
  # 95% prediction interval for the next single observation from this distribution. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rpois(20, lambda = 2) 

  predIntPois(dat, pi.type = "upper") 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Poisson
  #
  #Estimated Parameter(s):          lambda = 1.8
  #
  #Estimation Method:               mle/mme/mvue
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Prediction Interval Method:      conditional
  #
  #Prediction Interval Type:        upper
  #
  #Confidence Level:                95%
  #
  #Number of Future Observations:   1
  #
  #Prediction Interval:             LPL = 0
  #                                 UPL = 5

  #----------

  # Compare results above with the other approximation methods:

  predIntPois(dat, method = "conditional.approx.normal", 
    pi.type = "upper")$interval$limits
  #LPL UPL 
  #  0   4  
 

  predIntPois(dat, method = "conditional.approx.t", 
    pi.type = "upper")$interval$limits 
  #LPL UPL 
  #  0   4 


  predIntPois(dat, method = "normal.approx", 
    pi.type = "upper")$interval$limits 
  #LPL UPL 
  #  0   4 
  #Warning message:
  #In predIntPois(dat, method = "normal.approx", pi.type = "upper") :
  #  Estimated value of 'lambda' and/or number of future observations 
  #  is/are probably too small for the normal approximation to work well.

  #==========

  # Using the same data as in the previous example, compute a one-sided 
  # upper 95% prediction limit for k=10 future observations.  

  # Using conditional approximation method based on the normal distribution.

  predIntPois(dat, k = 10, method = "conditional.approx.normal", 
    pi.type = "upper") 

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Poisson
  #
  #Estimated Parameter(s):          lambda = 1.8
  #
  #Estimation Method:               mle/mme/mvue
  #
  #Data:                            dat
  #
  #Sample Size:                     20
  #
  #Prediction Interval Method:      conditional.approx.normal
  #
  #Prediction Interval Type:        upper
  #
  #Confidence Level:                95%
  #
  #Number of Future Observations:   10
  #
  #Prediction Interval:             LPL = 0
  #                                 UPL = 6

  
  # Using method based on approximating conditional distribution with 
  # Student's t-distribution

  predIntPois(dat, k = 10, method = "conditional.approx.t", 
    pi.type = "upper")$interval$limits
  #LPL UPL 
  #  0   6 

  #==========

  # Repeat the above example, but set k=5 and n.sum=3.  Thus, we want a 
  # 95% upper prediction limit for the next 5 sets of sums of 3 observations.

  predIntPois(dat, k = 5, n.sum = 3, method = "conditional.approx.t", 
    pi.type = "upper")$interval$limits
  #LPL UPL 
  #  0  12

  #==========

  # Reproduce Example 3.6 in Gibbons et al. (2009, p. 75)
  # A 32-constituent VOC scan was performed for n=16 upgradient 
  # samples and there were 5 detections out of these 16.  We 
  # want to construct a one-sided upper 95% prediction limit 
  # for 20 monitoring wells (so k=20 future observations) based 
  # on these data.

  # First we need to create a data set that will yield a mean 
  # of 5/16 based on a sample size of 16.  Any number of data 
  # sets will do.  Here are two possible ones:

  dat <- c(rep(1, 5), rep(0, 11))
  dat <- c(2, rep(1, 3), rep(0, 12))

  # Now call predIntPois.  Don't round the limits so we can 
  # compare to the example in Gibbons et al. (2009).

  predIntPois(dat, k = 20, method = "conditional.approx.t", 
    pi.type = "upper", round.limits = FALSE)

  #Results of Distribution Parameter Estimation
  #--------------------------------------------
  #
  #Assumed Distribution:            Poisson
  #
  #Estimated Parameter(s):          lambda = 0.3125
  #
  #Estimation Method:               mle/mme/mvue
  #
  #Data:                            dat
  #
  #Sample Size:                     16
  #
  #Prediction Interval Method:      conditional.approx.t
  #
  #Prediction Interval Type:        upper
  #
  #Confidence Level:                95%
  #
  #Number of Future Observations:   20
  #
  #Prediction Interval:             LPL = 0.000000
  #                                 UPL = 2.573258

  #==========

  # Cleanup
  #--------
  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }

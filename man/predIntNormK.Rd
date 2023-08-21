\name{predIntNormK}
\alias{predIntNormK}
\title{
  Compute the Value of \eqn{K} for a Prediction Interval for a Normal Distribution
}
\description{
  Compute the value of \eqn{K} (the multiplier of estimated standard deviation) used
  to construct a prediction interval for the next \eqn{k} observations or next set of
  \eqn{k} means based on data from a \link[stats:Normal]{normal distribution}.
  The function
  \code{predIntNormK} is called by \code{\link{predIntNorm}}.
}
\usage{
  predIntNormK(n, df = n - 1, n.mean = 1, k = 1,
    method = "Bonferroni", pi.type = "two-sided",
    conf.level = 0.95)
}
\arguments{
  \item{n}{
  a positive integer greater than 2 indicating the sample size upon which the
  prediction interval is based.
}
  \item{df}{
  the degrees of freedom associated with the prediction interval.  The default is
  \code{df=n-1}.
}
  \item{n.mean}{
  positive integer specifying the sample size associated with the \eqn{k} future averages.
  The default value is \code{n.mean=1} (i.e., individual observations).  Note that all
  future averages must be based on the same sample size.
}
  \item{k}{
  positive integer specifying the number of future observations or averages the
  prediction interval should contain with confidence level \code{conf.level}.
  The default value is \code{k=1}.
}
  \item{method}{
  character string specifying the method to use if the number of future observations
  (\code{k}) is greater than 1.  The possible values are \code{method="Bonferroni"}
  (approximate method based on Bonferonni inequality; the default), and \cr
  \code{method="exact"} (exact method due to Dunnett, 1955).  See the DETAILS section for
  more information.  This argument is ignored if \code{k=1}.
}
  \item{pi.type}{
  character string indicating what kind of prediction interval to compute.
  The possible values are \code{pi.type="two-sided"} (the default), \code{pi.type="lower"},
  and \code{pi.type="upper"}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level of the prediction interval.
  The default value is \code{conf.level=0.95}.
}
}
\details{
  A prediction interval for some population is an interval on the real line constructed
  so that it will contain \eqn{k} future observations or averages from that population
  with some specified probability \eqn{(1-\alpha)100\%}, where
  \eqn{0 < \alpha < 1} and \eqn{k} is some pre-specified positive integer.
  The quantity \eqn{(1-\alpha)100\%} is called
  the confidence coefficient or confidence level associated with the prediction
  interval.

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
  The derivation of the constant \eqn{K} is explained below.  The function
  \code{predIntNormK} computes the value of \eqn{K} and is called by
  \code{\link{predIntNorm}}.
  \cr

  \bold{The Derivation of K for One Future Observation or Average (k = 1)} \cr
  Let \eqn{X} denote a random variable from a \link[stats:Normal]{normal distribution}
  with parameters \code{mean=}\eqn{\mu} and \code{sd=}\eqn{\sigma}, and let
  \eqn{x_p} denote the \eqn{p}'th quantile of \eqn{X}.

  A true two-sided \eqn{(1-\alpha)100\%} prediction interval for the next
  \eqn{k=1} observation of \eqn{X} is given by:
  \deqn{[x_{\alpha/2}, x_{1-\alpha/2}] = [\mu - z_{1-\alpha/2}\sigma,  \mu + z_{1-\alpha/2}\sigma] \;\;\;\;\;\; (6)}
  where \eqn{z_p} denotes the \eqn{p}'th quantile of a standard normal distribution.

  More generally, a true two-sided \eqn{(1-\alpha)100\%} prediction interval for the
  next \eqn{k=1} average based on a sample of size \eqn{m} is given by:
  \deqn{[\mu - z_{1-\alpha/2}\frac{\sigma}{\sqrt{m}},  \mu + z_{1-\alpha/2}\frac{\sigma}{\sqrt{m}}] \;\;\;\;\;\; (7)}

  Because the values of \eqn{\mu} and \eqn{\sigma} are unknown, they must be
  estimated, and a prediction interval then constructed based on the estimated
  values of \eqn{\mu} and \eqn{\sigma}.

  For a two-sided prediction interval (\code{pi.type="two-sided"}),
  the constant \eqn{K} for a \eqn{(1-\alpha)100\%} prediction interval for the next
  \eqn{k=1} average based on a sample size of \eqn{m} is computed as:
  \deqn{K = t_{n-1, 1-\alpha/2} \sqrt{\frac{1}{m} + \frac{1}{n}} \;\;\;\;\;\; (8)}
  where \eqn{t_{\nu, p}} denotes the \eqn{p}'th quantile of the
  \link[stats:TDist]{Student's t-distribution} with \eqn{\nu}
  degrees of freedom.  For a one-sided prediction interval
  (\code{pi.type="lower"} or \code{pi.type="lower"}), the prediction interval
  is given by:
  \deqn{K = t_{n-1, 1-\alpha} \sqrt{\frac{1}{m} + \frac{1}{n}} \;\;\;\;\;\; (9)}.

  The formulas for these prediction intervals are derived as follows.  Let
  \eqn{\bar{y}} denote the future average based on \eqn{m} observations.  Then
  the quantity \eqn{\bar{y} - \bar{x}} has a normal distribution with expectation
  and variance given by:
  \deqn{E(\bar{y} - \bar{x}) = 0 \;\;\;\;\;\; (10)}
  \deqn{Var(\bar{y} - \bar{x}) = Var(\bar{y}) + Var(\bar{x}) = \frac{\sigma^2}{m} + \frac{\sigma^2}{n} = \sigma^2(\frac{1}{m} + \frac{1}{n}) \;\;\;\;\;\; (11)}
  so the quantity
  \deqn{t = \frac{\bar{y} - \bar{x}}{s\sqrt{\frac{1}{m} + \frac{1}{n}}} \;\;\;\;\;\; (12)}
  has a \link[stats:TDist]{Student's t-distribution} with \eqn{n-1} degrees of freedom.
  \cr

  \bold{The Derivation of K for More than One Future Observation or Average (k >1)} \cr
  When \eqn{k > 1}, the function \code{predIntNormK} allows for two ways to compute
  \eqn{K}:  an exact method due to Dunnett (1955) (\code{method="exact"}), and
  an approximate (conservative) method based on the Bonferroni inequality
  (\code{method="Bonferroni"}; see Miller, 1981a, pp.8, 67-70;
  Gibbons et al., 2009, p.4).  Each of these methods is explained below.
  \cr

  \emph{Exact Method Due to Dunnett (1955)} (\code{method="exact"}) \cr
  Dunnett (1955) derived the value of \eqn{K} in the context of the multiple
  comparisons problem of comparing several treatment means to one control mean.
  The value of \eqn{K} is computed as:
  \deqn{K = c \sqrt{\frac{1}{m} + \frac{1}{n}} \;\;\;\;\;\; (13)}
  where \eqn{c} is a constant that depends on the sample size \eqn{n}, the number of
  future observations (averages) \eqn{k}, the sample size associated with the
  \eqn{k} future averages \eqn{m}, and the confidence level \eqn{(1-\alpha)100\%}.

  When \code{pi.type="lower"} or \code{pi.type="upper"}, the value of \eqn{c} is the
  number that satisfies the following equation (Gupta and Sobel, 1957; Hahn, 1970a):
  \deqn{1 - \alpha = \int_{0}^{\infty} F_1(cs, k, \rho) h(s\sqrt{n-1}, n-1) \sqrt{n-1} ds \;\;\;\;\;\; (14)}
  where
  \deqn{F_1(x, k, \rho) = \int_{\infty}^{\infty} [\Phi(\frac{x + \rho^{1/2}y}{\sqrt{1 - \rho}})]^k \phi(y) dy \;\;\;\;\;\; (15)}
  \deqn{\rho = 1 / (\frac{n}{m} + 1) \;\;\;\;\;\; (16)}
  \deqn{h(x, \nu) = \frac{x^{\nu-1}e^{-x^2/2}}{2^{(\nu/2) - 1} \Gamma(\frac{\nu}{2})} \;\;\;\;\;\; (17)}
  and \eqn{\Phi()} and \eqn{\phi()} denote the cumulative distribution function and
  probability density function, respectively, of the standard normal distribution.
  Note that the function \eqn{h(x, \nu)} is the probability density function of a
  \link[=Chi]{chi random variable} with \eqn{\nu} degrees of freedom.

  When \code{pi.type="two-sided"}, the value of \eqn{c} is the number that satisfies
  the following equation:
  \deqn{1 - \alpha = \int_{0}^{\infty} F_2(cs, k, \rho) h(s\sqrt{n-1}, n-1) \sqrt{n-1} ds \;\;\;\;\;\; (18)}
  where
  \deqn{F_2(x, k, \rho) = \int_{\infty}^{\infty} [\Phi(\frac{x + \rho^{1/2}y}{\sqrt{1 - \rho}}) - \Phi(\frac{-x + \rho^{1/2}y}{\sqrt{1 - \rho}})]^k \phi(y) dy \;\;\;\;\;\; (19)}
  \cr

  \emph{Approximate Method Based on the Bonferroni Inequality} (\code{method="Bonferroni"}) \cr
  As shown above, when \eqn{k=1}, the value of \eqn{K} is given by Equation (8) or
  Equation (9) for two-sided or one-sided prediction intervals, respectively.  When
  \eqn{k > 1}, a conservative way to construct a \eqn{(1-\alpha^*)100\%} prediction
  interval for the next \eqn{k} observations or averages is to use a Bonferroni
  correction (Miller, 1981a, p.8) and set \eqn{\alpha = \alpha^*/k} in Equation (8)
  or (9) (Chew, 1968).  This value of \eqn{K} will be conservative in that the computed
  prediction intervals will be wider than the exact predictions intervals.
  Hahn (1969, 1970a) compared the exact values of \eqn{K} with those based on the
  Bonferroni inequality for the case of \eqn{m=1} and found the approximation to be
  quite satisfactory except when \eqn{n} is small, \eqn{k} is large, and \eqn{\alpha}
  is large.  For example, Gibbons (1987a) notes that for a 99\% prediction interval
  (i.e., \eqn{\alpha = 0.01}) for the next \eqn{k} observations, if \eqn{n > 4},
  the bias of \eqn{K} is never greater than 1\% no matter what the value of \eqn{k}.
}
\value{
  A numeric scalar equal to \eqn{K}, the multiplier of estimated standard
  deviation that is used to construct the prediction interval.
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). \emph{Statistics for Environmental Engineers}.
  Lewis Publishers, Boca Raton.

  Dunnett, C.W. (1955). A Multiple Comparisons Procedure for Comparing Several Treatments
  with a Control. \emph{Journal of the American Statistical Association} \bold{50}, 1096-1121.

  Dunnett, C.W. (1964). New Tables for Multiple Comparisons with a Control.
  \emph{Biometrics} \bold{20}, 482-491.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009).
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.
  John Wiley & Sons, Hoboken.

  Hahn, G.J. (1969). Factors for Calculating Two-Sided Prediction Intervals for
  Samples from a Normal Distribution.
  \emph{Journal of the American Statistical Association} \bold{64}(327), 878-898.

  Hahn, G.J. (1970a). Additional Factors for Calculating Prediction Intervals for
  Samples from a Normal Distribution.
  \emph{Journal of the American Statistical Association} \bold{65}(332), 1668-1676.

  Hahn, G.J. (1970b). Statistical Intervals for a Normal Population, Part I: Tables,
  Examples and Applications. \emph{Journal of Quality Technology} \bold{2}(3), 115-125.

  Hahn, G.J. (1970c). Statistical Intervals for a Normal Population, Part II:
  Formulas, Assumptions, Some Derivations. \emph{Journal of Quality Technology}
  \bold{2}(4), 195-206.

  Hahn, G.J., and W.Q. Meeker. (1991). \emph{Statistical Intervals: A Guide for Practitioners}.
  John Wiley and Sons, New York.

  Hahn, G., and W. Nelson. (1973). A Survey of Prediction Intervals and Their Applications.
  \emph{Journal of Quality Technology} \bold{5}, 178-188.

  Helsel, D.R., and R.M. Hirsch. (1992). \emph{Statistical Methods in Water Resources Research}.
  Elsevier, New York.

  Helsel, D.R., and R.M. Hirsch. (2002). \emph{Statistical Methods in Water Resources}.
  Techniques of Water Resources Investigations, Book 4, chapter A3. U.S. Geological Survey.
  (available on-line at:  \url{https://pubs.usgs.gov/tm/04/a03/tm4a3.pdf}).

  Millard, S.P., and Neerchal, N.K. (2001). \emph{Environmental Statistics with S-PLUS}.
  CRC Press, Boca Raton, Florida.

  Miller, R.G. (1981a). \emph{Simultaneous Statistical Inference}. McGraw-Hill, New York.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
  U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Prediction and tolerance intervals have long been applied to quality control and
  life testing problems (Hahn, 1970b,c; Hahn and Nelson, 1973).
  In the context of environmental statistics, prediction intervals are useful for
  analyzing data from groundwater detection monitoring programs at hazardous and
  solid waste facilities (e.g., Gibbons et al., 2009; Millard and Neerchal, 2001;
  USEPA, 2009).
}
\seealso{
  \code{\link{predIntNorm}}, \code{\link{predIntNormSimultaneous}},
  \code{\link{predIntLnorm}}, \code{\link{tolIntNorm}},
  \link{Normal}, \code{\link{estimate.object}}, \code{\link{enorm}}, \code{\link{eqnorm}}.
}
\examples{
  # Compute the value of K for a two-sided 95% prediction interval
  # for the next observation given a sample size of n=20.

  predIntNormK(n = 20)
  #[1] 2.144711

  #--------------------------------------------------------------------

  # Compute the value of K for a one-sided upper 99% prediction limit
  # for the next 3 averages of order 2 (i.e., each of the 3 future
  # averages is based on a sample size of 2 future observations) given a
  # samle size of n=20.

  predIntNormK(n = 20, n.mean = 2, k = 3, pi.type = "upper",
    conf.level = 0.99)
  #[1] 2.258026

  #----------

  # Compare the result above that is based on the Bonferroni method
  # with the exact method.

  predIntNormK(n = 20, n.mean = 2, k = 3, method = "exact",
    pi.type = "upper", conf.level = 0.99)
  #[1] 2.251084

  #--------------------------------------------------------------------

  # Example 18-1 of USEPA (2009, p.18-9) shows how to construct a 95%
  # prediction interval for 4 future observations assuming a
  # normal distribution based on arsenic concentrations (ppb) in
  # groundwater at a solid waste landfill.  There were 4 years of
  # quarterly monitoring, and years 1-3 are considered background,

  # So the sample size for the prediciton limit is n = 12,
  # and the number of future samples is k = 4.

  predIntNormK(n = 12, k = 4, pi.type = "upper")
  #[1] 2.698976
}
\keyword{ distribution }
\keyword{ htest }

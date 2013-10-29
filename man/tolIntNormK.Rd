\name{tolIntNormK}
\alias{tolIntNormK}
\title{
  Compute the Value of \eqn{K} for a Tolerance Interval for a Normal Distribution
}
\description{
  Compute the value of \eqn{K} (the multiplier of estimated standard deviation) used 
  to construct a tolerance interval based on data from a normal distribution.
}
\usage{
  tolIntNormK(n, df = n - 1, coverage = 0.95, cov.type = "content", 
    ti.type = "two-sided", conf.level = 0.95, method = "exact", 
    rel.tol = 1e-07, abs.tol = rel.tol)
}
\arguments{
  \item{n}{
  a positive integer greater than 2 indicating the sample size upon which the 
  tolerance interval is based.
}
  \item{df}{
  the degrees of freedom associated with the tolerance interval.  The default is 
  \code{df=n-1}.
}
  \item{coverage}{
  a scalar between 0 and 1 indicating the desired coverage of the tolerance interval.  
  The default value is \code{coverage=0.95}.
}
  \item{cov.type}{
  character string specifying the coverage type for the tolerance interval.  
  The possible values are \code{"content"} (\eqn{\beta}-content; the default), and 
  \code{"expectation"} (\eqn{\beta}-expectation).  See the help file for \code{\link{tolIntNorm}} 
  for more information on the difference between \eqn{\beta}-content and \eqn{\beta}-expectation 
  tolerance intervals. 
}
  \item{ti.type}{
  character string indicating what kind of tolerance interval to compute.  
  The possible values are \code{"two-sided"} (the default), \code{"lower"}, and 
  \code{"upper"}.
}
  \item{conf.level}{
  a scalar between 0 and 1 indicating the confidence level associated with the tolerance 
  interval.  The default value is \code{conf.level=0.95}.
}
  \item{method}{
  for the case of a two-sided tolerance interval, a character string specifying the method for 
  constructing the tolerance interval.  This argument is ignored if \code{ti.type="lower"} or 
  \code{ti.type="upper"}.  The possible values are \cr
  \code{"exact"} (the default) and \code{"wald.wolfowitz"} (the Wald-Wolfowitz approximation).  
  See the DETAILS section for more information.
}
  \item{rel.tol}{
  in the case when \code{ti.type="two-sided"} and \code{method="exact"}, the argument 
  \code{rel.tol} is passed to the function \code{\link{integrate}}.  The default value is \cr
  \code{rel.tol=1e-07}.
}
  \item{abs.tol}{
  in the case when \code{ti.type="two-sided"} and \code{method="exact"}, the argument 
  \code{abs.tol} is passed to the function \code{\link{integrate}}.  The default value is the 
  value of \code{rel.tol}.
}
}
\details{
  A tolerance interval for some population is an interval on the real line constructed so as to 
  contain \eqn{100 \beta \%} of the population (i.e., \eqn{100 \beta \%} of all future observations), 
  where \eqn{0 < \beta < 1}.  The quantity \eqn{100 \beta \%} is called the coverage.

  There are two kinds of tolerance intervals (Guttman, 1970):
  \itemize{ 
    \item A \eqn{\beta}-content tolerance interval with confidence level \eqn{100(1-\alpha)\%} is 
    constructed so that it contains at least \eqn{100 \beta \%} of the population (i.e., the 
    coverage is at least \eqn{100 \beta \%}) with probability \eqn{100(1-\alpha)\%}, where 
    \eqn{0 < \alpha < 1}. The quantity \eqn{100(1-\alpha)\%} is called the confidence level or 
    confidence coefficient associated with the tolerance interval.

    \item A \eqn{\beta}-expectation tolerance interval is constructed so that the 
    \emph{average} coverage of the interval is \eqn{100 \beta \%}.
  } 

  \bold{Note:} A \eqn{\beta}-expectation tolerance interval with coverage \eqn{100 \beta \%} is 
  equivalent to a prediction interval for one future observation with associated confidence level 
  \eqn{100 \beta \%}.  Note that there is no explicit confidence level associated with a 
  \eqn{\beta}-expectation tolerance interval.  If a \eqn{\beta}-expectation tolerance interval is 
  treated as a \eqn{\beta}-content tolerance interval, the confidence level associated with this 
  tolerance interval is usually around 50\% (e.g., Guttman, 1970, Table 4.2, p.76).  

  For a normal distribution, the form of a two-sided \eqn{100(1-\alpha)\%} tolerance 
  interval is: \deqn{[\bar{x} - Ks, \, \bar{x} + Ks]} where \eqn{\bar{x}} denotes the sample 
  mean, \eqn{s} denotes the sample standard deviation, and \eqn{K} denotes a constant 
  that depends on the sample size \eqn{n}, the coverage, and, for a \eqn{\beta}-content 
  tolerance interval (but not a \eqn{\beta}-expectation tolerance interval), 
  the confidence level.  

  Similarly, the form of a one-sided lower tolerance interval is: 
  \deqn{[\bar{x} - Ks, \, \infty]} 
  and the form of a one-sided upper tolerance interval is:
  \deqn{[-\infty, \, \bar{x} + Ks]} 
  but \eqn{K} differs for one-sided versus two-sided tolerance intervals.
  \cr

  \strong{The Derivation of \eqn{K} for a \eqn{\beta}-Content Tolerance Interval}

  \emph{One-Sided Case}

  When \code{ti.type="upper"} or \code{ti.type="lower"}, the constant \eqn{K} for a 
  \eqn{100 \beta \%} \eqn{\beta}-content tolerance interval with associated 
  confidence level \eqn{100(1 - \alpha)\%} is given by: 
  \deqn{K = t(n-1, 1 - \alpha, z_\beta \sqrt{n}) / \sqrt{n}}
  where \eqn{t(\nu, p, \delta)} denotes the \eqn{p}'th quantile of a non-central 
  t-distribution with \eqn{\nu} degrees of freedom and noncentrality parameter 
  \eqn{\delta} (see the help file for \link{TDist}), and \eqn{z_p} denotes the 
  \eqn{p}'th quantile of a standard normal distribution.
  \cr
  \cr

  \emph{Two-Sided Case}

  When \code{ti.type="two-sided"} and \code{method="exact"}, the exact formula for 
  the constant \eqn{K} for a \eqn{100 \beta \%} \eqn{\beta}-content tolerance interval 
  with associated confidence level \eqn{100(1-\alpha)\%} requires numerical integration 
  and has been derived by several different authors, including Odeh (1978), 
  Eberhardt et al. (1989), Jilek (1988), Fujino (1989), and Janiga and Miklos (2001).  
  Specifically, for given values of the sample size \eqn{n}, degrees of freedom \eqn{\nu}, 
  confidence level \eqn{(1-\alpha)}, and coverage \eqn{\beta}, the constant \eqn{K} is the 
  solution to the equation: 
  \deqn{\sqrt{\frac{n}{2 \pi}} \, \int^\infty_{-\infty} {F(x, K, \nu, R) \, e^{(-nx^2)/2}} \, dx = 1 - \alpha}
  where \eqn{F(x, K, \nu, R)} denotes the upper-tail area from \eqn{(\nu \, R^2) / K^2} to 
  \eqn{\infty} of the chi-squared distribution with \eqn{\nu} degrees of freedom, and 
  \eqn{R} is the solution to the equation: 
  \deqn{\Phi (x + R) - \Phi (x - R) = \beta} where 
  \eqn{\Phi()} denotes the standard normal cumulative distribuiton function.
  \cr
  \cr
  
  When \code{ti.type="two-sided"} and \code{method="wald.wolfowitz"}, the approximate formula 
  due to Wald and Wolfowitz (1946) for the constant \eqn{K} for a \eqn{100 \beta \%} 
  \eqn{\beta}-content tolerance interval with associated confidence level 
  \eqn{100(1-\alpha)\%} is given by: 
  \deqn{K \approx r \, u} 
  where \eqn{r} is the solution to the equation:  
  \deqn{\Phi (\frac{1}{\sqrt{n}} + r) - \Phi (\frac{1}{\sqrt{n}} - r) = \beta} 
  \eqn{\Phi ()} denotes the standard normal cumulative distribuiton function, and \eqn{u} is 
  given by: 
  \deqn{u = \sqrt{\frac{n-1}{\chi^{2} (n-1, \alpha)}}} 
  where \eqn{\chi^{2} (\nu, p)} denotes the \eqn{p}'th quantile of the chi-squared 
  distribution with \eqn{\nu} degrees of freedom.
  \cr
  \cr

  \strong{The Derivation of \eqn{K} for a \eqn{\beta}-Expectation Tolerance Interval}

  As stated above, a \eqn{\beta}-expectation tolerance interval with coverage \eqn{100 \beta \%} is 
  equivalent to a prediction interval for one future observation with associated confidence level 
  \eqn{100 \beta \%}.  This is because the probability that any single future observation will fall 
  into this interval is \eqn{100 \beta \%}, so the distribution of the number of \eqn{N} future 
  observations that will fall into this interval is binomial with parameters \code{size =} \eqn{N} and 
  \code{prob =} \eqn{\beta} (see the help file for \link{Binomial}).  Hence the expected proportion 
  of future observations that will fall into this interval is \eqn{100 \beta \%} and is independent of 
  the value of \eqn{N}.  See the help file for \code{\link{predIntNormK}} for information on 
  how to derive \eqn{K} for these intervals.
}
\value{
  The value of \eqn{K}, a numeric scalar used to construct tolerance intervals for a normal 
  (Gaussian) distribution.
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002). \emph{Statistics for Environmental Engineers}. 
  Lewis Publishers, Boca Raton.

  Draper, N., and H. Smith. (1998). \emph{Applied Regression Analysis}. Third Edition. 
  John Wiley and Sons, New York.

  Eberhardt, K.R., R.W. Mee, and C.P. Reeve. (1989). Computing Factors for 
  Exact Two-Sided Tolerance Limits for a Normal Distribution. \emph{Communications 
  in Statistics, Part B-Simulation and Computation} \bold{18}, 397-413.

  Ellison, B.E. (1964). On Two-Sided Tolerance Intervals for a Normal Distribution. 
  \emph{Annals of Mathematical Statistics} \bold{35}, 762-772.

  Fujino, T. (1989). Exact Two-Sided Tolerance Limits for a Normal Distribution. 
  \emph{Japanese Journal of Applied Statistics} \bold{18}, 29-36.

  Gibbons, R.D., D.K. Bhaumik, and S. Aryal. (2009). 
  \emph{Statistical Methods for Groundwater Monitoring}, Second Edition.  
  John Wiley & Sons, Hoboken.

  Gilbert, R.O. (1987). \emph{Statistical Methods for Environmental Pollution Monitoring}. 
  Van Nostrand Reinhold, New York.

  Guttman, I. (1970). \emph{Statistical Tolerance Regions: Classical and Bayesian}. 
  Hafner Publishing Co., Darien, CT.

  Hahn, G.J. (1970b). Statistical Intervals for a Normal Population, Part I: Tables, Examples 
  and Applications. \emph{Journal of Quality Technology} \bold{2}(3), 115-125.

  Hahn, G.J. (1970c). Statistical Intervals for a Normal Population, Part II: Formulas, Assumptions, 
  Some Derivations. \emph{Journal of Quality Technology} \bold{2}(4), 195-206.

  Hahn, G.J., and W.Q. Meeker. (1991). \emph{Statistical Intervals: A Guide for Practitioners}. 
  John Wiley and Sons, New York.

  Jilek, M. (1988). \emph{Statisticke Tolerancni Meze}. SNTL, Praha.

  Krishnamoorthy K., and T. Mathew. (2009). 
  \emph{Statistical Tolerance Regions: Theory, Applications, and Computation}. 
  John Wiley and Sons, Hoboken.

  Janiga, I., and R. Miklos. (2001). Statistical Tolerance Intervals for a Normal 
  Distribution.  \emph{Measurement Science Review} \bold{1}1, 29-32.

  Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton.

  Odeh, R.E. (1978). Tables of Two-Sided Tolerance Factors for a Normal Distribution. 
  \emph{Communications in Statistics, Part B-Simulation and Computation} \bold{7}, 183-201.

  Odeh, R.E., and D.B. Owen. (1980). \emph{Tables for Normal Tolerance Limits, Sampling Plans, 
  and Screening}. Marcel Dekker, New York.

  Owen, D.B. (1962). \emph{Handbook of Statistical Tables}. Addison-Wesley, Reading, MA.

  Singh, A., R. Maichle, and N. Armbya. (2010a). 
  \emph{ProUCL Version 4.1.00 User Guide (Draft)}. EPA/600/R-07/041, May 2010. 
  Office of Research and Development, U.S. Environmental Protection Agency, Washington, D.C.

  Singh, A., N. Armbya, and A. Singh. (2010b). 
  \emph{ProUCL Version 4.1.00 Technical Guide (Draft)}. EPA/600/R-07/041, May 2010.  
  Office of Research and Development, U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C.

  USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
  EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
  U.S. Environmental Protection Agency, Washington, D.C. 

  Wald, A., and J. Wolfowitz. (1946). Tolerance Limits for a Normal Distribution. 
  \emph{Annals of Mathematical Statistics} \bold{17}, 208-215.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Tabled values of \eqn{K} are given in Gibbons et al. (2009), Gilbert (1987), 
  Guttman (1970), Krishnamoorthy and Mathew (2009), Owen (1962), Odeh and Owen (1980), 
  and USEPA (2009). 

  Tolerance intervals have long been applied to quality control and 
  life testing problems (Hahn, 1970b,c; Hahn and Meeker, 1991; Krishnamoorthy and Mathew, 2009).  
  References that discuss tolerance intervals in the context of environmental monitoring include:  
  Berthouex and Brown (2002, Chapter 21), Gibbons et al. (2009), 
  Millard and Neerchal (2001, Chapter 6), Singh et al. (2010b), and USEPA (2009).
}
\seealso{
  \code{\link{tolIntNorm}}, \code{\link{predIntNorm}}, \link{Normal}, 
  \code{\link{estimate.object}}, \code{\link{enorm}}, \code{\link{eqnorm}}, 
  \link{Tolerance Intervals}, \link{Prediction Intervals}, 
  \link{Estimating Distribution Parameters}, 
  \link{Estimating Distribution Quantiles}.
}
\examples{
  # Compute the value of K for a two-sided 95% beta-content 
  # tolerance interval with associated confidence level 95% 
  # given a sample size of n=20.

  #----------
  # Exact method

  tolIntNormK(n = 20)
  #[1] 2.760346

  #----------
  # Approximate method due to Wald and Wolfowitz (1946)

  tolIntNormK(n = 20, method = "wald")
  # [1] 2.751789


  #--------------------------------------------------------------------

  # Compute the value of K for a one-sided upper tolerance limit 
  # with 99% coverage and associated confidence level 90% 
  # given a samle size of n=20.

  tolIntNormK(n = 20, ti.type = "upper", coverage = 0.99, 
    conf.level = 0.9)
  #[1] 3.051543

  #--------------------------------------------------------------------

  # Example 17-3 of USEPA (2009, p. 17-17) shows how to construct a 
  # beta-content upper tolerance limit with 95% coverage and 95% 
  # confidence  using chrysene data and assuming a lognormal 
  # distribution.  The sample size is n = 8 observations from 
  # the two compliance wells.  Here we will compute the 
  # multiplier for the log-transformed data.

  tolIntNormK(n = 8, ti.type = "upper")
  #[1] 3.187294
}
\keyword{ distribution }
\keyword{ htest }

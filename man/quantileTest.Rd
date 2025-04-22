\name{quantileTest}
\alias{quantileTest}
\title{
  Two-Sample Rank Test to Detect a Shift in a Proportion of the "Treated" Population
}
\description{
  Two-sample rank test to detect a positive shift in a proportion of one population 
  (here called the \dQuote{treated} population) compared to another (here called the 
  \dQuote{reference} population).  This test is usually called the quantile test 
  (Johnson et al., 1987).
}
\usage{
  quantileTest(x, y, alternative = "greater", target.quantile = 0.5, 
    target.r = NULL, exact.p = TRUE)
}
\arguments{
  \item{x}{
  numeric vector of observations from the \dQuote{treatment} group.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
} 
  \item{y}{
  numeric vector of observations from the \dQuote{reference} group.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are allowed but will be removed.
}
  \item{alternative}{
  character string indicating the kind of alternative hypothesis.  The possible values 
  are \code{"greater"} (right tail of treatment group shifted to the right of the 
  right tail of the reference group) and \code{"less"} (left tail of treatment group 
  shifted to the left of the left tail of the reference group).  The default value is \cr
  \code{alternative="greater"}.
}
  \item{target.quantile}{
  numeric scalar between 0 and 1 indicating the desired quantile to use as the 
  lower cut off point for the test.  Because of the discrete nature of empirical 
  quantiles, the upper bound for the possible empirical quantiles will often differ 
  from the value of \code{target.quantile}.  The default value is 
  \code{target.quantile=0.5} (i.e., the median).  This argument is ignored if the 
  argument \code{target.r} is supplied.
}
  \item{target.r}{
  integer indicating the rank of the observation to use as the lower cut off point 
  for the test.  The value of \code{target.r} must be greater than or equal to 
  2 and less than or equal to \eqn{N} (the total number of valid observations 
  contained in the arguments \code{x} and \code{y}).  The actual rank of the cut off 
  point may differ from \code{target.r} in the case of tied observations in 
  \code{x} and/or \code{y}.  The default value of this argument is \code{NULL}, in 
  which case the argument \code{target.quantile} is used to determine the lower cut 
  off for the test.
}
  \item{exact.p}{
  logical scalar indicating whether to compute the p-value based on the exact 
  distribution of the test statistic (\code{exact.p=TRUE}; the default) or based 
  on the normal approximation (\code{exact.p=FALSE}).
}
}
\details{
  Let \eqn{X} denote a random variable representing measurements from a 
  \dQuote{treatment} group with cumulative distribution function (cdf)
  \deqn{F_X(t) = Pr(X \le t) \;\;\;\;\;\; (1)}
  and let \eqn{x_1, x_2, \ldots, x_m} denote \eqn{m} observations from this 
  treatment group.  Let \eqn{Y} denote a random variable from a \dQuote{reference} 
  group with cdf
  \deqn{F_Y(t) = Pr(Y \le t) \;\;\;\;\;\; (2)}
  and let \eqn{y_1, y_2, \ldots, y_n} denote \eqn{n} observations from this 
  reference group.  Consider the null hypothesis:
  \deqn{H_0: F_X(t) = F_Y(t), \;\; -\infty < t < \infty \;\;\;\;\;\; (3)}
  versus the alternative hypothesis
  \deqn{H_a: F_X(t) = (1 - \epsilon) F_Y(t) + \epsilon F_Z(t) \;\;\;\;\;\; (4)}
  where \eqn{Z} denotes some random variable with cdf
  \deqn{F_Z(t) = Pr(Z \le t) \;\;\;\;\;\; (5)}
  and \eqn{0 < \epsilon \le 1}, \eqn{F_Z(t) \le F_Y(t)} for all values of \eqn{t}, 
  and \eqn{F_Z(t) \ne F_Y(t)} for at least one value of \eqn{t}.

  In English, the alternative hypothesis (4) says that a portion \eqn{\epsilon} of the 
  distribution for the treatment group (the distribution of \eqn{X}) is shifted to the 
  right of the distribution for the reference group (the distribution of \eqn{Y}).  
  The alternative hypothesis (4) with \eqn{\epsilon = 1} is the alternative hypothesis 
  associated with testing a location shift, for which the the 
  \link[stats:wilcox.test]{Wilcoxon rank sum test} can be used. 

  Johnson et al. (1987) investigated locally most powerful rank tests for the test of 
  the null hypothesis (3) against the alternative hypothesis (4).  They considered the 
  case when \eqn{Y} and \eqn{Z} were normal random variables and the case when the 
  densities of \eqn{Y} and \eqn{Z} assumed only two positive values.  For the latter 
  case, the locally most powerful rank test reduces to the following procedure, which 
  Johnson et al. (1987) call the quantile test.

  \enumerate{ 
  \item Combine the \eqn{n} observations from the reference group and the \eqn{m} 
  observations from the treatment group and rank them from smallest to largest.  
  Tied observations receive the average rank of all observations tied at that value.

  \item Choose a quantile \eqn{q} and determine the smallest rank \eqn{r} such that
  \deqn{\frac{r}{m+n+1} > q \;\;\;\;\;\; (6)}
  Note that because of the discrete nature of ranks, any quantile \eqn{q'} such 
  that
  \deqn{\frac{r}{m+n+1} > q' \ge \frac{r-1}{m+n+1} \;\;\;\;\;\; (7)}
  will yield the same value for \eqn{r} as the quantile \eqn{q} does.  
  Alternatively, choose a value of \eqn{r}.  The bounds on an associated quantile 
  are then given in Equation (7).  Note: the component called \code{parameters} in 
  the list returned by \code{quantileTest} contains an element named 
  \code{quantile.ub}.  The value of this element is the left-hand side of Equation (7).

  \item Set \eqn{k} equal to the number of observations from the treatment group 
  (the number of \eqn{X} observations) with ranks bigger than or equal to \eqn{r}.

  \item Under the null hypothesis (3), the probability that at least \eqn{k} out of 
  the \eqn{r} largest observations come from the treatment group is given by:
  \deqn{p = \sum_{i=k}^r \frac{{m+n-r \choose m-i} {r \choose i}}{{m+n \choose n}} \;\;\;\;\;\; (8)}
  This probability may be approximated by:
  \deqn{p = 1 - \Phi(\frac{k - \mu_k - 1/2}{\sigma_k}) \;\;\;\;\;\; (9)}
  where 
  \deqn{\mu_k = \frac{mr}{m+n} \;\;\;\;\;\; (10)}
  \deqn{\sigma_k^2 = \frac{mnr(m+n-r)}{(m+n)^2 (m+n-1)} \;\;\;\;\;\; (11)}
  and \eqn{\Phi} denotes the cumulative distribution function of the standard 
  normal distribution (USEPA, 1994, pp.7.16-7.17).  
  (See \code{\link{quantileTestPValue}}.)

  \item Reject the null hypothesis (3) in favor of the alternative hypothesis (4) at 
  significance level \eqn{\alpha} if \eqn{p \le \alpha}.
  }
 
  Johnson et al. (1987) note that their quantile test is asymptotically equivalent 
  to one proposed by Carrano and Moore (1982) in the context of a two-sided test.  
  Also, when \eqn{q=0.5}, the quantile test reduces to Mood's median test for two 
  groups (see Zar, 2010, p.172; Conover, 1980, pp.171-178).

  The optimal choice of \eqn{q} or \eqn{r} in Step 2 above (i.e., the choice that 
  yields the largest power) depends on the true underlying distributions of 
  \eqn{Y} and \eqn{Z} and the mixing proportion \eqn{\epsilon}.  
  Johnson et al. (1987) performed a simulation study and showed that the quantile 
  test performs better than the Wilcoxon rank sum test and the normal scores test 
  under the alternative of a mixed normal distribution with a shift of at least 
  2 standard deviations in the \eqn{Z} distribution.  USEPA (1994, pp.7.17-7.21) 
  shows that when the mixing proportion \eqn{\epsilon} is small and the shift is 
  large, the quantile test is more powerful than the Wilcoxon rank sum test, and 
  when \eqn{\epsilon} is large and the shift is small the Wilcoxon rank sum test 
  is more powerful than the quantile test.
}
\value{
  A list of class \code{"htestEnvStats"} containing the results of the hypothesis test.  
  See the help file for \cr
  \code{\link{htestEnvStats.object}} for details.
}
\references{
  Carrano, A., and D. Moore. (1982).  The Rationale and Methodology for Quantifying 
  Sister Chromatid Exchange in Humans.  In Heddle, J.A., ed., 
  \emph{Mutagenicity: New Horizons in Genetic Toxocology}.  Academic Press, New York, 
  pp.268-304.

  Conover, W.J. (1980). \emph{Practical Nonparametric Statistics}.  Second Edition. 
  John Wiley and Sons, New York, Chapter 4.

  Johnson, R.A., S. Verrill, and D.H. Moore. (1987).  Two-Sample Rank Tests for 
  Detecting Changes That Occur in a Small Proportion of the Treated Population.  
  \emph{Biometrics} \bold{43}, 641-655.

  Millard, S.P., and N.K. Neerchal. (2001).  \emph{Environmental Statistics with 
  S-PLUS}. CRC Press, Boca Raton, FL, pp.435-439.

  USEPA. (1994).  \emph{Statistical Methods for Evaluating the Attainment of Cleanup 
  Standards, Volume 3: Reference-Based Standards for Soils and Solid Media}.  
  EPA/230-R-94-004.  Office of Policy, Planning, and Evaluation, 
  U.S. Environmental Protection Agency, Washington, D.C.

  Zar, J.H. (2010).  \emph{Biostatistical Analysis}.  Fifth Edition.  
  Prentice-Hall, Upper Saddle River, NJ.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The EPA guidance document \emph{Statistical Methods for Evaluating the Attainment of 
  Cleanup Standards, Volume 3: Reference-Based Standards for Soils and Solid Media} 
  (USEPA, 1994, pp.4.7-4.9) recommends three different statistical tests for 
  determining whether a remediated Superfund site has attained compliance:  
  the Wilcoxon rank sum test, the quantile test, and the \dQuote{hot measurement} 
  comparison test.  The Wilcoxon rank sum test and quantile test are nonparametric 
  tests that compare chemical concentrations in the cleanup area with those in the 
  reference area.  The hot-measurement comparison test compares concentrations in the 
  cleanup area with a pre-specified upper limit value \emph{Hm} (the value of 
  \emph{Hm} must be negotiated between the EPA and the Superfund-site owner or 
  operator).  The Wilcoxon rank sum test is appropriate for detecting uniform failure 
  of remedial action throughout the cleanup area.  The quantile test is appropriate 
  for detecting failure in only a few areas within the cleanup area.  The 
  hot-measurement comparison test is appropriate for detecting hot spots that need to 
  be remediated regardless of the outcomes of the other two tests.

  USEPA (1994, pp.4.7-4.9) recommends applying all three tests to all cleanup units 
  within a cleanup area.  This leads to the usual multiple comparisons problem:  the 
  probability of at least one of the tests indicating non-compliance, when in fact the 
  cleanup area is in compliance, is greater than the pre-set Type I error level for 
  any of the individual tests.  USEPA (1994, p.3.3) recommends against using 
  multiple comparison procedures to control the overall Type I error and suggests 
  instead a re-sampling scheme where additional samples are taken in cases where 
  non-compliance is indicated.
}
\seealso{
  \code{\link{quantileTestPValue}}, \code{\link{wilcox.test}}, 
  \code{\link{htestEnvStats.object}}, \link{Hypothesis Tests}.
}
\examples{
  # Following Example 7.5 on pages 7.23-7.24 of USEPA (1994b), perform the 
  # quantile test for the TcCB data (the data are stored in EPA.94b.tccb.df).  
  # There are n=47 observations from the reference area and m=77 observations 
  # from the cleanup unit.  The target rank is set to 9, resulting in a value 
  # of quantile.ub=0.928.  Note that the p-value is 0.0114, not 0.0117.

  EPA.94b.tccb.df
  #    TcCB.orig   TcCB Censored      Area
  #1        0.22   0.22    FALSE Reference
  #2        0.23   0.23    FALSE Reference
  #...
  #46       1.20   1.20    FALSE Reference
  #47       1.33   1.33    FALSE Reference
  #48      <0.09   0.09     TRUE   Cleanup
  #49       0.09   0.09    FALSE   Cleanup
  #...
  #123     51.97  51.97    FALSE   Cleanup
  #124    168.64 168.64    FALSE   Cleanup

  # Determine the values to use for r and k for 
  # a desired significance level of 0.01 
  #--------------------------------------------

  p.vals <- quantileTestPValue(m = 77, n = 47, 
    r = c(rep(8, 3), rep(9, 3), rep(10, 3)), 
    k = c(6, 7, 8, 7, 8, 9, 8, 9, 10)) 

  round(p.vals, 3) 
  #[1] 0.355 0.122 0.019 0.264 0.081 0.011 0.193 0.053 0.007 

  # Choose r=9, k=9 to get a significance level of 0.011
  #-----------------------------------------------------

  with(EPA.94b.tccb.df, 
    quantileTest(TcCB[Area=="Cleanup"], TcCB[Area=="Reference"], 
    target.r = 9)) 

  #Results of Hypothesis Test
  #--------------------------
  #
  #Null Hypothesis:                 e = 0
  #
  #Alternative Hypothesis:          Tail of Fx Shifted to Right of
  #                                 Tail of Fy.
  #                                 0 < e <= 1, where
  #                                 Fx(t) = (1-e)*Fy(t) + e*Fz(t),
  #                                 Fz(t) <= Fy(t) for all t,
  #                                 and Fy != Fz
  #
  #Test Name:                       Quantile Test
  #
  #Data:                            x = TcCB[Area == "Cleanup"]  
  #                                 y = TcCB[Area == "Reference"]
  #
  #Sample Sizes:                    nx = 77
  #                                 ny = 47
  #
  #Test Statistics:                 k (# x obs of r largest) = 9
  #                                 r                        = 9
  #
  #Test Statistic Parameters:       m           = 77.000
  #                                 n           = 47.000
  #                                 quantile.ub =  0.928
  #
  #P-value:                         0.01136926

  #==========

  # Clean up
  #---------

  rm(p.vals)
}
\keyword{htestEnvStats}
\keyword{models}

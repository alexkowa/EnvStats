\name{aovPower}
\alias{aovPower}
\title{
  Compute the Power of a One-Way Fixed-Effects Analysis of Variance 
}
\description{
  Compute the power of a one-way fixed-effects analysis of variance, 
  given the sample sizes, population means, population standard deviation, and 
  significance level.
}
\usage{
  aovPower(n.vec, mu.vec = rep(0, length(n.vec)), sigma = 1, alpha = 0.05)
}
\arguments{
  \item{n.vec}{
  numeric vector of sample sizes for each group.  The \eqn{i^{th}} element of \code{n.vec} 
  denotes the sample size for group \eqn{i}.  The length of \code{n.vec} must be at least 2, 
  and all elements of \code{n.vec} must be greater than or equal to 2.  Missing (\code{NA}), 
  undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are not allowed.
}
  \item{mu.vec}{
  numeric vector of population means.  The length of \code{mu.vec} must be the 
  same as the length of \code{n.vec}.  The default value is a vector of zeros.  
  Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) 
  values are not allowed.
}
  \item{sigma}{
  numeric scalar specifying the population standard deviation (\eqn{\sigma}) 
  for each group. The default value is \code{sigma=1}.
}
  \item{alpha}{
  numeric scalar between 0 and 1 indicating the Type I error level associated 
  with the hypothesis test.  The default value is \code{alpha=0.05}.
}
}
\details{
  Consider \eqn{k} normally distributed populations with common standard deviation 
  \eqn{\sigma}. Let \eqn{\mu_i} denote the mean of the \eqn{i}'th group 
  (\eqn{i = 1, 2, \ldots, k}), and let 
  \eqn{\underline{x}_i = x_{i1}, x_{i2}, \ldots, x_{in_i}} denote a vector of 
  \eqn{n_i} observations from the \eqn{i}'th group.  
  The statistical method of analysis of variance (ANOVA) tests the null hypothesis:
  \deqn{H_0: \mu_1 = \mu_2 = \cdots = \mu_k \;\;\;\;\;\; (1)}
  against the alternative hypothesis that at least one of the means is different from 
  the rest by using the F-statistic given by:
  \deqn{F = \frac{[\sum_{i=1}^k n_i(\bar{x}_{i.} - \bar{x}_{..})^2]/(k-1)}{[\sum_{i=1}^k \sum_{j=1}^{n_i} (x_{ij} - \bar{x}_{i.})^2]/(N-k)} \;\;\;\;\;\; (2)}
  where
  \deqn{\bar{x}_{i.} = \frac{1}{n_i} \sum_{j=1}^{n_i} x_{ij} \;\;\;\;\;\; (3)}
  \deqn{\bar{x}_{..} = \frac{1}{N} \sum_{i=1}^k n_i\bar{x}_{i.} = \frac{1}{N} \sum_{i=1}^k \sum_{j=1}^{n_i} x_{ij} \;\;\;\;\;\; (4)}
  \deqn{N = \sum_{i=1}^k n_i \;\;\;\;\;\; (5)}
  Under the null hypothesis (1), the F-statistic in (2) follows an 
  \link[stats:FDist]{F-distribution} with \eqn{k-1} and \eqn{N-k} degrees of freedom.  
  Analysis of variance rejects the null hypothesis (1) at significance level 
  \eqn{\alpha} when
  \deqn{F > F_{k-1, N-k}(1 - \alpha) \;\;\;\;\;\; (6)}
  where \eqn{F_{\nu_1, \nu_2}(p)} denotes the \eqn{p}'th quantile of the 
  F-distribution with \eqn{\nu_1} and \eqn{\nu_2} degrees of freedom 
  (Zar, 2010, Chapter 10; Berthouex and Brown, 2002, Chapter 24; 
  Helsel and Hirsh, 1992, pp. 164--169).

  The power of this test, denoted by \eqn{1-\beta}, where \eqn{\beta} denotes the 
  probability of a Type II error, is given by:
  \deqn{1 - \beta = Pr[F_{k-1, N-k, \Delta} > F_{k-1, N-k}(1 - \alpha)] \;\;\;\;\;\; (7)}
  where 
  \deqn{\Delta = \frac{\sum_{i=1}^k n_i(\mu_i - \bar{\mu}_.)^2}{\sigma^2} \;\;\;\;\;\; (8)}
  \deqn{\bar{\mu}_. = \frac{1}{k} \sum_{i=1}^k \mu _i \;\;\;\;\;\; (9)}
  and \eqn{F_{\nu_1, \nu_2, \Delta}} denotes a 
  \link[stats:FDist]{non-central F random variable} with \eqn{\nu_1} and 
  \eqn{\nu_2} degrees of freedom and non-centrality parameter \eqn{\Delta}.  
  Equation (7) can be re-written as:
  \deqn{1 - \beta = 1 - H[F_{k-1, N-k}(1 - \alpha), k-1, N-k, \Delta] \;\;\;\;\;\; (10)}
  where \eqn{H(x, \nu_1, \nu_2, \Delta)} denotes the cumulative distribution function 
  of this random variable evaluated at \eqn{x} (Scheffe, 1959, pp.38--39, 62--65).

  The power of the one-way fixed-effects ANOVA depends on the 
  sample sizes for each of the \eqn{k} groups, the value of the 
  population means for each of the \eqn{k} groups, the population 
  standard deviation \eqn{\sigma}, and the significance level
  \eqn{\alpha}.
}
\value{
  a numeric scalar indicating the power of the one-way fixed-effects ANOVA for 
  the given sample sizes, population means, population standard deviation, and 
  significance level.
}
\references{
  Berthouex, P.M., and L.C. Brown. (2002).  
  \emph{Statistics for Environmental Engineers}.  Second Edition.  
  Lewis Publishers, Boca Raton, FL.

  Helsel, D.R., and R.M. Hirsch. (1992). 
  \emph{Statistical Methods in Water Resources Research}. 
  Elsevier, New York, NY, Chapter 7.

  Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995). 
  \emph{Continuous Univariate Distributions, Volume 2}. 
  Second Edition. John Wiley and Sons, New York, 
  Chapters 27, 29, 30.

  Millard, S.P., and Neerchal, N.K. (2001). \emph{Environmental Statistics with S-PLUS}. 
  CRC Press, Boca Raton, Florida.

  Scheffe, H. (1959). \emph{The Analysis of Variance}. 
  John Wiley and Sons, New York, 477pp.

  USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
  EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.  
  U.S. Environmental Protection Agency, Washington, D.C. p.6-38.

  Zar, J.H. (2010). \emph{Biostatistical Analysis}. 
  Fifth Edition. Prentice-Hall, Upper Saddle River, NJ, 
  Chapter 10.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The normal and lognormal distribution are probably the two most 
  frequently used distributions to model environmental data.  
  Sometimes it is necessary to compare several means to determine 
  whether any are significantly different from each other 
  (e.g., USEPA, 2009, p.6-38).  In this case, assuming 
  normally distributed data, you perform a one-way parametric 
  analysis of variance.

  In the course of designing a sampling program, an environmental 
  scientist may wish to determine the relationship between sample 
  size, Type I error level, power, and differences in means if 
  one of the objectives of the sampling program is to determine 
  whether a particular mean differs from a group of means.  The 
  functions \code{aovPower}, \code{\link{aovN}}, and 
  \code{\link{plotAovDesign}} can be used to investigate these 
  relationships for the case of normally-distributed observations.
}
\seealso{
  \code{\link{aovN}}, \code{\link{plotAovDesign}}, 
  \code{\link{Normal}}, \code{\link{aov}}.
}
\examples{
  # Look at how the power of a one-way ANOVA increases 
  # with increasing sample size:

  aovPower(n.vec = rep(5, 3), mu.vec = c(10, 15, 20), sigma = 5) 
  #[1] 0.7015083 

  aovPower(n.vec = rep(10, 3), mu.vec = c(10, 15, 20), sigma = 5) 
  #[1] 0.9732551

  #----------------------------------------------------------------

  # Look at how the power of a one-way ANOVA increases 
  # with increasing variability in the population means:

  aovPower(n.vec = rep(5,3), mu.vec = c(10, 10, 11), sigma=5) 
  #[1] 0.05795739 

  aovPower(n.vec = rep(5, 3), mu.vec = c(10, 10, 15), sigma = 5) 
  #[1] 0.2831863 

  aovPower(n.vec = rep(5, 3), mu.vec = c(10, 13, 15), sigma = 5) 
  #[1] 0.2236093 

  aovPower(n.vec = rep(5, 3), mu.vec = c(10, 15, 20), sigma = 5) 
  #[1] 0.7015083

  #----------------------------------------------------------------

  # Look at how the power of a one-way ANOVA increases 
  # with increasing values of Type I error:

  aovPower(n.vec = rep(10,3), mu.vec = c(10, 12, 14), 
    sigma = 5, alpha = 0.001) 
  #[1] 0.02655785 

  aovPower(n.vec = rep(10,3), mu.vec = c(10, 12, 14), 
    sigma = 5, alpha = 0.01) 
  #[1] 0.1223527 

  aovPower(n.vec = rep(10,3), mu.vec = c(10, 12, 14), 
    sigma = 5, alpha = 0.05) 
  #[1] 0.3085313 

  aovPower(n.vec = rep(10,3), mu.vec = c(10, 12, 14), 
    sigma = 5, alpha = 0.1) 
  #[1] 0.4373292

  #==========

  # The example on pages 5-11 to 5-14 of USEPA (1989b) shows 
  # log-transformed concentrations of lead (mg/L) at two 
  # background wells and four compliance wells, where observations 
  # were taken once per month over four months (the data are 
  # stored in EPA.89b.loglead.df.)  Assume the true mean levels 
  # at each well are 3.9, 3.9, 4.5, 4.5, 4.5, and 5, respectively. 
  # Compute the power of a one-way ANOVA to test for mean 
  # differences between wells.  Use alpha=0.05, and assume the 
  # true standard deviation is equal to the one estimated from 
  # the data in this example.

  # First look at the data 
  names(EPA.89b.loglead.df)
  #[1] "LogLead"   "Month"     "Well"      "Well.type"

  dev.new()
  stripChart(LogLead ~ Well, data = EPA.89b.loglead.df,
    show.ci = FALSE, xlab = "Well Number", 
    ylab="Log [ Lead (ug/L) ]", 
    main="Lead Concentrations at Six Wells")

  # Note: The assumption of a constant variance across 
  # all wells is suspect.


  # Now perform the ANOVA and get the estimated sd 
  aov.list <- aov(LogLead ~ Well, data=EPA.89b.loglead.df) 

  summary(aov.list) 
  #            Df Sum Sq Mean Sq F value  Pr(>F)  
  #Well         5 5.7447 1.14895  3.3469 0.02599 *
  #Residuals   18 6.1791 0.34328                  
  #---
  #Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 '' 1 

  # Now call the function aovPower 
  aovPower(n.vec = rep(4, 6), 
    mu.vec = c(3.9,3.9,4.5,4.5,4.5,5), sigma=sqrt(0.34)) 
  #[1] 0.5523148

  # Clean up
  rm(aov.list)
}
\keyword{ design }
\keyword{ htest }
\keyword{ models }
\keyword{ regression }

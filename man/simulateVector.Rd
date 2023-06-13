\name{simulateVector}
\alias{simulateVector}
\title{
  Simulate a Vector of Random Numbers From a Specified Theoretical or Empirical Probability Distribution
}
\description{
  Simulate a vector of random numbers from a specified theoretical probability
  distribution or empirical probability distribution, using either Latin Hypercube
  sampling or simple random sampling.
}
\usage{
  simulateVector(n, distribution = "norm", param.list = list(mean = 0, sd = 1),
    sample.method = "SRS", seed = NULL, sorted = FALSE,
    left.tail.cutoff = ifelse(is.finite(supp.min), 0, .Machine$double.eps),
    right.tail.cutoff = ifelse(is.finite(supp.max), 0, .Machine$double.eps))
}
\arguments{
  \item{n}{
  a positive integer indicating the number of random numbers to generate.
}
  \item{distribution}{
  a character string denoting the distribution abbreviation.  The default value is
  \code{distribution="norm"}.  See the help file for \code{\link{Distribution.df}}
  for a list of possible distribution abbreviations.

  Alternatively, the character string \code{"emp"} may be used to denote sampling
  from an empirical distribution based on a set of observations.  The vector
  containing the observations is specified in the argument \code{param.list}.
}
  \item{param.list}{
  a list with values for the parameters of the distribution.
  The default value is \code{param.list=list(mean=0, sd=1)}.
  See the help file for \code{\link{Distribution.df}} for the names and
  possible values of the parameters associated with each distribution.

  Alternatively, if you specify an empirical distribution by setting \cr
  \code{distribution="emp"}, then \code{param.list} must be a list of the
  form \code{list(obs=}\emph{name}\code{)}, where \emph{name} denotes the
  name of the vector containing the observations to use for the empirical
  distribution.  In this case, you may also supply arguments to the
  \code{\link{qemp}} function through \code{param.list}.  For example, you
  may set \cr
  \code{param.list=list(obs=}\emph{name}\code{, discrete=T)} to
  specify an empirical distribution based on a discrete random variable.
}
  \item{sample.method}{
  a character string indicating whether to use simple random sampling \cr
  (\code{sample.method="SRS"}, the default) or
  Latin Hypercube sampling \cr
  (\code{sample.method="LHS"}).
}
  \item{seed}{
  integer to supply to the \R function \code{\link{set.seed}}.
  The default value is \code{seed=NULL}, in which case the random seed is
  not set but instead based on the current value of \code{.Random.seed}.
}
  \item{sorted}{
  logical scalar indicating whether to return the random numbers in sorted
  (ascending) order.  The default value is \code{sorted=FALSE}.
}
  \item{left.tail.cutoff}{
  a scalar between 0 and 1 indicating what proportion of the left-tail of
  the probability distribution to omit for Latin Hypercube sampling.
  For densities with a finite support minimum (e.g., \link{Lognormal} or
  \link{Empirical}) the default value is \code{left.tail.cutoff=0};
  for densities with a support minimum of \eqn{-\infty}, the default value is
  \code{left.tail.cutoff=.Machine$double.eps}.
  This argument is ignored if \code{sample.method="SRS"}.
}
  \item{right.tail.cutoff}{
  a scalar between 0 and 1 indicating what proportion of the right-tail of
  the probability distribution to omit for Latin Hypercube sampling.
  For densities with a finite support maximum (e.g., \link{Beta} or
  \link{Empirical}) the default value is \code{right.tail.cutoff=0};
  for densities with a support maximum of \eqn{\infty}, the default value
  is \code{right.tail.cutoff=.Machine$double.eps}.
  This argument is ignored if \code{sample.method="SRS"}.
}
}
\details{
  \bold{Simple Random Sampling} (\code{sample.method="SRS"}) \cr
  When \code{sample.method="SRS"}, the function \code{simulateVector} simply
  calls the function \code{r}\emph{abb}, where \emph{abb} denotes the
  abbreviation of the specified distribution (e.g., \code{\link{rlnorm}},
  \code{\link{remp}}, etc.).
  \cr

  \bold{Latin Hypercube Sampling} (\code{sample.method="LHS"}) \cr
  When \code{sample.method="LHS"}, the function \code{simulateVector} generates
  \code{n} random numbers using Latin Hypercube sampling.  The distribution is
  divided into \code{n} intervals of equal probability \eqn{1/n} and simple random
  sampling is performed once within each interval; i.e., Latin Hypercube sampling
  is simply stratified sampling without replacement, where the strata are defined
  by the 0'th, 100(1/n)'th, 100(2/n)'th, ..., and 100'th percentiles of the
  distribution.

  \bold{\emph{Latin Hypercube sampling}}, sometimes abbreviated \bold{\emph{LHS}},
  is a method of sampling from a probability distribution that ensures all
  portions of the probability distribution are represented in the sample.
  It was introduced in the published literature by McKay et al. (1979) to overcome
  the following problem in Monte Carlo simulation based on simple random sampling
  (SRS).  Suppose we want to generate random numbers from a specified distribution.
  If we use simple random sampling, there is a low probability of getting very many
  observations in an area of low probability of the distribution.  For example, if
  we generate \eqn{n} observations from the distribution, the probability that none
  of these observations falls into the upper 98'th percentile of the distribution
  is \eqn{0.98^n}.  So, for example, there is a 13\% chance that out of 100
  random numbers, none will fall at or above the 98'th percentile.  If we are
  interested in reproducing the shape of the distribution, we will need a very large
  number of observations to ensure that we can adequately characterize the tails of
  the distribution (Vose, 2008, pp. 59--62).

  See Millard (2013) for a visual explanation of Latin Hypercube sampling.
}
\value{
  a numeric vector of random numbers from the specified distribution.
}
\references{
  Iman, R.L., and W.J. Conover. (1980).  Small Sample Sensitivity Analysis
  Techniques for Computer Models, With an Application to Risk Assessment
  (with Comments).  \emph{Communications in Statistics--Volume A, Theory and Methods},
  \bold{9}(17), 1749--1874.

  Iman, R.L., and J.C. Helton. (1988).  An Investigation of Uncertainty and
  Sensitivity Analysis Techniques for Computer Models.  \emph{Risk Analysis}
  \bold{8}(1), 71--90.

  Iman, R.L. and J.C. Helton. (1991).  The Repeatability of Uncertainty and
  Sensitivity Analyses for Complex Probabilistic Risk Assessments.
  \emph{Risk Analysis} \bold{11}(4), 591--606.

  McKay, M.D., R.J. Beckman., and W.J. Conover. (1979).  A Comparison of Three
  Methods for Selecting Values of Input Variables in the Analysis of Output
  From a Computer Code.  \emph{Technometrics} \bold{21}(2), 239--245.

  Millard, S.P. (2013).  \emph{EnvStats: an R Package for Environmental Statistics}.
  Springer, New York.  \url{https://link.springer.com/book/10.1007/978-1-4614-8456-1}.

  Vose, D. (2008).  \emph{Risk Analysis:  A Quantitative Guide}.  Third Edition.
  John Wiley & Sons, West Sussex, UK, 752 pp.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  \bold{\emph{Latin Hypercube sampling}}, sometimes abbreviated \bold{\emph{LHS}},
  is a method of sampling from a probability distribution that ensures all
  portions of the probability distribution are represented in the sample.
  It was introduced in the published literature by McKay et al. (1979).
  Latin Hypercube sampling is often used in probabilistic risk assessment,
  specifically for sensitivity and uncertainty analysis
  (e.g., Iman and Conover, 1980; Iman and Helton, 1988; Iman and Helton, 1991;
  Vose, 1996).
}
\seealso{
  \link{Probability Distributions and Random Numbers}, \link{Empirical},
  \code{\link{simulateMvMatrix}}, \code{\link{set.seed}}.
}
\examples{
  # Generate 10 observations from a lognormal distribution with
  # parameters mean=10 and cv=1 using simple random sampling:

  simulateVector(10, distribution = "lnormAlt",
    param.list = list(mean = 10, cv = 1), seed = 47,
    sort = TRUE)
  # [1]  2.086931  2.863589  3.112866  5.592502  5.732602  7.160707
  # [7]  7.741327  8.251306 12.782493 37.214748

  #----------

  # Repeat the above example by calling rlnormAlt directly:

  set.seed(47)
  sort(rlnormAlt(10, mean = 10, cv = 1))
  # [1]  2.086931  2.863589  3.112866  5.592502  5.732602  7.160707
  # [7]  7.741327  8.251306 12.782493 37.214748

  #----------

  # Now generate 10 observations from the same lognormal distribution
  # but use Latin Hypercube sampling.  Note that the largest value
  # is larger than for simple random sampling:

  simulateVector(10, distribution = "lnormAlt",
    param.list = list(mean = 10, cv = 1), seed = 47,
    sample.method = "LHS", sort = TRUE)
  # [1]  2.406149  2.848428  4.311175  5.510171  6.467852  8.174608
  # [7]  9.506874 12.298185 17.022151 53.552699

  #==========

  # Generate 50 observations from a Pareto distribution with parameters
  # location=10 and shape=2, then use this resulting vector of
  # observations as the basis for generating 3 observations from an
  # empirical distribution using Latin Hypercube sampling:

  set.seed(321)
  pareto.rns <- rpareto(50, location = 10, shape = 2)

  simulateVector(3, distribution = "emp",
    param.list = list(obs = pareto.rns), sample.method = "LHS")
  #[1] 11.50685 13.50962 17.47335

  #==========

  # Clean up
  #---------
  rm(pareto.rns)
}
\keyword{ distribution }
\keyword{ datagen }

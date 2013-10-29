\name{lMoment}
\alias{lMoment}
\title{
  Estimate \eqn{L}-Moments
}
\description{
  Estimate the \eqn{r}'th \eqn{L}-moment from a random sample.
}
\usage{
  lMoment(x, r = 1, method = "unbiased", 
    plot.pos.cons = c(a = 0.35, b = 0), na.rm = FALSE)
}
\arguments{
  \item{x}{
  numeric vector of observations. 
}
  \item{r}{
  positive integer specifying the order of the moment.
}
  \item{method}{
  character string specifying what method to use to compute the 
  \eqn{L}-moment.  The possible values are \code{"unbiased"} 
  (method based on the U-statistic; the default), or \code{"plotting.position"} 
  (method based on the plotting position formula).  See the DETAILS section for 
  more information.
}
  \item{plot.pos.cons}{
  numeric vector of length 2 specifying the constants used in the formula for the 
  plotting positions when \code{method="plotting.position"}.  The default value is 
  \code{plot.pos.cons=c(a=0.35, b=0)}.  If this vector has a names attribute with 
  the value \code{c("a","b")} or \code{c("b","a")}, then the elements will be 
  matched by name in the formula for computing the plotting positions.  Otherwise, 
  the first element is mapped to the name \code{"a"} and the second element to the 
  name \code{"b"}.  See the DETAILS section for more information.  This argument is 
  ignored if \code{method="ubiased"}.
}
  \item{na.rm}{
  logical scalar indicating whether to remove missing values from \code{x}.  
  If \code{na.rm=FALSE} (the default) and \code{x} contains missing values, 
  then a missing value (\code{NA}) is returned.  If \code{na.rm=TRUE}, missing 
  values are removed from \code{x} prior to computing the \eqn{L}-moment.
}
}
\details{
  \bold{Definitions: \eqn{L}-Moments and \eqn{L}-Moment Ratios} \cr
  The definition of an \eqn{L}-moment given by Hosking (1990) is as follows.  
  Let \eqn{X} denote a random variable with cdf \eqn{F}, and let \eqn{x(p)} 
  denote the \eqn{p}'th quantile of the distribution.  Furthermore, let
  \deqn{x_{1:n} \le x_{2:n} \le \ldots \le x_{n:n}}
  denote the order statistics of a random sample of size \eqn{n} drawn from the 
  distribution of \eqn{X}.  Then the \eqn{r}'th \eqn{L}-moment is given by:
  \deqn{\lambda_r = \frac{1}{r} \sum^{r-1}_{k=0} (-1)^k {r-1 \choose k} E[X_{r-k:r}]}
  for \eqn{r = 1, 2, \ldots}.

  Hosking (1990) shows that the above equation can be rewritten as:
  \deqn{\lambda_r = \int^1_0 x(u) P^*_{r-1}(u) du}
  where
  \deqn{P^*_r(u) = \sum^r_{k=0} p^*_{r,k} u^k}
  \deqn{p^*_{r,k} = (-1)^{r-k} {r \choose k} {r+k \choose k} = \frac{(-1)^{r-k} (r+k)!}{(k!)^2 (r-k)!}}
  The first four \eqn{L}-moments are given by: 
  \deqn{\lambda_1 = E[X]}
  \deqn{\lambda_2 = \frac{1}{2} E[X_{2:2} - X_{1:2}]}
  \deqn{\lambda_3 = \frac{1}{3} E[X_{3:3} - 2X_{2:3} + X_{1:3}]}
  \deqn{\lambda_4 = \frac{1}{4} E[X_{4:4} - 3X_{3:4} + 3X_{2:4} - X_{1:4}]}
  Thus, the first \eqn{L}-moment is a measure of location, and the second 
  \eqn{L}-moment is a measure of scale. 

  Hosking (1990) defines the \eqn{L}-moment ratios of \eqn{X} to be:
  \deqn{\tau_r = \frac{\lambda_r}{\lambda_2}}
  for \eqn{r = 2, 3, \ldots}.  He shows that for a non-degenerate random variable 
  with a finite mean, these quantities lie in the interval \eqn{(-1, 1)}.  
  The quantity
  \deqn{\tau_3 = \frac{\lambda_3}{\lambda_2}}
  is the \eqn{L}-moment analog of the coefficient of skewness, and the quantity
  \deqn{\tau_4 = \frac{\lambda_4}{\lambda_2}}
  is the \eqn{L}-moment analog of the coefficient of kurtosis.  Hosking (1990) also 
  defines an \eqn{L}-moment analog of the coefficient of variation (denoted the 
  \eqn{L}-CV) as:
  \deqn{\lambda = \frac{\lambda_2}{\lambda_1}}
  He shows that for a positive-valued random variable, the \eqn{L}-CV lies 
  in the interval \eqn{(0, 1)}.

  \bold{Relationship Between \eqn{L}-Moments and Probability-Weighted Moments} \cr
  Hosking (1990) and Hosking and Wallis (1995) show that \eqn{L}-moments can be 
  written as linear combinations of probability-weighted moments:
  \deqn{\lambda_r = (-1)^{r-1} \sum^{r-1}_{k=0} p^*_{r-1,k} \alpha_k = \sum^{r-1}_{j=0} p^*_{r-1,j} \beta_j}
  where 
  \deqn{\alpha_k = M(1, 0, k) = \frac{1}{k+1} E[X_{1:k+1}]}
  \deqn{\beta_j = M(1, j, 0) = \frac{1}{j+1} E[X_{j+1:j+1}]}
  See the help file for \code{\link{pwMoment}} for more information on 
  probability-weighted moments.

  \bold{Estimating L-Moments} \cr
  The two commonly used methods for estimating \eqn{L}-moments are the 
  \dQuote{unbiased} method based on U-statistics (Hoeffding, 1948; 
  Lehmann, 1975, pp. 362-371), and the \dQuote{plotting-position} method.  
  Hosking and Wallis (1995) recommend using the unbiased method for almost all 
  applications.

  \emph{Unbiased Estimators} (\code{method="unbiased"}) \cr
  Using the relationship between \eqn{L}-moments and probability-weighted moments 
  explained above, the unbiased estimator of the \eqn{r}'th \eqn{L}-moment is based on 
  unbiased estimators of probability-weighted moments and is given by:
  \deqn{l_r = (-1)^{r-1} \sum^{r-1}_{k=0} p^*_{r-1,k} a_k = \sum^{r-1}_{j=0} p^*_{r-1,j} b_j}
  where
  \deqn{a_k = \frac{1}{n} \sum^{n-k}_{i=1} x_{i:n} \frac{{n-i \choose k}}{{n-1 \choose k}}}
  \deqn{b_j = \frac{1}{n} \sum^{n}_{i=j+1} x_{i:n} \frac{{i-1 \choose j}}{{n-1 \choose j}}}

  \emph{Plotting-Position Estimators} (\code{method="plotting.position"}) \cr
  Using the relationship between \eqn{L}-moments and probability-weighted moments 
  explained above, the plotting-position estimator of the \eqn{r}'th \eqn{L}-moment 
  is based on the plotting-position estimators of probability-weighted moments and 
  is given by:
  \deqn{\tilde{\lambda}_r = (-1)^{r-1} \sum^{r-1}_{k=0} p^*_{r-1,k} \tilde{\alpha}_k = \sum^{r-1}_{j=0} p^*_{r-1,j} \tilde{\beta}_j}
  where
  \deqn{\tilde{\alpha}_k = \frac{1}{n} \sum^n_{i=1} (1 - p_{i:n})^k x_{i:n}}
  \deqn{\tilde{\beta}_j =  \frac{1}{n} \sum^{n}_{i=1} p^j_{i:n} x_{i:n}}
  and
  \deqn{p_{i:n} = \hat{F}(x_{i:n})}
  denotes the plotting position of the \eqn{i}'th order statistic in the random 
  sample of size \eqn{n}, that is, a distribution-free estimate of the cdf of 
  \eqn{X} evaluated at the \eqn{i}'th order statistic.  Typically, plotting 
  positions have the form:
  \deqn{p_{i:n} = \frac{i-a}{n+b}}
  where \eqn{b > -a > -1}.  For this form of plotting position, the 
  plotting-position estimators are asymptotically equivalent to their 
  unbiased estimator counterparts. 

  \bold{Estimating \eqn{L}-Moment Ratios} \cr
  \eqn{L}-moment ratios are estimated by simply replacing the population 
  \eqn{L}-moments with the estimated \eqn{L}-moments.  The estimated ratios 
  based on the unbiased estimators are given by:
  \deqn{t_r = \frac{l_r}{l_2}}
  and the estimated ratios based on the plotting-position estimators are given by:
  \deqn{\tilde{\tau}_r = \frac{\tilde{\lambda}_r}{\tilde{\lambda}_2}}
  In particular, the \eqn{L}-moment skew is estimated by:
  \deqn{t_3 = \frac{l_3}{l_2}}
  or 
  \deqn{\tilde{\tau}_3 = \frac{\tilde{\lambda}_3}{\tilde{\lambda}_2}}
  and the \eqn{L}-moment kurtosis is estimated by:
  \deqn{t_4 = \frac{l_4}{l_2}}
  or 
  \deqn{\tilde{\tau}_4 = \frac{\tilde{\lambda}_4}{\tilde{\lambda}_2}}
  Similarly, the \eqn{L}-moment coefficient of variation can be estimated using 
  the unbiased \eqn{L}-moment estimators:
  \deqn{l = \frac{l_2}{l_1}}
  or using the plotting-position L-moment estimators:
  \deqn{\tilde{\lambda} = \frac{\tilde{\lambda}_2}{\tilde{\lambda}_1}}
}
\value{
  A numeric scalar--the value of the \eqn{r}'th \eqn{L}-moment as defined by Hosking (1990).
}
\references{
  Fill, H.D., and J.R. Stedinger. (1995).  \eqn{L} Moment and Probability Plot 
  Correlation Coefficient Goodness-of-Fit Tests for the Gumbel Distribution and 
  Impact of Autocorrelation.  \emph{Water Resources Research} \bold{31}(1), 225--229.

  Hosking, J.R.M. (1990).  L-Moments: Analysis and Estimation of Distributions 
  Using Linear Combinations of Order Statistics.  \emph{Journal of the Royal 
  Statistical Society, Series B} \bold{52}(1), 105--124.
 
  Hosking, J.R.M., and J.R. Wallis (1995).  A Comparison of Unbiased and 
  Plotting-Position Estimators of \eqn{L} Moments.  \emph{Water Resources Research} 
  \bold{31}(8), 2019--2025.

  Vogel, R.M., and N.M. Fennessey. (1993).  \eqn{L} Moment Diagrams Should 
  Replace Product Moment Diagrams.  \emph{Water Resources Research} \bold{29}(6), 
  1745--1752.
}
\author{
    Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  Hosking (1990) introduced the idea of \eqn{L}-moments, which are expectations 
  of certain linear combinations of order statistics, as the basis of a general 
  theory of describing theoretical probability distributions, computing summary 
  statistics from observed data, estimating distribution parameters and quantiles, 
  and performing hypothesis tests.  The theory of \eqn{L}-moments parallels the 
  theory of conventional moments.  \eqn{L}-moments have several advantages over 
  conventional moments, including:
  \itemize{
    \item \eqn{L}-moments can characterize a wider range of distributions because 
    they always exist as long as the distribution has a finite mean.

    \item \eqn{L}-moments are estimated by linear combinations of order statistics, 
    so estimators based on \eqn{L}-moments are more robust to the presence of 
    outliers than estimators based on conventional moments.

    \item Based on the author's and others' experience, \eqn{L}-moment estimators 
    are less biased and approximate their asymptotic distribution more closely in 
    finite samples than estimators based on conventional moments.

    \item \eqn{L}-moment estimators are sometimes more efficient (smaller RMSE) than 
    even maximum likelihood estimators for small samples.
  }
 
  Hosking (1990) presents a table with formulas for the \eqn{L}-moments of common 
  probability distributions.  Articles that illustrate the use of \eqn{L}-moments 
  include Fill and Stedinger (1995), Hosking and Wallis (1995), and 
  Vogel and Fennessey (1993).

  Hosking (1990) and Hosking and Wallis (1995) show the relationship between 
  probabiity-weighted moments and \eqn{L}-moments.
}
\seealso{
  \code{\link{cv}}, \code{\link{skewness}}, \code{\link{kurtosis}}, 
  \code{\link{pwMoment}}.
}
\examples{
  # Generate 20 observations from a generalized extreme value distribution 
  # with parameters location=10, scale=2, and shape=.25, then compute the 
  # first four L-moments. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(250) 
  dat <- rgevd(20, location = 10, scale = 2, shape = 0.25) 

  lMoment(dat) 
  #[1] 10.59556 

  lMoment(dat, 2) 
  #[1] 1.0014 

  lMoment(dat, 3) 
  #[1] 0.1681165
 
  lMoment(dat, 4) 
  #[1] 0.08732692

  #----------

  # Now compute some L-moments based on the plotting-position estimators:

  lMoment(dat, method = "plotting.position") 
  #[1] 10.59556

  lMoment(dat, 2, method = "plotting.position") 
  #[1] 1.110264 

  lMoment(dat, 3, method="plotting.position", plot.pos.cons = c(.325,1)) 
  #[1] -0.4430792
 
  #----------

  # Clean up
  #---------
  rm(dat)
}
\keyword{ distribution }
\keyword{ htest }

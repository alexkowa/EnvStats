\name{Empirical}
\alias{Empirical}
\alias{demp}
\alias{pemp}
\alias{qemp}
\alias{remp}
\title{
  The Empirical Distribution Based on a Set of Observations
}
\description{
  Density, distribution function, quantile function, and random generation for 
  the empirical distribution based on a set of observations
}
\usage{
  demp(x, obs, discrete = FALSE, density.arg.list = NULL)
  pemp(q, obs, discrete = FALSE, 
    prob.method = ifelse(discrete, "emp.probs", "plot.pos"), 
    plot.pos.con = 0.375) 
  qemp(p, obs, discrete = FALSE, 
    prob.method = ifelse(discrete, "emp.probs", "plot.pos"), 
    plot.pos.con = 0.375)
  remp(n, obs)
}
\arguments{
  \item{x}{
  vector of quantiles.
}
  \item{q}{
  vector of quantiles.
}
  \item{p}{
  vector of probabilities between 0 and 1.
}
  \item{n}{
  sample size.  If \code{length(n)} is larger than 1, then \code{length(n)} 
  random values are returned.
}
  \item{obs}{
  numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), and 
  infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
}
  \item{discrete}{
  logical scalar indicating whether the assumed parent distribution of \code{x} is 
  discrete (\code{discrete=TRUE}) or continuous (\code{discrete=FALSE}).  The 
  default value is \code{FALSE}.
}
  \item{density.arg.list}{
  list with arguments to the \R \code{\link{density}} function.  The default value is 
  \code{NULL}.  (See the help file for \code{\link{density}} 
  for more information on the arguments to density.)  The argument 
  \code{density.arg.list} is ignored if \code{discrete=TRUE}.
}
  \item{prob.method}{
  character string indicating what method to use to compute the empirical 
  probabilities.  Possible values are \code{"emp.probs"} (empirical probabilities, 
  default if \code{discrete=TRUE}) and \code{"plot.pos"} (plotting positions, 
  default if \code{discrete=FALSE}).  See the DETAILS section for more explanation.
}
  \item{plot.pos.con}{
  numeric scalar between 0 and 1 containing the value of the plotting position 
  constant.  The default value is \code{plot.pos.con=0.375}.  See the DETAILS 
  section for more information. This argument is ignored if 
  \code{prob.method="emp.probs"}.
}
}
\details{
  Let \eqn{x_1, x_2, \ldots, x_n} denote a random sample of n observations 
  from some unknown probability distribution (i.e., the elements of the argument 
  \code{obs}), and let \eqn{x_{(i)}} denote the \eqn{i^{th}} order statistic, that is, 
  the \eqn{i^{th}} largest observation, for \eqn{i = 1, 2, \ldots, n}.

  \emph{Estimating Density} \cr
  The function \code{demp} computes the empirical probability density function.  If 
  the observations are assumed to come from a discrete distribution, the probability 
  density (mass) function is estimated by:
  \deqn{\hat{f}(x) = \widehat{Pr}(X = x) = \frac{\sum^n_{i=1} I_{[x]}(x_i)}{n}}
  where \eqn{I} is the indicator function: 
  \tabular{lll}{
    \eqn{I_{[x]}(y) =} \tab \eqn{1} \tab if \eqn{y = x}, \cr
                       \tab \eqn{0} \tab if \eqn{y \ne x} 
  }
  That is, the estimated probability of observing the value \eqn{x} is simply the 
  observed proportion of observations equal to \eqn{x}.

  If the observations are assumed to come from a continuous distribution, the 
  function \code{demp} calls the \R function \code{\link{density}} to compute the 
  estimated density based on the values specified in the argument \code{obs}, 
  and then uses linear interpolation to estimate the density at the values 
  specified in the argument \code{x}.  See the \R help file for 
  \code{\link{density}} for more information on how the empirical density is 
  computed in the continuous case.

  \emph{Estimating Probabilities} \cr
  The function \code{pemp} computes the estimated cumulative distribution function 
  (cdf), also called the empirical cdf (ecdf).  If the observations are assumed to 
  come from a discrete distribution, the value of the cdf evaluated at the \eqn{i^{th}} 
  order statistic is usually estimated by: 
  \deqn{\hat{F}[x_{(i)}] = \widehat{Pr}(X \le x_{(i)}) = \hat{p}_i = 
    \frac{\sum^n_{j=1} I_{(-\infty, x_{(i)}]}(x_j)}{n}}
  where: 
  \tabular{lll}{
    \eqn{I_{(-\infty, x]}(y) =} \tab \eqn{1} \tab if \eqn{y \le x}, \cr
                                \tab \eqn{0} \tab if \eqn{y > x} 
  }
  (D'Agostino, 1986a).  That is, the estimated value of the cdf at the \eqn{i^{th}} 
  order statistic is simply the observed proportion of observations less than or 
  equal to the \eqn{i^{th}} order statistic.  This estimator is sometimes called the 
  \dQuote{empirical probabilities} estimator and is intuitively appealing.  
  The function \code{pemp} uses the above equations to compute the empirical cdf when 
  \code{prob.method="emp.probs"}.

  For any general value of \eqn{x}, when the observations are assumed to come from a 
  discrete distribution, the value of the cdf is estimated by:  
  \tabular{lll}{
    \eqn{\hat{F}(x) =} \tab \eqn{0}         \tab if \eqn{x < x_{(1)}}, \cr
                       \tab \eqn{\hat{p}_i} \tab if \eqn{x_{(i)} \le x < x_{(i+1)}}, \cr
                       \tab \eqn{1}         \tab if \eqn{x \ge x_{(n)}} 
  }  
  The function \code{pemp} uses the above equation when \code{discrete=TRUE}.

  If the observations are assumed to come from a continuous distribution, the value 
  of the cdf evaluated at the \eqn{i^{th}} order statistic is usually estimated by: 
  \deqn{\hat{F}[x_{(i)}] = \hat{p}_i = \frac{i - a}{n - 2a + 1}}
  where \eqn{a} denotes the plotting position constant and \eqn{0 \le a \le 1} 
  (Cleveland, 1993, p.18; D'Agostino, 1986a, pp.8,25).  The estimators defined by 
  the above equation are called \emph{plotting positions} and are used to construct 
  \link[=qqPlot]{probability plots}.  The function \code{pemp} uses the above equation 
  when \cr 
  \code{prob.method="plot.pos"}.

  For any general value of \eqn{x}, the value of the cdf is estimated by linear 
  interpolation: 
  \tabular{lll}{
    \eqn{\hat{F}(x) =} \tab \eqn{\hat{p}_1} \tab if \eqn{x < x_{(1)}}, \cr
                       \tab \eqn{(1 - r)\hat{p}_i + r\hat{p}_{i+1}} \tab if \eqn{x_{(i)} \le x < x_{(i+1)}}, \cr
                       \tab \eqn{\hat{p}_n} \tab if \eqn{x \ge x_{(n)}} 
  }
  where
  \deqn{r = \frac{x - x_{(i)}}{x_{(i+1)} - x_{(i)}}}
  (Chambers et al., 1983).  The function \code{pemp} uses the above two equations 
  when \code{discrete=FALSE}.

  \emph{Estimating Quantiles} \cr
  The function \code{qemp} computes the estimated quantiles based on the observed 
  data.  If the observations are assumed to come from a discrete distribution, the 
  \eqn{p^{th}} quantile is usually estimated by: 
  \tabular{lll}{
    \eqn{\hat{x}_p =} \tab \eqn{x_{(1)}} \tab if \eqn{p \le \hat{p}_1}, \cr
                      \tab \eqn{x_{(i)}} \tab if \eqn{\hat{p}_{i-1} < p \le \hat{p}_i}, \cr
                      \tab \eqn{x_n}     \tab if \eqn{p > \hat{p}_n} 
  }
  The function \code{qemp} uses the above equation when \code{discrete=TRUE}.

  If the observations are assumed to come from a continuous distribution, the 
  \eqn{p^{th}} quantile is usually estimated by linear interpolation: 
  \tabular{lll}{
    \eqn{\hat{x}_p =} \tab \eqn{x_{(1)}} \tab if \eqn{p \le \hat{p}_1}, \cr
                      \tab \eqn{(1 - r)x_{(i-1)} + rx_{(i)}} \tab if \eqn{\hat{p}_{i-1} < p \le \hat{p}_i}, \cr
                      \tab \eqn{x_n} \tab if \eqn{p > \hat{p}_n} \cr
  }
  where
  \deqn{r = \frac{p - \hat{p}_{i-1}}{\hat{p}_i - \hat{p}_{i-1}}}
  The function \code{qemp} uses the above two equations when \code{discrete=FALSE}.

  \emph{Generating Random Numbers From the Empirical Distribution} \cr
  The function \code{remp} simply calls the \R function \code{\link{sample}} to 
  sample the elements of \code{obs} with replacement.
}
\value{
  density (\code{demp}), probability (\code{pemp}), quantile (\code{qemp}), or 
  random sample (\code{remp}) for the empirical distribution based on the data 
  contained in the vector \code{obs}.
}
\references{
  Chambers, J.M., W.S. Cleveland, B. Kleiner, and P.A. Tukey. (1983).  
  \emph{Graphical Methods for Data Analysis}.  Duxbury Press, Boston, MA, 
  pp.11--16.

  Cleveland, W.S. (1993).  \emph{Visualizing Data}.  Hobart Press, Summit, 
  New Jersey, 360pp.

  D'Agostino, R.B. (1986a).  Graphical Analysis.  
  In: D'Agostino, R.B., and M.A. Stephens, eds. \emph{Goodness-of Fit Techniques}. 
  Marcel Dekker, New York, Chapter 2, pp.7--62.

  Scott, D. W. (1992).  
  \emph{Multivariate Density Estimation:  Theory, Practice and Visualization}.  
  John Wiley and Sons, New York. 

  Sheather, S. J. and Jones M. C. (1991).  A Reliable Data-Based Bandwidth Selection 
  Method for Kernel Density Estimation.  
  \emph{Journal of the Royal Statististical Society B}, 683--690. 

  Silverman, B.W. (1986).  \emph{Density Estimation for Statistics and Data Analysis}.  
  Chapman and Hall, London.

  Wegman, E.J. (1972).  Nonparametric Probability Density Estimation.  
  \emph{Technometrics} \bold{14}, 533-546.
}
\author{
  Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
}
\note{
  The function \code{demp} let's you perform nonparametric density estimation.  
  The function \code{pemp} computes the value of the empirical cumulative 
  distribution function (ecdf) for user-specified quantiles.  The ecdf is a 
  nonparametric estimate of the true cdf (see \code{\link{ecdfPlot}}).  The 
  function \code{qemp} computes nonparametric estimates of quantiles 
  (see the help files for \code{\link{eqnpar}} and \code{\link{quantile}}).  
  The function \code{remp} let's you sample a set of observations with replacement, 
  which is often done while bootstrapping or performing some other kind of 
  Monte Carlo simulation.
}
\seealso{
  \code{\link{density}}, \code{\link{approx}}, \code{\link{epdfPlot}}, 
  \code{\link{ecdfPlot}}, \code{\link{cdfCompare}}, \code{\link{qqplot}}, 
  \code{\link{eqnpar}}, \code{\link{quantile}}, \code{\link{sample}}, \cr
  \code{\link{simulateVector}}, \code{\link{simulateMvMatrix}}.
}
\examples{
  # Create a set of 100 observations from a gamma distribution with 
  # parameters shape=4 and scale=5. 
  # (Note: the call to set.seed simply allows you to reproduce this example.)

  set.seed(3) 
  obs <- rgamma(100, shape=4, scale=5)

  # Now plot the empirical distribution (with a histogram) and the true distribution:

  dev.new()
  hist(obs, col = "cyan", xlim = c(0, 65), freq = FALSE, 
    ylab = "Relative Frequency") 

  pdfPlot('gamma', list(shape = 4, scale = 5), add = TRUE) 

  box()

  # Now plot the empirical distribution (based on demp) with the 
  # true distribution:

  x <- qemp(p = seq(0, 1, len = 100), obs = obs) 
  y <- demp(x, obs) 

  dev.new()
  plot(x, y, xlim = c(0, 65), type = "n", 
    xlab = "Value of Random Variable", 
    ylab = "Relative Frequency") 
  lines(x, y, lwd = 2, col = "cyan") 

  pdfPlot('gamma', list(shape = 4, scale = 5), add = TRUE)

  # Alternatively, you can create the above plot with the function 
  # epdfPlot:

  dev.new()
  epdfPlot(obs, xlim = c(0, 65), epdf.col = "cyan", 
    xlab = "Value of Random Variable", 
    main = "Empirical and Theoretical PDFs")

  pdfPlot('gamma', list(shape = 4, scale = 5), add = TRUE)

  


  # Clean Up
  #---------
  rm(obs, x, y)

}
\keyword{ distribution }
\keyword{ datagen }

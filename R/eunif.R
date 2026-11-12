#' Estimate Parameters of a Uniform Distribution
#' @description
#' Estimate the minimum and maximum parameters of a
#'   \link[stats:Uniform]{uniform distribution}.
#' @usage
#' eunif(x, method = "mle")
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), and
#'   infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
#' }
#'   \item{method}{
#'   character string specifying the method of estimation.  The possible values are
#'   \code{"mle"} (maximum likelihood; the default), \code{"mme"} (method of moments),
#'   and \code{"mmue"} (method of moments based on the unbiased estimator of variance).
#'   See the DETAILS section for more information on these estimation methods.
#' }
#' }
#' @rawRd
#' \details{
#'   If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or
#'   infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to
#'   performing the estimation.
#'
#'   Let \eqn{\underline{x} = (x_1, x_2, \ldots, x_n)} be a vector of
#'   \eqn{n} observations from an \link[stats:Uniform]{uniform distribution} with
#'   parameters \code{min=}\eqn{a} and \code{max=}\eqn{b}.  Also, let \eqn{x_{(i)}}
#'   denote the \eqn{i}'th order statistic.
#'
#'   \bold{Estimation} \cr
#'
#'   \emph{Maximum Likelihood Estimation} (\code{method="mle"}) \cr
#'   The maximum likelihood estimators (mle's) of \eqn{a} and \eqn{b} are given by
#'   (Johnson et al, 1995, p.286):
#'   \deqn{\hat{a}_{mle} = x_{(1)} \;\;\;\; (1)}
#'   \deqn{\hat{b}_{mle} = x_{(n)} \;\;\;\; (2)}
#'   \cr
#'
#'   \emph{Method of Moments Estimation} (\code{method="mme"}) \cr
#'   The method of moments estimators (mme's) of \eqn{a} and \eqn{b} are given by
#'   (Forbes et al., 2011):
#'   \deqn{\hat{a}_{mme} = \bar{x} - \sqrt{3} s_m \;\;\;\; (3)}
#'   \deqn{\hat{b}_{mme} = \bar{x} + \sqrt{3} s_m \;\;\;\; (4)}
#'   where
#'   \deqn{\bar{x} = \frac{1}{n} \sum_{i=1}^n x_i \;\;\;\; (5)}
#'   \deqn{s^2_m = \frac{1}{n} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\; (6)}
#'   \cr
#'
#'   \emph{Method of Moments Estimation Based on the Unbiased Estimator of Variance} (\code{method="mmue"}) \cr
#'   The method of moments estimators based on the unbiased estimator of variance are
#'   exactly the same as the method of moments estimators given in equations (3-6) above,
#'   except that the method of moments estimator of variance in equation (6) is replaced
#'   with the unbiased estimator of variance:
#'   \deqn{\hat{a}_{mmue} = \bar{x} - \sqrt{3} s \;\;\;\; (7)}
#'   \deqn{\hat{b}_{mmue} = \bar{x} + \sqrt{3} s \;\;\;\; (8)}
#'   where
#'   \deqn{s^2 = \frac{1}{n-1} \sum_{i=1}^n (x_i - \bar{x})^2 \;\;\;\; (9)}
#' }
#' @rawRd
#' \value{
#'   a list of class \code{"estimate"} containing the estimated parameters and other
#'   information.  See \cr
#'   \code{\link{estimate.object}} for details.
#' }
#' @rawRd
#' \references{
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995).
#'   \emph{Continuous Univariate Distributions, Volume 2}.
#'   Second Edition. John Wiley and Sons, New York.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The \link[stats:Uniform]{uniform distribution} (also called the rectangular
#'   distribution) with parameters \code{min} and \code{max} takes on values on the
#'   real line between \code{min} and \code{max} with equal probability.  It has been
#'   used to represent the distribution of round-off errors in tabulated values.  Another
#'   important application is that the distribution of the cumulative distribution
#'   function (cdf) of any kind of continuous random variable follows a uniform
#'   distribution with parameters \code{min=0} and \code{max=1}.
#' }
#' @rawRd
#' \seealso{
#'   \link[stats]{Uniform}, \code{\link{estimate.object}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 20 observations from a uniform distribution with parameters
#'   # min=-2 and max=3, then estimate the parameters via maximum likelihood.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(250)
#'   dat <- runif(20, min = -2, max = 3)
#'   eunif(dat)
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Uniform
#'   #
#'   #Estimated Parameter(s):          min = -1.574529
#'   #                                 max =  2.837006
#'   #
#'   #Estimation Method:               mle
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'
#'   #----------
#'
#'   # Compare the three methods of estimation:
#'
#'   eunif(dat, method = "mle")$parameters
#'   #      min       max
#'   #-1.574529  2.837006
#'
#'
#'   eunif(dat, method = "mme")$parameters
#'   #      min       max
#'   #-1.988462  2.650737
#'
#'
#'   eunif(dat, method = "mmue")$parameters
#'   #      min       max
#'   #-2.048721  2.710996
#'
#'   #----------
#'
#'   # Clean up
#'   #---------
#'   rm(dat)
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ htest }

eunif <-
function (x, method = "mle") 
{
    if (!is.vector(x, mode = "numeric") || is.factor(x)) 
        stop("'x' must be a numeric vector")
    data.name <- deparse(substitute(x))
    if ((bad.obs <- sum(!(x.ok <- is.finite(x)))) > 0) {
        is.not.finite.warning(x)
        x <- x[x.ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' removed."))
    }
    n <- length(x)
    if (n < 2 || length(unique(x)) < 2) 
        stop(paste("'x' must contain at least 2 non-missing distinct values. ", 
            "This is not true for 'x' =", data.name))
    method <- match.arg(method, c("mle", "mme", "mmue"))
    switch(method, mme = {
        a <- mean(x)
        h <- sqrt(3) * sqrt((n - 1)/n) * sd(x)
        dist.params <- c(min = a - h, max = a + h)
    }, mmue = {
        a <- mean(x)
        h <- sqrt(3) * sd(x)
        dist.params <- c(min = a - h, max = a + h)
    }, mle = {
        dist.params <- c(min = min(x), max = max(x))
    })
    ret.list <- list(distribution = "Uniform", sample.size = n, 
        parameters = dist.params, n.param.est = 2, method = method, 
        data.name = data.name, bad.obs = bad.obs)
    oldClass(ret.list) <- "estimate"
    ret.list
}

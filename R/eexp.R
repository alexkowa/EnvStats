#' Estimate Rate Parameter of an Exponential Distribution
#' @description
#' Estimate the rate parameter of an
#'   \link[stats:Exponential]{exponential distribution}, and optionally construct a
#'   confidence interval for the rate parameter.
#' @usage
#' eexp(x, method = "mle/mme", ci = FALSE, ci.type = "two-sided",
#'     ci.method = "exact", conf.level = 0.95)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   numeric vector of observations.
#' }
#'   \item{method}{
#'   character string specifying the method of estimation.  Currently the only
#'   possible value is \code{"mle/mme"}
#'   (maximum likelihood/method of moments; the default).  See the DETAILS section for
#'   more information.
#' }
#'   \item{ci}{
#'   logical scalar indicating whether to compute a confidence interval for the
#'   location or scale parameter.  The default value is \code{FALSE}.
#' }
#'   \item{ci.type}{
#'   character string indicating what kind of confidence interval to compute.  The
#'   possible values are \code{"two-sided"} (the default), \code{"lower"}, and
#'   \code{"upper"}.  This argument is ignored if \code{ci=FALSE}.
#' }
#'   \item{ci.method}{
#'   character string indicating what method to use to construct the confidence interval
#'   for the location or scale parameter.  Currently, the only possible value is
#'   \code{"exact"} (the default).  See the DETAILS section for more information.
#'   This argument is ignored if \code{ci=FALSE}.
#' }
#'   \item{conf.level}{
#'   a scalar between 0 and 1 indicating the confidence level of the confidence interval.
#'   The default value is \code{conf.level=0.95}. This argument is ignored if
#'   \code{ci=FALSE}.
#' }
#' }
#' @rawRd
#' \details{
#'   If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or
#'   infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to
#'   performing the estimation.
#'
#'   Let \eqn{\underline{x} = (x_1, x_2, \ldots, x_n)} be a vector of \eqn{n}
#'   observations from an \link[stats:Exponential]{exponential distribution} with
#'   parameter \code{rate=}\eqn{\lambda}.
#'
#'   \emph{Estimation} \cr
#'   The maximum likelihood estimator (mle) of \eqn{\lambda} is given by:
#'   \deqn{\hat{\lambda}_{mle} = \frac{1}{\bar{x}}}
#'   where
#'   \deqn{\bar{x} = \frac{1}{n}\sum^n_{i=1} x_i}
#'   (Forbes et al., 2011).  That is, the mle is the reciprocal of the sample mean.
#'
#'   Sometimes the exponential distribution is parameterized with a scale parameter
#'   instead of a rate parameter.  The scale parameter is the reciprocal of the rate
#'   parameter, and the sample mean is both the mle and the minimum variance unbiased
#'   estimator (mvue) of the scale parameter.
#'
#'   \emph{Confidence Interval} \cr
#'   When \code{ci=TRUE}, an exact \eqn{(1-\alpha)100\%} confidence intervals for
#'   \eqn{\lambda} can be constructed based on the relationship between the
#'   exponential distribution, the \link[stats:GammaDist]{gamma distribution}, and
#'   the \link[stats:Chisquare]{chi-square distribution}.  An exponential distribution
#'   with parameter \code{rate=}\eqn{\lambda} is equivalent to a gamma distribution
#'   with parameters \code{shape=1} and \code{scale=}\eqn{1/\lambda}.  The sum of
#'   \eqn{n} iid gamma random variables with parameters \code{shape=1} and
#'   \code{scale=}\eqn{1/\lambda} is a gamma random variable with parameters
#'   \code{shape=}\eqn{n} and \code{scale=}\eqn{1/\lambda}.  Finally, a gamma
#'   distribution with parameters \code{shape=}\eqn{n} and \code{scale=}\eqn{1/\lambda}
#'   is equivalent to 0.5 times a chi-square distribution with degrees of freedom
#'   \code{df=}\eqn{2n}. Thus, the quantity \eqn{2n\bar{x}} has a chi-square
#'   distribution with degrees of freedom \code{df=}\eqn{2n}.
#'
#'   A two-sided \eqn{(1-\alpha)100\%} confidence interval for \eqn{\lambda} is
#'   therefore constructed as:
#'   \deqn{[\frac{\chi^2(2n, \alpha/2)}{2n\bar{x}}, \; \frac{chi^2(2n, 1 - \alpha/2)}{2n\bar{x}} ]}
#'   where \eqn{\chi^2(\nu,p)} is the \eqn{p}'th quantile of a
#'   \link[stats:Chisquare]{chi-square distribution} with \eqn{\nu} degrees of freedom.
#'
#'   One-sided confidence intervals are computed in a similar fashion.
#' }
#' @rawRd
#' \value{
#'   a list of class \code{"estimate"} containing the estimated parameters and other information.
#'   See \cr
#'   \code{\link{estimate.object}} for details.
#' }
#' @rawRd
#' \references{
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994).
#'   \emph{Continuous Univariate Distributions, Volume 1}.
#'   Second Edition. John Wiley and Sons, New York.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The \link[stats:Exponential]{exponential distribution} is a special case of the
#'   \link[stats:GammaDist]{gamma distribution}, and
#'   takes on positive real values.  A major use of the exponential distribution is
#'   in life testing where it is used to model the lifetime of a product, part,
#'   person, etc.
#'
#'   The exponential distribution is the only continuous distribution with a
#'   \dQuote{lack of memory} property.  That is, if the lifetime of a part follows
#'   the exponential distribution, then the distribution of the time until failure
#'   is the same as the distribution of the time until failure given that the part
#'   has survived to time \eqn{t}.
#'
#'   The exponential distribution is related to the double exponential (also called
#'   Laplace) distribution, and to the \link[=EVD]{extreme value distribution}.
#' }
#' @rawRd
#' \seealso{
#'   \link[stats:Exponential]{Exponential}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 20 observations from an exponential distribution with parameter
#'   # rate=2, then estimate the parameter and construct a 90% confidence interval.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(250)
#'   dat <- rexp(20, rate = 2)
#'   eexp(dat, ci=TRUE, conf = 0.9)
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Exponential
#'   #
#'   #Estimated Parameter(s):          rate = 2.260587
#'   #
#'   #Estimation Method:               mle/mme
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     20
#'   #
#'   #Confidence Interval for:         rate
#'   #
#'   #Confidence Interval Method:      Exact
#'   #
#'   #Confidence Interval Type:        two-sided
#'   #
#'   #Confidence Level:                90%
#'   #
#'   #Confidence Interval:             LCL = 1.498165
#'   #                                 UCL = 3.151173
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

eexp <-
function (x, method = "mle/mme", ci = FALSE, ci.type = "two-sided", 
    ci.method = "exact", conf.level = 0.95) 
{
    if (!is.vector(x, mode = "numeric")) 
        stop("'x' must be a numeric vector")
    data.name <- deparse(substitute(x))
    method <- match.arg(method)
    if ((bad.obs <- sum(!(x.ok <- is.finite(x)))) > 0) {
        is.not.finite.warning(x)
        x <- x[x.ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' removed."))
    }
    n <- length(x)
    if (n < 1 || any(x < 0) || all(x == 0)) 
        stop(paste("'x' must contain at least one non-missing value,", 
            "all non-missing values of 'x' must be non-negative,", 
            "and at least one value of 'x' must be positive."))
    rate <- 1/mean(x)
    ret.list <- list(distribution = "Exponential", sample.size = n, 
        parameters = c(rate = rate), n.param.est = 1, method = method, 
        data.name = data.name, bad.obs = bad.obs)
    if (ci) {
        ci.type <- match.arg(ci.type, c("two-sided", "lower", 
            "upper"))
        ci.method <- match.arg(ci.method)
        if (conf.level <= 0 || conf.level >= 1) 
            stop("The value of 'conf.level' must be between 0 and 1.")
        ci.obj <- switch(ci.method, exact = ci.exp.exact(rate = rate, 
            n = n, ci.type = ci.type, alpha = 1 - conf.level))
        ret.list <- c(ret.list, list(interval = ci.obj))
    }
    oldClass(ret.list) <- "estimate"
    ret.list
}

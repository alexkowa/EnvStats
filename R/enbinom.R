#' Estimate Probability Parameter of a Negative Binomial Distribution
#' @description
#' Estimate the probability parameter of a
#'   \link[stats:NegBinomial]{negative binomial distribution}.
#' @usage
#' enbinom(x, size, method = "mle/mme")
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of non-negative integers indicating the number of trials that took place
#'   \emph{before} \code{size} \dQuote{successes} occurred.  (The total number of
#'   trials that took place is \code{x+1}).  Missing (\code{NA}), undefined (\code{NaN}),
#'   and infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.  If
#'   \code{length(x)=n} and \code{n} is greater than 1, it is assumed that \code{x}
#'   represents observations from \code{n} separate negative binomial experiments that
#'   all had the same probability of success (\code{prob}), but possibly different
#'   values of \code{size}.
#' }
#'   \item{size}{
#'   vector of positive integers indicating the number of \dQuote{successes} that
#'   must be observed before the trials are stopped.  Missing (\code{NA}),
#'   undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf}) values are allowed
#'   but will be removed.  The length of \code{size} must be 1 or else the same
#'   length as \code{x}.
#' }
#'   \item{method}{
#'   character string specifying the method of estimation.  Possible values are: \cr
#'   \code{"mle/mme"} (maximum likelihood and method of moments; the default) and \cr
#'   \code{"mvue"} (minimum variance unbiased).  \cr
#'   You cannot use \code{method="mvue"} if
#'   the sum of the elements in \code{size} is 1.  See the DETAILS section for more
#'   information on these estimation methods.
#' }
#' }
#' @rawRd
#' \details{
#'   If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or
#'   infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to
#'   performing the estimation.
#'
#'   Let \eqn{\underline{x} = (x_1, x_2, \ldots, x_n)} be a vector of \eqn{n}
#'   independent observations from \link[stats:NegBinomial]{negative binomial distributions}
#'   with parameters \code{prob=}\eqn{p} and \code{size=}\eqn{\underline{k}}, where
#'   where \eqn{\underline{k} = c(k_1, k_2, \ldots, k_n)} is a vector of \eqn{n}
#'   (possibly different) values.
#'
#'   It can be shown (e.g., Forbes et al., 2011) that if \eqn{X} is defined as:
#'   \deqn{X = \sum^n_{i = 1} x_i}
#'   then \eqn{X} is an observation from a
#'   \link[stats:NegBinomial]{negative binomial distribution} with
#'   parameters \code{prob=}\eqn{p} and \code{size=}\eqn{K}, where
#'   \deqn{K = \sum^n_{i = 1} k_i}
#'
#'   \emph{Estimation} \cr
#'   The maximum likelihood and method of moments estimator (mle/mme) of
#'   \eqn{p} is given by:
#'   \deqn{\hat{p}_{mle} = \frac{K}{X + K}}
#'   and the minimum variance unbiased estimator (mvue) of \eqn{p} is given by:
#'   \deqn{\hat{p}_{mvue} = \frac{K - 1}{X + K - 1}}
#'   (Forbes et al., 2011).  Note that the mvue of \eqn{p} is not defined for
#'   \eqn{K=1}.
#' }
#' @rawRd
#' \value{
#'   a list of class \code{"estimate"} containing the estimated parameters and other information. \cr
#'   See \code{\link{estimate.object}} for details.
#' }
#' @rawRd
#' \references{
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Johnson, N. L., S. Kotz, and A. Kemp. (1992).
#'   \emph{Univariate Discrete Distributions}.  Second Edition. John Wiley and Sons,
#'   New York, Chapter 5.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The \link[stats:NegBinomial]{negative binomial distribution} has its roots in
#'   a gambling game where participants would bet on the number of tosses of a
#'   coin necessary to achieve a fixed number of heads.  The negative binomial
#'   distribution has been applied in a wide variety of fields, including accident
#'   statistics, birth-and-death processes, and modeling spatial distributions of
#'   biological organisms.
#'
#'   The \link[stats:Geometric]{geometric distribution} with parameter \code{prob=}\eqn{p}
#'   is a special case of the negative binomial distribution with parameters
#'   \code{size=1} and \code{prob=}\eqn{p}.
#' }
#' @rawRd
#' \seealso{
#'   \link[stats]{NegBinomial}, \code{\link{egeom}}, \link[stats]{Geometric}.
#' }
#' @rawRd
#' \examples{
#'   # Generate an observation from a negative binomial distribution with
#'   # parameters size=2 and prob=0.2, then estimate the parameter prob.
#'   # Note: the call to set.seed simply allows you to reproduce this example.
#'   # Also, the only parameter that is estimated is prob; the parameter
#'   # size is supplied in the call to enbinom.  The parameter size is printed in
#'   # order to show all of the parameters associated with the distribution.
#'
#'   set.seed(250)
#'   dat <- rnbinom(1, size = 2, prob = 0.2)
#'   dat
#'   #[1] 5
#'
#'   enbinom(dat, size = 2)
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Negative Binomial
#'   #
#'   #Estimated Parameter(s):          size = 2.0000000
#'   #                                 prob = 0.2857143
#'   #
#'   #Estimation Method:               mle/mme for 'prob'
#'   #
#'   #Data:                            dat, 2
#'   #
#'   #Sample Size:                     1
#'
#'   #----------
#'
#'   # Generate 3 observations from negative binomial distributions with
#'   # parameters size=c(2,3,4) and prob=0.2, then estimate the parameter
#'   # prob using the mvue.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   size.vec <- 2:4
#'   set.seed(250)
#'   dat <- rnbinom(3, size = size.vec, prob = 0.2)
#'   dat
#'   #[1]  5 19 12
#'
#'   enbinom(dat, size = size.vec, method = "mvue")
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Negative Binomial
#'   #
#'   #Estimated Parameter(s):          size = 9.0000000
#'   #                                 prob = 0.1818182
#'   #
#'   #Estimation Method:               mvue for 'prob'
#'   #
#'   #Data:                            dat, size.vec
#'   #
#'   #Sample Size:                     3
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

enbinom <-
function (x, size, method = "mle/mme") 
{
    if (!is.vector(x, mode = "numeric") || is.factor(x) || !is.vector(size, 
        mode = "numeric") || is.factor(size)) 
        stop("'x' and 'size' must be numeric vectors")
    data.name <- paste(deparse(substitute(x)), deparse(substitute(size)), 
        sep = ", ")
    lx <- length(x)
    lsize <- length(size)
    pass <- (lx == lsize) || (lsize == 1)
    if (!pass) 
        stop(paste("The length 'size' must be 1 or the same", 
            "length as 'x'."))
    if (lx > lsize) 
        size <- rep(size, lx)
    if ((bad.obs <- sum(!(all.ok <- is.finite(x) & is.finite(size)))) > 
        0) {
        is.not.finite.warning(x)
        is.not.finite.warning(size)
        x <- x[all.ok]
        size <- size[all.ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' and/or 'size' removed."))
    }
    n <- length(x)
    if (n < 1) 
        stop("'x' and 'size' must contain at least one non-missing pair of values.")
    if (!all(x == trunc(x)) || any(x < 0) || !all(size == trunc(size)) || 
        any(size < 1)) 
        stop(paste("All values of 'x' must be non-negative integers,", 
            "and all values of 'size' must be positive integers."))
    x <- sum(x)
    size <- sum(size)
    method <- match.arg(method, c("mle/mme", "mvue"))
    dist.params <- switch(method, `mle/mme` = c(size = size, 
        prob = size/(size + x)), mvue = c(size = size, prob = (size - 
        1)/(size + x - 1)))
    ret.list <- list(distribution = "Negative Binomial", sample.size = n, 
        parameters = dist.params, n.param.est = 1, method = paste(method, 
            "for 'prob'"), data.name = data.name, bad.obs = bad.obs)
    oldClass(ret.list) <- "estimate"
    ret.list
}

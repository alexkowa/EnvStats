#' Estimate Probability Parameter of a Geometric Distribution
#' @description
#' Estimate the probability parameter of a
#'   \link[stats:Geometric]{geometric distribution}.
#' @usage
#' egeom(x, method = "mle/mme")
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   vector of non-negative integers indicating the number of trials that took place
#'   \emph{before} the first \dQuote{success} occurred.  (The total number of trials
#'   that took place is \code{x+1}).  Missing (\code{NA}), undefined (\code{NaN}), and
#'   infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.  If
#'   \code{length(x)=n} and \code{n} is greater than 1, it is assumed that \code{x}
#'   represents observations from \code{n} separate geometric experiments that all had
#'   the same probability of success (\code{prob}).
#' }
#'   \item{method}{
#'   character string specifying the method of estimation.  Possible values are \cr
#'   \code{"mle/mme"} (maximum likelihood and method of moments; the default) and
#'   \code{"mvue"} (minimum variance unbiased).  You cannot use \code{method="mvue"} if \cr
#'   \code{length(x)=1}.  See the DETAILS section for more information on these
#'   estimation methods.
#' }
#' }
#' @rawRd
#' \details{
#'   If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or
#'   infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to
#'   performing the estimation.
#'
#'   Let \eqn{\underline{x} = (x_1, x_2, \ldots, x_n)} be a vector of \eqn{n}
#'   independent observations from a \link[stats:Geometric]{geometric distribution}
#'   with parameter \code{prob=}\eqn{p}.
#'
#'   It can be shown (e.g., Forbes et al., 2011) that if \eqn{X} is defined as:
#'   \deqn{X = \sum^n_{i = 1} x_i}
#'   then \eqn{X} is an observation from a
#'   \link[stats:NegBinomial]{negative binomial distribution} with
#'   parameters \code{prob=}\eqn{p} and \code{size=}\eqn{n}.
#'
#'   \emph{Estimation} \cr
#'   The maximum likelihood and method of moments estimator (mle/mme) of
#'   \eqn{p} is given by:
#'   \deqn{\hat{p}_{mle} = \frac{n}{X + n}}
#'   and the minimum variance unbiased estimator (mvue) of \eqn{p} is given by:
#'   \deqn{\hat{p}_{mvue} = \frac{n - 1}{X + n - 1}}
#'   (Forbes et al., 2011).  Note that the mvue of \eqn{p} is not defined for
#'   \eqn{n=1}.
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
#'   The \link[stats:Geometric]{geometric distribution} with parameter
#'   \code{prob=}\eqn{p} is a special case of the
#'   \link[stats:NegBinomial]{negative binomial distribution} with parameters
#'   \code{size=1} and \code{prob=p}.
#'
#'   The negative binomial distribution has its roots in a gambling game where
#'   participants would bet on the number of tosses of a coin necessary to achieve
#'   a fixed number of heads.  The negative binomial distribution has been applied
#'   in a wide variety of fields, including accident statistics, birth-and-death
#'   processes, and modeling spatial distributions of biological organisms.
#' }
#' @rawRd
#' \seealso{
#'   \link[stats]{Geometric}, \code{\link{enbinom}}, \link[stats]{NegBinomial}.
#' }
#' @rawRd
#' \examples{
#'   # Generate an observation from a geometric distribution with parameter
#'   # prob=0.2, then estimate the parameter prob.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(250)
#'   dat <- rgeom(1, prob = 0.2)
#'   dat
#'   #[1] 4
#'
#'   egeom(dat)
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Geometric
#'   #
#'   #Estimated Parameter(s):          prob = 0.2
#'   #
#'   #Estimation Method:               mle/mme
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     1
#'
#'   #----------
#'
#'   # Generate 3 observations from a geometric distribution with parameter
#'   # prob=0.2, then estimate the parameter prob with the mvue.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(200)
#'   dat <- rgeom(3, prob = 0.2)
#'   dat
#'   #[1] 0 1 2
#'
#'   egeom(dat, method = "mvue")
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Geometric
#'   #
#'   #Estimated Parameter(s):          prob = 0.4
#'   #
#'   #Estimation Method:               mvue
#'   #
#'   #Data:                            dat
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

egeom <-
function (x, method = "mle/mme") 
{
    if (!is.vector(x, mode = "numeric")) 
        stop("'x' must be a numeric vector")
    data.name <- deparse(substitute(x))
    if ((bad.obs <- sum(!(x.ok <- is.finite(x)))) > 0) {
        is.not.finite.warning(x)
        x <- x[x.ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' removed."))
    }
    n <- length(x)
    if (n < 1) 
        stop("'x' must contain at least one non-missing value.")
    if (!all(x == trunc(x)) || any(x < 0)) 
        stop("All values of 'x' must be non-negative integers.")
    x <- sum(x)
    method <- match.arg(method, c("mle/mme", "mvue"))
    if (method == "mvue" && n < 2) 
        stop(paste("You must have at least two non-missing", 
            "values in 'x' to compute the 'mvue' of 'prob'."))
    dist.params <- switch(method, `mle/mme` = c(prob = n/(n + 
        x)), mvue = c(prob = (n - 1)/(n + x - 1)))
    ret.list <- list(distribution = "Geometric", sample.size = n, 
        parameters = dist.params, n.param.est = 1, method = method, 
        data.name = data.name, bad.obs = bad.obs)
    oldClass(ret.list) <- "estimate"
    ret.list
}

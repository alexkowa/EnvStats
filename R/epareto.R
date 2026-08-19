#' Estimate Parameters of a Pareto Distribution
#' @description
#' Estimate the location and shape parameters of a \link[=Pareto]{Pareto distribution}.
#' @usage
#' epareto(x, method = "mle", plot.pos.con = 0.375)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   numeric vector of observations.
#' }
#'   \item{method}{
#'   character string specifying the method of estimation.  Possible values are
#'   \code{"mle"} (maximum likelihood; the default), and \code{"lse"} (least-squares).
#'   See the DETAILS section for more information on these estimation methods.
#' }
#'   \item{plot.pos.con}{
#'   numeric scalar between 0 and 1 containing the value of the plotting position
#'   constant used to construct the values of the empirical cdf.  The default value is
#'   \code{plot.pos.con=0.375}.  This argument is used only when \code{method="lse"}.
#' }
#' }
#' @rawRd
#' \details{
#'   If \code{x} contains any missing (\code{NA}), undefined (\code{NaN}) or
#'   infinite (\code{Inf}, \code{-Inf}) values, they will be removed prior to
#'   performing the estimation.
#'
#'   Let \eqn{\underline{x} = (x_1, x_2, \ldots, x_n)} be a vector of
#'   \eqn{n} observations from a \link[=Pareto]{Pareto distribution} with
#'   parameters \code{location=}\eqn{\eta} and \code{shape=}\eqn{\theta}.
#'
#'   \emph{Maximum Likelihood Estimatation} (\code{method="mle"}) \cr
#'   The maximum likelihood estimators (mle's) of \eqn{\eta} and \eqn{\theta} are
#'   given by (Evans et al., 1993; p.122; Johnson et al., 1994, p.581):
#'   \deqn{\hat{\eta}_{mle} = x_{(1)} \;\;\;\; (1)}
#'   \deqn{\hat{\theta}_{mle} = n [\sum_{i=1}^n log(\frac{x_i}{\hat{\eta}_{mle}}) ]^{-1} \;\;\;\; (2)}
#'   where \eqn{x_(1)} denotes the first order statistic (i.e., the minimum value).
#'   \cr
#'
#'   \emph{Least-Squares Estimation} (\code{method="lse"}) \cr
#'   The least-squares estimators (lse's) of \eqn{\eta} and \eqn{\theta} are derived as
#'   follows.  Let \eqn{X} denote a \link{Pareto} random variable with parameters
#'   \code{location=}\eqn{\eta} and \code{shape=}\eqn{\theta}.  It can be shown that
#'   \deqn{log[1 - F(x)] = \theta log(\eta) - \theta log(x) \;\;\;\; (3)}
#'   where \eqn{F} denotes the cumulative distribution function of \eqn{X}.  Set
#'   \deqn{y_i = log[1 - \hat{F}(x_i)] \;\;\;\; (4)}
#'   \deqn{z_i = log(x_i) \;\;\;\; (5)}
#'   where \eqn{\hat{F}(x)} denotes the empirical cumulative distribution function
#'   evaluated at \eqn{x}.  The least-squares estimates of \eqn{\eta} and \eqn{\theta}
#'   are obtained by solving the regression equation
#'   \deqn{y_i = \beta_{0} + \beta_{1} z_i \;\;\;\; (6)}
#'   and setting
#'   \deqn{\hat{\theta}_{lse} = -\hat{\beta}_{1} \;\;\;\; (7)}
#'   \deqn{\hat{\eta}_{lse} = exp(\frac{\hat{\beta}_0}{\hat{\theta}_{lse}}) \;\;\;\; (8)}
#'   (Johnson et al., 1994, p.580).
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
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994).
#'   \emph{Continuous Univariate Distributions, Volume 1}.
#'   Second Edition. John Wiley and Sons, New York.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The Pareto distribution is named after Vilfredo Pareto (1848-1923), a professor
#'   of economics.  It is derived from Pareto's law, which states that the number of
#'   persons \eqn{N} having income \eqn{\ge x} is given by:
#'   \deqn{N = A x^{-\theta}}
#'   where \eqn{\theta} denotes Pareto's constant and is the shape parameter for the
#'   probability distribution.
#'
#'   The Pareto distribution takes values on the positive real line.  All values must be
#'   larger than the \dQuote{location} parameter \eqn{\eta}, which is really a threshold
#'   parameter.  There are three kinds of Pareto distributions.  The one described here
#'   is the Pareto distribution of the first kind.  Stable Pareto distributions have
#'   \eqn{0 < \theta < 2}.  Note that the \eqn{r}'th moment only exists if
#'   \eqn{r < \theta}.
#'
#'   The Pareto distribution is related to the
#'   \link[stats:Exponential]{exponential distribution} and
#'   \link[stats:Logistic]{logistic distribution} as follows.
#'   Let \eqn{X} denote a Pareto random variable with \code{location=}\eqn{\eta} and
#'   \code{shape=}\eqn{\theta}.  Then \eqn{log(X/\eta)} has an exponential distribution
#'   with parameter \code{rate=}\eqn{\theta}, and \eqn{-log\{ [(X/\eta)^\theta] - 1 \}}
#'   has a logistic distribution with parameters \code{location=}\eqn{0} and
#'   \code{scale=}\eqn{1}.
#'
#'   The Pareto distribution has a very long right-hand tail.  It is often applied in
#'   the study of socioeconomic data, including the distribution of income, firm size,
#'   population, and stock price fluctuations.
#' }
#' @rawRd
#' \seealso{
#'   \link{Pareto}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 30 observations from a Pareto distribution with parameters
#'   # location=1 and shape=1 then estimate the parameters.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(250)
#'   dat <- rpareto(30, location = 1, shape = 1)
#'   epareto(dat)
#'
#'   #Results of Distribution Parameter Estimation
#'   #--------------------------------------------
#'   #
#'   #Assumed Distribution:            Pareto
#'   #
#'   #Estimated Parameter(s):          location = 1.009046
#'   #                                 shape    = 1.079850
#'   #
#'   #Estimation Method:               mle
#'   #
#'   #Data:                            dat
#'   #
#'   #Sample Size:                     30
#'
#'   #----------
#'
#'   # Compare the results of using the least-squares estimators:
#'
#'   epareto(dat, method="lse")$parameters
#'   #location    shape
#'   #1.085924 1.144180
#'
#'   #----------
#'
#'   # Clean up
#'   #---------
#'
#'   rm(dat)
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ htest }

epareto <-
function (x, method = "mle", plot.pos.con = 0.375) 
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
    if (n < 2 || any(x <= 0) || length(unique(x)) < 2) 
        stop(paste("'x' must contain at least 2 non-missing distinct values,", 
            "and all non-missing values of x must be positive."))
    if (!is.vector(plot.pos.con, mode = "numeric") || length(plot.pos.con) != 
        1 || plot.pos.con < 0 || plot.pos.con > 1) 
        stop("'plot.pos.con' must be a numeric scalar between 0 and 1")
    method <- match.arg(method, c("mle", "lse"))
    switch(method, lse = {
        F.hat <- ppoints(n, a = plot.pos.con)
        coef <- lm(log(1 - F.hat) ~ log(sort(x)))$coefficients
        shape <- -coef[2]
        location <- exp(coef[1]/shape)
        dist.params <- c(location, shape)
    }, mle = {
        location <- min(x)
        shape <- n/sum(log(x/location))
        dist.params <- c(location, shape)
    })
    names(dist.params) <- c("location", "shape")
    ret.list <- list(distribution = "Pareto", sample.size = n, 
        parameters = dist.params, method = method, data.name = data.name, 
        bad.obs = bad.obs)
    oldClass(ret.list) <- "estimate"
    ret.list
}

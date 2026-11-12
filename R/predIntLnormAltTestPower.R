#' Probability That at Least One Future Observation Falls Outside a Prediction Interval for a Lognormal Distribution
#' @description
#' Compute the probability that at least one out of \eqn{k} future observations
#'   (or geometric means) falls outside a prediction interval for \eqn{k} future
#'   observations (or geometric means) for a normal distribution.
#' @usage
#' predIntLnormAltTestPower(n, df = n - 1, n.geomean = 1, k = 1,
#'     ratio.of.means = 1, cv = 1, pi.type = "upper", conf.level = 0.95)
#' @rawRd
#' \arguments{
#'   \item{n}{
#'   vector of positive integers greater than 2 indicating the sample size upon which
#'   the prediction interval is based.
#' }
#'   \item{df}{
#'   vector of positive integers indicating the degrees of freedom associated with
#'   the sample size.  The default value is \code{df=n-1}.
#' }
#'   \item{n.geomean}{
#'   positive integer specifying the sample size associated with the future
#'   geometric means.  The default value is \code{n.geomean=1} (i.e., individual
#'   observations).  Note that all future geometric means must be based on the
#'   same sample size.
#' }
#'   \item{k}{
#'   vector of positive integers specifying the number of future observations that the
#'   prediction interval should contain with confidence level \code{conf.level}.  The
#'   default value is \code{k=1}.
#' }
#'   \item{ratio.of.means}{
#'   numeric vector specifying the ratio of the mean of the population that will be
#'   sampled to produce the future observations vs. the mean of the population that
#'   was sampled to construct the prediction interval.  See the DETAILS section below
#'   for more information.  The default value is \code{ratio.of.means=1}.
#' }
#'   \item{cv}{
#'   numeric vector of positive values specifying the coefficient of variation for
#'   both the population that was sampled to construct the prediction interval \bold{and}
#'   the population that will be sampled to produce the future observations.  The
#'   default value is \code{cv=1}.
#' }
#'   \item{pi.type}{
#'   character string indicating what kind of prediction interval to compute.
#'   The possible values are \code{pi.type="upper"} (the default), and
#'   \code{pi.type="lower"}.
#' }
#'   \item{conf.level}{
#'   numeric vector of values between 0 and 1 indicating the confidence level of the
#'   prediction interval.  The default value is \code{conf.level=0.95}.
#' }
#' }
#' @rawRd
#' \details{
#'   A prediction interval for some population is an interval on the real line
#'   constructed so that it will contain \eqn{k} future observations or averages
#'   from that population with some specified probability \eqn{(1-\alpha)100\%},
#'   where \eqn{0 < \alpha < 1} and \eqn{k} is some pre-specified positive integer.
#'   The quantity \eqn{(1-\alpha)100\%} is call the confidence coefficient or
#'   confidence level associated with the prediction interval.  The function
#'   \code{\link{predIntNorm}} computes a standard prediction interval based on a
#'   sample from a normal distribution.
#'
#'   The function \code{\link{predIntNormTestPower}} computes the probability that at
#'   least one out of \eqn{k} future observations or averages will \bold{not} be contained in
#'   a prediction interval based on the assumption of normally distributed observations,
#'   where the population mean for the future observations is allowed to differ from
#'   the population mean for the observations used to construct the prediction interval.
#'
#'   The function \code{predIntLnormAltTestPower} assumes all observations are
#'   from a \link[=LognormalAlt]{lognormal distribution}.  The observations used to
#'   construct the prediction interval are assumed to come from a lognormal distribution
#'   with mean \eqn{\theta_2} and coefficient of variation \eqn{\tau}.  The future
#'   observations are assumed to come from a lognormal distribution with mean
#'   \eqn{\theta_1} and coefficient of variation \eqn{\tau}; that is, the means are
#'   allowed to differ between the two populations, but not the coefficient of variation.
#'
#'   The function \code{predIntLnormAltTestPower} calls the function
#'   \code{\link{predIntNormTestPower}}, with the argument \code{delta.over.sigma}
#'   given by:
#'   \deqn{\frac{\delta}{\sigma} = \frac{log(R)}{\sqrt{log(\tau^2 + 1)}} \;\;\;\;\;\; (1)}
#'   where \eqn{R} is given by:
#'   \deqn{R = \frac{\theta_1}{\theta_2} \;\;\;\;\;\; (2)}
#'   and corresponds to the argument \code{ratio.of.means} for the function
#'   \code{predIntLnormAltTestPower}, and \eqn{\tau} corresponds to the argument
#'   \code{cv}.
#' }
#' @rawRd
#' \value{
#'   vector of numbers between 0 and 1 equal to the probability that at least one of
#'   \eqn{k} future observations or geometric means will fall outside the prediction
#'   interval.
#' }
#' @rawRd
#' \references{
#'   See the help files for \code{\link{predIntNormTestPower}} and
#'   \code{\link{tTestLnormAltPower}}.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   See the help files for \code{\link{predIntNormTestPower}}.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{plotPredIntLnormAltTestPowerCurve}},
#'   \code{\link{predIntLnormAlt}},
#'   \code{\link{predIntNorm}}, \code{\link{predIntNormK}}, \cr
#'   \code{\link{plotPredIntNormTestPowerCurve}},
#'   \code{\link{predIntLnormAltSimultaneous}}, \cr
#'   \code{\link{predIntLnormAltSimultaneousTestPower}}, \link{Prediction Intervals},
#'   \link{LognormalAlt}.
#' }
#' @rawRd
#' \examples{
#'   # Show how the power increases as ratio.of.means increases.  Assume a
#'   # 95% upper prediction interval.
#'
#'   predIntLnormAltTestPower(n = 4, ratio.of.means = 1:3)
#'   #[1] 0.0500000 0.1459516 0.2367793
#'
#'   #----------
#'
#'   # Look at how the power increases with sample size for an upper one-sided
#'   # prediction interval with k=3, ratio.of.means=4, and a confidence level of 95%.
#'
#'   predIntLnormAltTestPower(n = c(4, 8), k = 3, ratio.of.means = 4)
#'   #[1] 0.2860952 0.4533567
#'
#'   #----------
#'
#'   # Show how the power for an upper 95% prediction limit increases as the
#'   # number of future observations k increases.  Here, we'll use n=20 and
#'   # ratio.of.means=2.
#'
#'   predIntLnormAltTestPower(n = 20, k = 1:3, ratio.of.means = 2)
#'   #[1] 0.1945886 0.2189538 0.2321562
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ htest }

predIntLnormAltTestPower <-
function (n, df = n - 1, n.geomean = 1, k = 1, ratio.of.means = 1, 
    cv = 1, pi.type = "upper", conf.level = 0.95) 
{
    pi.type <- match.arg(pi.type, c("upper", "lower"))
    if (!is.vector(n, mode = "numeric") || !all(is.finite(n)) || 
        any(n < 2)) 
        stop(paste("'n' must be a numeric vector", "with all elements greater than or equal to 2", 
            "and no Missing (NA), Infinite (-Inf, Inf),", "or Undefined (Nan) values."))
    if (!is.vector(df, mode = "numeric") || !all(is.finite(df)) || 
        any(df < 1)) 
        stop(paste("'df' must be a numeric vector", "with all elements greater than or equal to 1", 
            "and no Missing (NA), Infinite (-Inf, Inf),", "or Undefined (Nan) values."))
    if (!is.vector(n.geomean, mode = "numeric") || !all(is.finite(n.geomean)) || 
        any(n.geomean < 1)) 
        stop(paste("'n.geomean' must be a numeric vector", "with all elements greater than or equal to 1", 
            "and no Missing (NA), Infinite (-Inf, Inf),", "or Undefined (Nan) values."))
    if (!is.vector(k, mode = "numeric") || !all(is.finite(k)) || 
        !all(k == trunc(k)) || any(k < 1)) 
        stop(paste("'k' must be a numeric vector of integers", 
            "with all elements greater than 0", "and no Missing (NA), Infinite(-Inf, Inf),", 
            "or Undefined (Nan) values."))
    if (!is.vector(ratio.of.means, mode = "numeric") || !all(is.finite(ratio.of.means)) || 
        any(ratio.of.means <= .Machine$double.eps)) 
        stop(paste("'ratio.of.means' must be a numeric vector", 
            "of positive values", "with no Missing (NA), Infinite(-Inf, Inf),", 
            "or Undefined (Nan) values."))
    if (!is.vector(cv, mode = "numeric") || !all(is.finite(cv)) || 
        any(cv <= .Machine$double.eps)) 
        stop(paste("'cv' must be a numeric vector", "of positive values", 
            "with no Missing (NA), Infinite(-Inf, Inf),", "or Undefined (Nan) values."))
    if (!is.vector(conf.level, mode = "numeric") || !all(is.finite(conf.level)) || 
        any(conf.level <= .Machine$double.eps) || any(conf.level >= 
        1 - .Machine$double.eps)) 
        stop(paste("'conf.level' must be a numeric vector", "with all elements between 0 and 1", 
            "and no Missing (NA), Infinite(-Inf, Inf),", "or Undefined (Nan) values."))
    arg.mat <- cbind.no.warn(n = as.vector(n), df = as.vector(df), 
        n.geomean = as.vector(n.geomean), k = as.vector(k), ratio.of.means = as.vector(ratio.of.means), 
        cv = as.vector(cv), conf.level = as.vector(conf.level))
    for (i in c("n", "df", "n.geomean", "k", "ratio.of.means", 
        "cv", "conf.level")) assign(i, arg.mat[, i])
    delta.over.sigma <- log(ratio.of.means)/sqrt(log(cv^2 + 1))
    predIntNormTestPower(n = n, df = df, n.mean = n.geomean, 
        k = k, delta.over.sigma = delta.over.sigma, pi.type = pi.type, 
        conf.level = conf.level)
}

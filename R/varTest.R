#' One-Sample Chi-Squared Test on Variance
#' @description
#' Estimate the variance, test the null hypothesis using the chi-squared test that the variance is equal
#'   to a user-specified value, and create a confidence interval for the variance.
#' @usage
#' varTest(x, alternative = "two.sided", conf.level = 0.95,
#'     sigma.squared = 1, data.name = NULL)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), and
#'   infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
#' }
#'   \item{alternative}{
#'   character string indicating the kind of alternative hypothesis.  The possible values are
#'   \code{"two.sided"} (the default), \code{"greater"}, and \code{"less"}.
#' }
#'   \item{conf.level}{
#'   numeric scalar between 0 and 1 indicating the confidence level associated with the confidence
#'   interval for the population variance.  The default value is \cr
#'   \code{conf.level=0.95}.
#' }
#'   \item{sigma.squared}{
#'   a numeric scalar indicating the hypothesized value of the variance.  The default value is
#'   \code{sigma.squared=1}.
#' }
#'   \item{data.name}{
#'   character string indicating the name of the data used for the test of variance.
#' }
#' }
#' @rawRd
#' \details{
#'   The function \code{varTest} performs the one-sample chi-squared test of the hypothesis
#'   that the population variance is equal to the user specified value given by the argument
#'   \code{sigma.squared}, and it also returns a confidence interval for the population variance.
#'   The \R function \code{\link{var.test}} performs the F-test for comparing two variances.
#' }
#' @rawRd
#' \value{
#'   A list of class \code{"htest"} containing the results of the hypothesis test.
#'   See the help file for \code{\link{htest.object}} for details.
#' }
#' @rawRd
#' \references{
#'   van Belle, G., L.D. Fisher, Heagerty, P.J., and Lumley, T. (2004).
#'   \emph{Biostatistics: A Methodology for the Health Sciences, 2nd Edition}.
#'   John Wiley & Sons, New York.
#'
#'   Millard, S.P., and N.K. Neerchal. (2001). \emph{Environmental Statistics with S-PLUS}.
#'   CRC Press, Boca Raton, FL.
#'
#'   Zar, J.H. (2010). \emph{Biostatistical Analysis}. Fifth Edition.
#'   Prentice-Hall, Upper Saddle River, NJ.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   Just as you can perform tests of hypothesis on measures of location (mean, median, percentile, etc.),
#'   you can do the same thing for measures of spread or variability.  Usually, we are interested in
#'   estimating variability only because we want to quantify the uncertainty of our estimated location or
#'   percentile.  Sometimes, however, we are interested in estimating variability and quantifying the
#'   uncertainty in our estimate of variability (for example, for performing a sensitivity analysis for
#'   power or sample size calculations), or testing whether the population variability is equal to a
#'   certain value.  There are at least two possible methods of performing a one-sample hypothesis test on
#'   variability:
#'
#'   \itemize{
#'     \item Perform a hypothesis test for the population variance based on the chi-squared statistic,
#'           assuming the underlying population is normal.
#'
#'     \item Perform a hypothesis test for any kind of measure of spread assuming any kind of underlying
#'           distribution based on a bootstrap confidence interval (using, for example, the
#'           package \pkg{boot}).
#'   }
#'
#'   You can use \code{varTest} for the first method.
#'
#'   \bold{Note:}  For a one-sample test of location, Student's t-test is fairly robust to departures
#'   from normality (i.e., the Type I error rate is maintained), as long as the sample size is
#'   reasonably "large."  The chi-squared test on the population variance, however, is extremely sensitive
#'   to departures from normality.  For example, if the underlying population is skewed, the actual
#'   Type I error rate will be larger than assumed.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{var.test}}, \code{\link{varGroupTest}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 20 observations from a normal distribution with parameters
#'   # mean=2 and sd=1.  Test the null hypothesis that the true variance is
#'   # equal to 0.5 against the alternative that the true variance is not
#'   # equal to 0.5.
#'   # (Note: the call to set.seed allows you to reproduce this example).
#'
#'   set.seed(23)
#'   dat <- rnorm(20, mean = 2, sd = 1)
#'   varTest(dat, sigma.squared = 0.5)
#'
#'   #Results of Hypothesis Test
#'   #--------------------------
#'   #
#'   #Null Hypothesis:                 variance = 0.5
#'   #
#'   #Alternative Hypothesis:          True variance is not equal to 0.5
#'   #
#'   #Test Name:                       Chi-Squared Test on Variance
#'   #
#'   #Estimated Parameter(s):          variance = 0.753708
#'   #
#'   #Data:                            dat
#'   #
#'   #Test Statistic:                  Chi-Squared = 28.64090
#'   #
#'   #Test Statistic Parameter:        df = 19
#'   #
#'   #P-value:                         0.1436947
#'   #
#'   #95% Confidence Interval:         LCL = 0.4359037
#'   #                                 UCL = 1.6078623
#'
#'   # Note that in this case we would not reject the
#'   # null hypothesis at the 5% or even the 10% level.
#'
#'   # Clean up
#'   rm(dat)
#' }
#' @rawRd
#' \keyword{htest}
#' @rawRd
#' \keyword{models}

varTest <-
function (x, alternative = "two.sided", conf.level = 0.95, sigma.squared = 1,
    data.name = NULL)
{
    if (is.null(data.name))
        data.name <- deparse(substitute(x))
    alternative <- match.arg(alternative, c("two.sided", "less",
        "greater"))
    if (!missing(conf.level))
        if ((length(conf.level) != 1) || !is.finite(conf.level) ||
            (conf.level <= 0) || (conf.level >= 1))
            stop("argument 'conf.level' must be a single number greater than zero and less than one.")
    alpha <- 1 - conf.level
    if ((bad.obs <- sum(!(x.ok <- is.finite(x)))) > 0) {
        is.not.finite.warning(x)
        x <- x[x.ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' removed."))
    }
    num.df <- length(x) - 1
    if (length(unique(x)) < 2) {
        stop("All values in 'x' are the same.  Variance is zero.")
    }
    if (!missing(sigma.squared))
        if ((length(sigma.squared) != 1) || !is.finite(sigma.squared) ||
            sigma.squared <= 0)
            stop(paste("argument 'sigma.squared' must be a",
                "single positive numeric value."))
    var.x <- var(x)
    X <- (num.df * var.x)/sigma.squared
    p.less <- pchisq(X, df = num.df)
    p.greater <- 1 - pchisq(X, df = num.df)
    p.value <- switch(alternative, two.sided = 2 * min(p.less,
        p.greater), less = p.less, greater = p.greater)
    statistic <- X
    names(statistic) <- "Chi-Squared"
    parameters <- num.df
    names(parameters) <- "df"
    null.value <- sigma.squared
    names(null.value) <- "variance"
    method <- "Chi-Squared Test on Variance"
    estimate <- var.x
    names(estimate) <- "variance"
    ci.type <- switch(alternative, two.sided = "two-sided", less = "upper",
        greater = "lower")
    ci.interval <- enorm(x, ci = TRUE, ci.type = ci.type, conf.level = conf.level,
        ci.param = "var")$interval$limits
    attr(ci.interval, "conf.level") <- conf.level
    ret.val <- c(list(statistic = statistic, parameters = parameters,
        p.value = p.value, estimate = estimate, null.value = null.value,
        alternative = alternative, method = method, data.name = data.name),
        list(conf.int = ci.interval))
    oldClass(ret.val) <- "htestEnvStats"
    return(ret.val)
}

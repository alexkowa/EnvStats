#' Compute Sample Size Necessary to Achieve Specified Power for One-Way Fixed-Effects Analysis of Variance
#' @description
#' Compute the sample sizes necessary to achieve a specified power
#'   for a one-way fixed-effects analysis of variance test, given
#'   the population means, population standard deviation, and
#'   significance level.
#' @usage
#' aovN(mu.vec, sigma = 1, alpha = 0.05, power = 0.95,
#'     round.up = TRUE, n.max = 5000, tol = 1e-07, maxiter = 1000)
#' @rawRd
#' \arguments{
#'   \item{mu.vec}{
#'   required numeric vector of population means. The length of
#'   \code{mu.vec} must be at least 2.  Missing (\code{NA}),
#'   undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are not allowed.
#' }
#'   \item{sigma}{
#'   optional numeric scalar specifying the population standard
#'   deviation (\eqn{\sigma}) for each group.  The default value
#'   is \code{sigma=1}.
#' }
#'   \item{alpha}{
#'   optional numeric scalar between 0 and 1 indicating the Type I
#'   error level associated with the hypothesis test.  The default
#'   value is \code{alpha=0.05}.
#' }
#'   \item{power}{
#'   optional numeric scalar between 0 and 1 indicating the power
#'   associated with the hypothesis test.  The default value
#'   is \code{power=0.95}.
#' }
#'   \item{round.up}{
#'   optional logical scalar indicating whether to round up the value of the
#'   computed sample size to the next smallest integer.  The default
#'   value is \code{round.up=TRUE}.
#' }
#'   \item{n.max}{
#'   positive integer greater then 1 indicating the maximum sample size per group.
#'   The default value is \code{n.max=5000}.
#' }
#'   \item{tol}{
#'   optional numeric scalar indicating the tolerance to use in the
#'   \code{\link{uniroot}} search algorithm.  The default value is
#'   \code{tol=1e-7}.
#' }
#'   \item{maxiter}{
#'   optional positive integer indicating the maximum number of iterations to use in the
#'   \code{\link{uniroot}} search algorithm.  The default value is
#'   \code{maxiter=1000}.
#' }
#' }
#' @rawRd
#' \details{
#'   The F-statistic to test the equality of \eqn{k} population means
#'   assuming each population has a normal distribution with the same
#'   standard deviation \eqn{\sigma} is presented in most basic
#'   statistics texts, including Zar (2010, Chapter 10),
#'   Berthouex and Brown (2002, Chapter 24), and Helsel and Hirsh (1992, pp.164-169).
#'   The formula for the power of this test is given in Scheffe
#'   (1959, pp.38-39,62-65).  The power of the one-way fixed-effects ANOVA depends
#'   on the sample sizes for each of the \eqn{k} groups, the value of the
#'   population means for each of the \eqn{k} groups, the population
#'   standard deviation \eqn{\sigma}, and the significance level
#'   \eqn{\alpha}.  See the help file for \code{\link{aovPower}}.
#'
#'   The function \code{aovN} assumes equal sample
#'   sizes for each of the \eqn{k} groups and uses a search
#'   algorithm to determine the sample size \eqn{n} required to
#'   attain a specified power, given the values of the population
#'   means and the significance level.
#' }
#' @rawRd
#' \value{
#'   numeric scalar indicating the required sample size for each
#'   group.  (The number of groups is equal to the length of the
#'   argument \code{mu.vec}.)
#' }
#' @rawRd
#' \references{
#'   Berthouex, P.M., and L.C. Brown. (2002).
#'   \emph{Statistics for Environmental Engineers}.  Second Edition.
#'   Lewis Publishers, Boca Raton, FL.
#'
#'   Helsel, D.R., and R.M. Hirsch. (1992).
#'   \emph{Statistical Methods in Water Resources Research}.
#'   Elsevier, New York, NY, Chapter 7.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995).
#'   \emph{Continuous Univariate Distributions, Volume 2}.
#'   Second Edition. John Wiley and Sons, New York,
#'   Chapters 27, 29, 30.
#'
#'   Millard, S.P., and Neerchal, N.K. (2001). \emph{Environmental Statistics with S-PLUS}.
#'   CRC Press, Boca Raton, Florida.
#'
#'   Scheffe, H. (1959). \emph{The Analysis of Variance}.
#'   John Wiley and Sons, New York, 477pp.
#'
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. p.6-38.
#'
#'   Zar, J.H. (2010). \emph{Biostatistical Analysis}.
#'   Fifth Edition. Prentice-Hall, Upper Saddle River, NJ,
#'   Chapter 10.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   The normal and lognormal distribution are probably the two most
#'   frequently used distributions to model environmental data.
#'   Sometimes it is necessary to compare several means to determine
#'   whether any are significantly different from each other
#'   (e.g., USEPA, 2009, p.6-38).  In this case, assuming
#'   normally distributed data, you perform a one-way parametric
#'   analysis of variance.
#'
#'   In the course of designing a sampling program, an environmental
#'   scientist may wish to determine the relationship between sample
#'   size, Type I error level, power, and differences in means if
#'   one of the objectives of the sampling program is to determine
#'   whether a particular mean differs from a group of means.  The
#'   functions \code{\link{aovPower}}, \code{aovN}, and
#'   \code{\link{plotAovDesign}} can be used to investigate these
#'   relationships for the case of normally-distributed observations.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{aovPower}}, \code{\link{plotAovDesign}},
#'   \code{\link{Normal}}, \code{\link{aov}}.
#' }
#' @rawRd
#' \examples{
#'   # Look at how the required sample size for a one-way ANOVA
#'   # increases with increasing power:
#'
#'   aovN(mu.vec = c(10, 12, 15), sigma = 5, power = 0.8)
#'   #[1] 21
#'
#'   aovN(mu.vec = c(10, 12, 15), sigma = 5, power = 0.9)
#'   #[1] 27
#'
#'   aovN(mu.vec = c(10, 12, 15), sigma = 5, power = 0.95)
#'   #[1] 33
#'
#'   #----------------------------------------------------------------
#'
#'   # Look at how the required sample size for a one-way ANOVA,
#'   # given a fixed power, decreases with increasing variability
#'   # in the population means:
#'
#'   aovN(mu.vec = c(10, 10, 11), sigma=5)
#'   #[1] 581
#'
#'   aovN(mu.vec = c(10, 10, 15), sigma = 5)
#'   #[1] 25
#'
#'   aovN(mu.vec = c(10, 13, 15), sigma = 5)
#'   #[1] 33
#'
#'   aovN(mu.vec = c(10, 15, 20), sigma = 5)
#'   #[1] 10
#'
#'   #----------------------------------------------------------------
#'
#'   # Look at how the required sample size for a one-way ANOVA,
#'   # given a fixed power, decreases with increasing values of
#'   # Type I error:
#'
#'   aovN(mu.vec = c(10, 12, 14), sigma = 5, alpha = 0.001)
#'   #[1] 89
#'
#'   aovN(mu.vec = c(10, 12, 14), sigma = 5, alpha = 0.01)
#'   #[1] 67
#'
#'   aovN(mu.vec = c(10, 12, 14), sigma = 5, alpha = 0.05)
#'   #[1] 50
#'
#'   aovN(mu.vec = c(10, 12, 14), sigma = 5, alpha = 0.1)
#'   #[1] 42
#' }
#' @rawRd
#' \keyword{ design }
#' @rawRd
#' \keyword{ htest }
#' @rawRd
#' \keyword{ models }
#' @rawRd
#' \keyword{ regression }

aovN <-
function (mu.vec, sigma = 1, alpha = 0.05, power = 0.95, round.up = TRUE, 
    n.max = 5000, tol = 1e-07, maxiter = 1000) 
{
    if (!is.vector(mu.vec, mode = "numeric") || !is.vector(sigma, 
        mode = "numeric") || !is.vector(alpha, mode = "numeric") || 
        !is.vector(power, mode = "numeric")) 
        stop(paste("'mu.vec', 'sigma, 'alpha', and 'power'", 
            "must be numeric vectors."))
    if (!all(is.finite(mu.vec)) || !all(is.finite(sigma)) || 
        !all(is.finite(alpha)) || !all(is.finite(power))) 
        stop(paste("Missing (NA), Infinite (Inf, -Inf), and", 
            "Undefined (Nan) values are not allowed in", "'mu.vec', 'sigma', 'alpha', or 'power'"))
    if ((n.grps <- length(mu.vec)) < 2 || length(unique(mu.vec)) == 
        1) 
        stop("'mu.vec' must have at least 2 distinct values")
    if (any(sigma <= 0)) 
        stop("All values of 'sigma' must be positive.")
    if (any(alpha <= 0) || any(alpha >= 1)) 
        stop("All values of 'alpha' must be greater than 0 and less than 1")
    if (any(power <= alpha) || any(power >= 1)) 
        stop(paste("All values of 'power' must be greater than the", 
            "corresponding elements of 'alpha' and less than 1"))
    if (!is.vector(n.max, mode = "numeric") || length(n.max) != 
        1 || !is.finite(n.max) || n.max != trunc(n.max) || n.max < 
        2) 
        stop("'n.max' must be a positive integer greater than 1")
    if (!is.vector(maxiter, mode = "numeric") || length(maxiter) != 
        1 || !is.finite(maxiter) || maxiter != trunc(maxiter) || 
        maxiter < 2) 
        stop("'maxiter' must be a positive integer greater than 1")
    arg.mat <- cbind.no.warn(sigma = as.vector(sigma), alpha = as.vector(alpha), 
        power = as.vector(power))
    for (i in c("sigma", "alpha", "power")) assign(i, arg.mat[, 
        i])
    N <- length(sigma)
    n.vec <- numeric(N)
    for (i in 1:N) n.vec[i] <- aovN.scalar(mu.vec = mu.vec, sigma = sigma[i], 
        alpha = alpha[i], power = power[i], round.up = round.up, 
        n.max = n.max, tol = tol, maxiter = maxiter)
    names(n.vec) <- NULL
    n.vec
}

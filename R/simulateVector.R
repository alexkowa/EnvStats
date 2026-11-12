#' Simulate a Vector of Random Numbers From a Specified Theoretical or Empirical Probability Distribution
#' @description
#' Simulate a vector of random numbers from a specified theoretical probability
#'   distribution or empirical probability distribution, using either Latin Hypercube
#'   sampling or simple random sampling.
#' @usage
#' simulateVector(n, distribution = "norm", param.list = list(mean = 0, sd = 1),
#'     sample.method = "SRS", seed = NULL, sorted = FALSE,
#'     left.tail.cutoff = ifelse(is.finite(supp.min), 0, .Machine$double.eps),
#'     right.tail.cutoff = ifelse(is.finite(supp.max), 0, .Machine$double.eps))
#' @rawRd
#' \arguments{
#'   \item{n}{
#'   a positive integer indicating the number of random numbers to generate.
#' }
#'   \item{distribution}{
#'   a character string denoting the distribution abbreviation.  The default value is
#'   \code{distribution="norm"}.  See the help file for \code{\link{Distribution.df}}
#'   for a list of possible distribution abbreviations.
#'
#'   Alternatively, the character string \code{"emp"} may be used to denote sampling
#'   from an empirical distribution based on a set of observations.  The vector
#'   containing the observations is specified in the argument \code{param.list}.
#' }
#'   \item{param.list}{
#'   a list with values for the parameters of the distribution.
#'   The default value is \code{param.list=list(mean=0, sd=1)}.
#'   See the help file for \code{\link{Distribution.df}} for the names and
#'   possible values of the parameters associated with each distribution.
#'
#'   Alternatively, if you specify an empirical distribution by setting \cr
#'   \code{distribution="emp"}, then \code{param.list} must be a list of the
#'   form \code{list(obs=}\emph{name}\code{)}, where \emph{name} denotes the
#'   name of the vector containing the observations to use for the empirical
#'   distribution.  In this case, you may also supply arguments to the
#'   \code{\link{qemp}} function through \code{param.list}.  For example, you
#'   may set \cr
#'   \code{param.list=list(obs=}\emph{name}\code{, discrete=T)} to
#'   specify an empirical distribution based on a discrete random variable.
#' }
#'   \item{sample.method}{
#'   a character string indicating whether to use simple random sampling \cr
#'   (\code{sample.method="SRS"}, the default) or
#'   Latin Hypercube sampling \cr
#'   (\code{sample.method="LHS"}).
#' }
#'   \item{seed}{
#'   integer to supply to the \R function \code{\link{set.seed}}.
#'   The default value is \code{seed=NULL}, in which case the random seed is
#'   not set but instead based on the current value of \code{.Random.seed}.
#' }
#'   \item{sorted}{
#'   logical scalar indicating whether to return the random numbers in sorted
#'   (ascending) order.  The default value is \code{sorted=FALSE}.
#' }
#'   \item{left.tail.cutoff}{
#'   a scalar between 0 and 1 indicating what proportion of the left-tail of
#'   the probability distribution to omit for Latin Hypercube sampling.
#'   For densities with a finite support minimum (e.g., \link{Lognormal} or
#'   \link{Empirical}) the default value is \code{left.tail.cutoff=0};
#'   for densities with a support minimum of \eqn{-\infty}, the default value is
#'   \code{left.tail.cutoff=.Machine$double.eps}.
#'   This argument is ignored if \code{sample.method="SRS"}.
#' }
#'   \item{right.tail.cutoff}{
#'   a scalar between 0 and 1 indicating what proportion of the right-tail of
#'   the probability distribution to omit for Latin Hypercube sampling.
#'   For densities with a finite support maximum (e.g., \link{Beta} or
#'   \link{Empirical}) the default value is \code{right.tail.cutoff=0};
#'   for densities with a support maximum of \eqn{\infty}, the default value
#'   is \code{right.tail.cutoff=.Machine$double.eps}.
#'   This argument is ignored if \code{sample.method="SRS"}.
#' }
#' }
#' @rawRd
#' \details{
#'   \bold{Simple Random Sampling} (\code{sample.method="SRS"}) \cr
#'   When \code{sample.method="SRS"}, the function \code{simulateVector} simply
#'   calls the function \code{r}\emph{abb}, where \emph{abb} denotes the
#'   abbreviation of the specified distribution (e.g., \code{\link{rlnorm}},
#'   \code{\link{remp}}, etc.).
#'   \cr
#'
#'   \bold{Latin Hypercube Sampling} (\code{sample.method="LHS"}) \cr
#'   When \code{sample.method="LHS"}, the function \code{simulateVector} generates
#'   \code{n} random numbers using Latin Hypercube sampling.  The distribution is
#'   divided into \code{n} intervals of equal probability \eqn{1/n} and simple random
#'   sampling is performed once within each interval; i.e., Latin Hypercube sampling
#'   is simply stratified sampling without replacement, where the strata are defined
#'   by the 0'th, 100(1/n)'th, 100(2/n)'th, ..., and 100'th percentiles of the
#'   distribution.
#'
#'   \bold{\emph{Latin Hypercube sampling}}, sometimes abbreviated \bold{\emph{LHS}},
#'   is a method of sampling from a probability distribution that ensures all
#'   portions of the probability distribution are represented in the sample.
#'   It was introduced in the published literature by McKay et al. (1979) to overcome
#'   the following problem in Monte Carlo simulation based on simple random sampling
#'   (SRS).  Suppose we want to generate random numbers from a specified distribution.
#'   If we use simple random sampling, there is a low probability of getting very many
#'   observations in an area of low probability of the distribution.  For example, if
#'   we generate \eqn{n} observations from the distribution, the probability that none
#'   of these observations falls into the upper 98'th percentile of the distribution
#'   is \eqn{0.98^n}.  So, for example, there is a 13\% chance that out of 100
#'   random numbers, none will fall at or above the 98'th percentile.  If we are
#'   interested in reproducing the shape of the distribution, we will need a very large
#'   number of observations to ensure that we can adequately characterize the tails of
#'   the distribution (Vose, 2008, pp. 59--62).
#'
#'   See Millard (2013) for a visual explanation of Latin Hypercube sampling.
#' }
#' @rawRd
#' \value{
#'   a numeric vector of random numbers from the specified distribution.
#' }
#' @rawRd
#' \references{
#'   Iman, R.L., and W.J. Conover. (1980).  Small Sample Sensitivity Analysis
#'   Techniques for Computer Models, With an Application to Risk Assessment
#'   (with Comments).  \emph{Communications in Statistics--Volume A, Theory and Methods},
#'   \bold{9}(17), 1749--1874.
#'
#'   Iman, R.L., and J.C. Helton. (1988).  An Investigation of Uncertainty and
#'   Sensitivity Analysis Techniques for Computer Models.  \emph{Risk Analysis}
#'   \bold{8}(1), 71--90.
#'
#'   Iman, R.L. and J.C. Helton. (1991).  The Repeatability of Uncertainty and
#'   Sensitivity Analyses for Complex Probabilistic Risk Assessments.
#'   \emph{Risk Analysis} \bold{11}(4), 591--606.
#'
#'   McKay, M.D., R.J. Beckman., and W.J. Conover. (1979).  A Comparison of Three
#'   Methods for Selecting Values of Input Variables in the Analysis of Output
#'   From a Computer Code.  \emph{Technometrics} \bold{21}(2), 239--245.
#'
#'   Millard, S.P. (2013).  \emph{EnvStats: an R Package for Environmental Statistics}.
#'   Springer, New York.  \url{https://link.springer.com/book/10.1007/978-1-4614-8456-1}.
#'
#'   Vose, D. (2008).  \emph{Risk Analysis:  A Quantitative Guide}.  Third Edition.
#'   John Wiley & Sons, West Sussex, UK, 752 pp.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   \bold{\emph{Latin Hypercube sampling}}, sometimes abbreviated \bold{\emph{LHS}},
#'   is a method of sampling from a probability distribution that ensures all
#'   portions of the probability distribution are represented in the sample.
#'   It was introduced in the published literature by McKay et al. (1979).
#'   Latin Hypercube sampling is often used in probabilistic risk assessment,
#'   specifically for sensitivity and uncertainty analysis
#'   (e.g., Iman and Conover, 1980; Iman and Helton, 1988; Iman and Helton, 1991;
#'   Vose, 1996).
#' }
#' @rawRd
#' \seealso{
#'   \link{Probability Distributions and Random Numbers}, \link{Empirical},
#'   \code{\link{simulateMvMatrix}}, \code{\link{set.seed}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 10 observations from a lognormal distribution with
#'   # parameters mean=10 and cv=1 using simple random sampling:
#'
#'   simulateVector(10, distribution = "lnormAlt",
#'     param.list = list(mean = 10, cv = 1), seed = 47,
#'     sort = TRUE)
#'   # [1]  2.086931  2.863589  3.112866  5.592502  5.732602  7.160707
#'   # [7]  7.741327  8.251306 12.782493 37.214748
#'
#'   #----------
#'
#'   # Repeat the above example by calling rlnormAlt directly:
#'
#'   set.seed(47)
#'   sort(rlnormAlt(10, mean = 10, cv = 1))
#'   # [1]  2.086931  2.863589  3.112866  5.592502  5.732602  7.160707
#'   # [7]  7.741327  8.251306 12.782493 37.214748
#'
#'   #----------
#'
#'   # Now generate 10 observations from the same lognormal distribution
#'   # but use Latin Hypercube sampling.  Note that the largest value
#'   # is larger than for simple random sampling:
#'
#'   simulateVector(10, distribution = "lnormAlt",
#'     param.list = list(mean = 10, cv = 1), seed = 47,
#'     sample.method = "LHS", sort = TRUE)
#'   # [1]  2.406149  2.848428  4.311175  5.510171  6.467852  8.174608
#'   # [7]  9.506874 12.298185 17.022151 53.552699
#'
#'   #==========
#'
#'   # Generate 50 observations from a Pareto distribution with parameters
#'   # location=10 and shape=2, then use this resulting vector of
#'   # observations as the basis for generating 3 observations from an
#'   # empirical distribution using Latin Hypercube sampling:
#'
#'   set.seed(321)
#'   pareto.rns <- rpareto(50, location = 10, shape = 2)
#'
#'   simulateVector(3, distribution = "emp",
#'     param.list = list(obs = pareto.rns), sample.method = "LHS")
#'   #[1] 11.50685 13.50962 17.47335
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(pareto.rns)
#' }
#' @rawRd
#' \keyword{ distribution }
#' @rawRd
#' \keyword{ datagen }

simulateVector <-
function (n, distribution = "norm", param.list = list(mean = 0, 
    sd = 1), sample.method = "SRS", seed = NULL, sorted = FALSE, 
    left.tail.cutoff = ifelse(is.finite(supp.min), 0, .Machine$double.eps), 
    right.tail.cutoff = ifelse(is.finite(supp.max), 0, .Machine$double.eps)) 
{
    if (!is.vector(n, mode = "numeric") || length(n) != 1 || 
        n != trunc(n) || n < 1) 
        stop("'n' must be a positive integer.")
    if (!is.vector(distribution, mode = "character") || length(distribution) != 
        1) 
        stop("'distribution' must be a character string.")
    sample.method <- match.arg(sample.method, c("SRS", "LHS"))
    idist <- charmatch(distribution, c("emp", .Distribution.abb), 
        nomatch = 0)
    if (any(idist == 0)) 
        stop(paste("The argument 'distribution' contains an unknown or", 
            "ambiguous distribution abbreviation. ", "See the help file for 'EnvStats::Distribution.df' for", 
            "a list of distribution abbreviations. ", "You may also use the abbreviation 'emp' to denote", 
            "an empirical distribution"))
    emp <- idist == 1
    if (!is.list(param.list)) 
        stop("'param.list' must be a list.")
    if (!emp) {
        check.da.list <- check.distribution.args.simulate(distribution, 
            param.list, sample.method)
        dist.abb <- check.da.list$dist.abb
        dist.name <- check.da.list$dist.name
        param.list <- check.da.list$param.list
        supp.min <- eval(parse(text = EnvStats::Distribution.df[dist.abb, 
            "Support.Min"]), envir = param.list)
        supp.max <- eval(parse(text = EnvStats::Distribution.df[dist.abb, 
            "Support.Max"]), envir = param.list)
    }
    else {
        dist.abb <- "emp"
        names.param.list <- names(param.list)
        if (length(param.list) < 1 || (sample.method == "SRS" && 
            length(param.list) > 1) || is.null(names.param.list) || 
            any(names.param.list == "") || all(is.na(charmatch(names.param.list, 
            "obs")))) 
            stop(paste("You have specified sampling from ", "an empirical distribution.  ", 
                "For empirical distributions, ", "'param.list' must be a list of the ", 
                "form\n\n\t", "list(obs = ?)\n\n\t", "where ? denotes a numeric vector of empirical ", 
                "observations.  If sample.method=='LHS', this list ", 
                "may also include other arguments to the function ", 
                "qemp(), such as \n\n\t", "list(obs = ?, discrete = TRUE)\n\n\t", 
                "where again ? denotes a numeric vector", sep = ""))
        obs <- param.list$obs
        if ((bad.obs <- sum(!(obs.ok <- is.finite(obs)))) > 0) {
            is.not.finite.warning(obs)
            param.list$obs <- obs[obs.ok]
            warning(paste(bad.obs, "observations with NA/NaN/Inf in 'obs' removed."))
        }
        if (!is.vector(param.list$obs, mode = "numeric") || length(param.list$obs) <= 
            1) 
            stop(paste("You have specified sampling from ", "an empirical distribution.  ", 
                "In this case, the component of 'param.list' ", 
                "named 'obs' must be a numeric vector ", "with at least two non-missing elements.", 
                sep = ""))
        if (sample.method == "LHS" && length(param.list) > 1 && 
            any(is.na(charmatch(names.param.list, c("obs", "discrete", 
                "prob.method", "plot.pos.con"))))) 
            stop(paste("A component of 'param.list' has a name that is", 
                "an unknown argument to 'qemp'."))
        supp.min <- min(param.list$obs)
        supp.max <- max(param.list$obs)
    }
    if (!is.null(seed)) {
        if (!is.vector(seed, mode = "numeric") || length(seed) != 
            1 || seed != trunc(seed) || seed < 0 || seed > 1000) 
            stop("'seed' must be  an integer between 0 and 1000")
        set.seed(seed)
    }
    if (sample.method == "LHS") {
        if (n < 2) 
            stop(paste("The value of 'n' must be greater than 1", 
                "when sample.method='LHS'"))
        if (!is.vector(left.tail.cutoff, mode = "numeric") || 
            length(left.tail.cutoff) != 1 || left.tail.cutoff < 
            0 || !is.vector(right.tail.cutoff, mode = "numeric") || 
            length(right.tail.cutoff) != 1 || right.tail.cutoff < 
            0 || left.tail.cutoff >= 1 - right.tail.cutoff) 
            stop(paste("'left.tail.cutoff' and 'right.tail.cutoff' must be", 
                "numeric scalars between 0 and 1, and 'left.tail.cutoff'", 
                "must be smaller than 1-'right.tail.cutoff'"))
        if (left.tail.cutoff == 0 && supp.min == -Inf) 
            stop(paste("The value of 'left.tail.cutoff' must be greater", 
                "than 0 for the", dist.name, "distribution since the support", 
                "on the left-hand tail is infinite."))
        if (right.tail.cutoff == 0 && supp.max == Inf) 
            stop(paste("The value of 'right.tail.cutoff' must be greater", 
                "than 0 for the", dist.name, "distribution since the support", 
                "on the right-hand tail is infinite."))
    }
    if (sample.method == "SRS") {
        rname <- paste("r", dist.abb, sep = "")
        x <- do.call(rname, c(list(n = n), param.list))
        if (sorted) 
            x <- sort(x)
    }
    else {
        p <- seq(left.tail.cutoff, 1 - right.tail.cutoff, length = n + 
            1)
        p <- runif(n, min = p[1:n], max = p[2:(n + 1)])
        qname <- paste("q", dist.abb, sep = "")
        x <- do.call(qname, c(list(p = p), param.list))
        if (!sorted) 
            x <- sample(x)
    }
    x
}

#' Two-Sample or Paired-Sample Randomization (Permutation) Test for Location
#' @description
#' Perform a two-sample or paired-sample randomization (permutation) test for
#'   location based on either means or medians.
#' @usage
#' twoSamplePermutationTestLocation(x, y, fcn = "mean", alternative = "two.sided",
#'     mu1.minus.mu2 = 0, paired = FALSE, exact = FALSE, n.permutations = 5000,
#'     seed = NULL, tol = sqrt(.Machine$double.eps))
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   numeric vector of observations from population 1.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are allowed but will be removed.
#' }
#'   \item{y}{
#'   numeric vector of observations from population 2.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are allowed but will be removed.
#'
#'   In the case when \code{paired=TRUE}, the argument \code{y} must have the same number
#'   of elements as the argument \code{x}.
#' }
#'   \item{fcn}{
#'   character string indicating which location parameter to compare between the two
#'   groups.  The possible values are \code{fcn="mean"} (the default) and
#'   \code{fcn="median"}.  This argument is ignored when \code{paired=TRUE}.
#' }
#'   \item{alternative}{
#'   character string indicating the kind of alternative hypothesis.  The possible values
#'   are \code{"two.sided"} (the default), \code{"less"}, and \code{"greater"}.
#' }
#'   \item{mu1.minus.mu2}{
#'   numeric scalar indicating the hypothesized value of the difference between the
#'   means or medians.  The default value is \code{mu1.minus.mu2=0}.
#' }
#'   \item{paired}{
#'   logical scalar indicating whether to perform a paired or two-sample permutation
#'   test.  The possible values are \code{paired=FALSE} (the default; indicates a
#'   two-sample permutation test) and \code{paired=TRUE} (indicates take differences of
#'   pairs and perform a one-sample permutation test).
#' }
#'   \item{exact}{
#'   logical scalar indicating whether to perform the exact permutation test (i.e.,
#'   enumerate all possible permutations) or simply sample from the permutation
#'   distribution.  The default value is \code{exact=FALSE}.
#' }
#'   \item{n.permutations}{
#'   integer indicating how many times to sample from the permutation distribution when
#'   \code{exact=FALSE}.  The default value is \code{n.permutations=5000}.
#'   This argument is ignored when \code{exact=TRUE}.
#' }
#'   \item{seed}{
#'   positive integer to pass to the \R function \code{\link{set.seed}}.  The
#'   default is \code{seed=NULL}, in which case the current value of
#'   \code{\link{.Random.seed}} is used.
#'   Using the \code{seed} argument lets you reproduce the exact same result if all
#'   other arguments stay the same.
#' }
#'   \item{tol}{
#'   numeric scalar indicating the tolerance to use for computing the p-value for the
#'   two-sample permutation test.  The default value is \cr
#'   \code{tol=sqrt(.Machine$double.eps)}.  See the DETAILS section below for more
#'   information.
#' }
#' }
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{twoSamplePermutationTestLocation}.
#' @rawRd
#' \value{
#'   A list of class \code{"permutationTest"} containing the results of the hypothesis
#'   test.  See the help file for \code{\link{permutationTest.object}} for details.
#' }
#' @rawRd
#' \references{
#'   Efron, B., and R.J. Tibshirani. (1993).  \emph{An Introduction to the Bootstrap}.
#'   Chapman and Hall, New York, Chapter 15.
#'
#'   Manly, B.F.J. (2007).  \emph{Randomization, Bootstrap and Monte Carlo Methods in
#'   Biology}.  Third Edition. Chapman & Hall, New York, Chapter 6.
#'
#'   Millard, S.P., and N.K. Neerchal. (2001).
#'   \emph{Environmental Statistics with S-PLUS}.  CRC Press, Boca Raton, FL,
#'   pp.426--431.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   A frequent question in environmental statistics is \dQuote{Is the concentration of
#'   chemical X in Area A greater than the concentration of chemical X in Area B?}.
#'   For example, in groundwater detection monitoring at hazardous and solid waste sites,
#'   the concentration of a chemical in the groundwater at a downgradient well must be
#'   compared to \dQuote{background}.  If the concentration is \dQuote{above} the
#'   background then the site enters assessment monitoring.  As another example, soil
#'   cleanup at a Superfund site may involve comparing the concentration of a chemical
#'   in the soil at a \dQuote{cleaned up} site with the concentration at a
#'   \dQuote{background} site.  If the concentration at the \dQuote{cleaned up} site is
#'   \dQuote{greater} than the background concentration, then further investigation and
#'   remedial action may be required.  Determining what it means for the chemical
#'   concentration to be \dQuote{greater} than background is a policy decision: you may
#'   want to compare averages, medians, 95'th percentiles, etc.
#'
#'   Hypothesis tests you can use to compare \dQuote{location} between two groups include:
#'   \link[stats:t.test]{Student's t-test}, Fisher's randomization test
#'   (described in this help file), the \link[stats:wilcox.test]{Wilcoxon rank sum test},
#'   other \link[=twoSampleLinearRankTest]{two-sample linear rank tests},
#'   the \link[=quantileTest]{quantile test}, and a test based on a bootstrap confidence
#'   interval.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{permutationTest.object}}, \code{\link{plot.permutationTest}},
#'   \code{\link{oneSamplePermutationTest}}, \cr
#'   \code{\link{twoSamplePermutationTestProportion}},
#'   \link{Hypothesis Tests}, \code{\link[boot]{boot}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 10 observations from a lognormal distribution with parameters
#'   # mean=5 and cv=2, and and 20 observations from a lognormal distribution with
#'   # parameters mean=10 and cv=2.  Test the null hypothesis that the means of the
#'   # two distributions are the same against the alternative that the mean for
#'   # group 1 is less than the mean for group 2.
#'   # (Note: the call to set.seed allows you to reproduce the same data
#'   # (dat1 and dat2), and setting the argument seed=732 in the call to
#'   # twoSamplePermutationTestLocation() lets you reproduce this example by
#'   # getting the same sample from the permutation distribution).
#'
#'   set.seed(256)
#'   dat1 <- rlnormAlt(10, mean = 5, cv = 2)
#'   dat2 <- rlnormAlt(20, mean = 10, cv = 2)
#'
#'   test.list <- twoSamplePermutationTestLocation(dat1, dat2,
#'     alternative = "less", seed = 732)
#'
#'   # Print the results of the test
#'   #------------------------------
#'   test.list
#'
#'   #Results of Hypothesis Test
#'   #--------------------------
#'   #
#'   #Null Hypothesis:                 mu.x-mu.y = 0
#'   #
#'   #Alternative Hypothesis:          True mu.x-mu.y is less than 0
#'   #
#'   #Test Name:                       Two-Sample Permutation Test
#'   #                                 Based on Differences in Means
#'   #                                 (Based on Sampling
#'   #                                 Permutation Distribution
#'   #                                 5000 Times)
#'   #
#'   #Estimated Parameter(s):          mean of x =  2.253439
#'   #                                 mean of y = 11.825430
#'   #
#'   #Data:                            x = dat1
#'   #                                 y = dat2
#'   #
#'   #Sample Sizes:                    nx = 10
#'   #                                 ny = 20
#'   #
#'   #Test Statistic:                  mean.x - mean.y = -9.571991
#'   #
#'   #P-value:                         0.001
#'
#'
#'   # Plot the results of the test
#'   #-----------------------------
#'   dev.new()
#'   plot(test.list)
#'
#'   #==========
#'
#'   # The guidance document "Statistical Methods for Evaluating the Attainment of
#'   # Cleanup Standards, Volume 3: Reference-Based Standards for Soils and Solid
#'   # Media" (USEPA, 1994b, pp. 6.22-6.25) contains observations of
#'   # 1,2,3,4-Tetrachlorobenzene (TcCB) in ppb at a Reference Area and a Cleanup Area.
#'   # These data are stored in the data frame EPA.94b.tccb.df.  Use the
#'   # two-sample permutation test to test for a difference in means between the
#'   # two areas vs. the alternative that the mean in the Cleanup Area is greater.
#'   # Do the same thing for the medians.
#'   #
#'   # The permutation test based on comparing means shows a significant differnce,
#'   # while the one based on comparing medians does not.
#'
#'
#'   # First test for a difference in the means.
#'   #------------------------------------------
#'
#'   mean.list <- with(EPA.94b.tccb.df,
#'     twoSamplePermutationTestLocation(
#'       TcCB[Area=="Cleanup"], TcCB[Area=="Reference"],
#'       alternative = "greater", seed = 47))
#'
#'   mean.list
#'
#'   #Results of Hypothesis Test
#'   #--------------------------
#'   #
#'   #Null Hypothesis:                 mu.x-mu.y = 0
#'   #
#'   #Alternative Hypothesis:          True mu.x-mu.y is greater than 0
#'   #
#'   #Test Name:                       Two-Sample Permutation Test
#'   #                                 Based on Differences in Means
#'   #                                 (Based on Sampling
#'   #                                 Permutation Distribution
#'   #                                 5000 Times)
#'   #
#'   #Estimated Parameter(s):          mean of x = 3.9151948
#'   #                                 mean of y = 0.5985106
#'   #
#'   #Data:                            x = TcCB[Area == "Cleanup"]
#'   #                                 y = TcCB[Area == "Reference"]
#'   #
#'   #Sample Sizes:                    nx = 77
#'   #                                 ny = 47
#'   #
#'   #Test Statistic:                  mean.x - mean.y = 3.316684
#'   #
#'   #P-value:                         0.0206
#'
#'   dev.new()
#'   plot(mean.list)
#'
#'
#'   #----------
#'
#'   # Now test for a difference in the medians.
#'   #------------------------------------------
#'
#'   median.list <- with(EPA.94b.tccb.df,
#'     twoSamplePermutationTestLocation(
#'       TcCB[Area=="Cleanup"], TcCB[Area=="Reference"],
#'       fcn = "median", alternative = "greater", seed = 47))
#'
#'   median.list
#'
#'   #Results of Hypothesis Test
#'   #--------------------------
#'   #
#'   #Null Hypothesis:                 mu.x-mu.y = 0
#'   #
#'   #Alternative Hypothesis:          True mu.x-mu.y is greater than 0
#'   #
#'   #Test Name:                       Two-Sample Permutation Test
#'   #                                 Based on Differences in Medians
#'   #                                 (Based on Sampling
#'   #                                 Permutation Distribution
#'   #                                 5000 Times)
#'   #
#'   #Estimated Parameter(s):          median of x = 0.43
#'   #                                 median of y = 0.54
#'   #
#'   #Data:                            x = TcCB[Area == "Cleanup"]
#'   #                                 y = TcCB[Area == "Reference"]
#'   #
#'   #Sample Sizes:                    nx = 77
#'   #                                 ny = 47
#'   #
#'   #Test Statistic:                  median.x - median.y = -0.11
#'   #
#'   #P-value:                         0.936
#'
#'   dev.new()
#'   plot(median.list)
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(test.list, mean.list, median.list)
#'   graphics.off()
#' }
#' @rawRd
#' \keyword{htest}
#' @rawRd
#' \keyword{models}

twoSamplePermutationTestLocation <-
function (x, y, fcn = "mean", alternative = "two.sided", mu1.minus.mu2 = 0, 
    paired = FALSE, exact = FALSE, n.permutations = 5000, seed = NULL, 
    tol = sqrt(.Machine$double.eps)) 
{
    alternative <- match.arg(alternative, c("two.sided", "less", 
        "greater"))
    if (!is.vector(mu1.minus.mu2, mode = "numeric") || is.factor(mu1.minus.mu2) || 
        length(mu1.minus.mu2) != 1 || !is.finite(mu1.minus.mu2)) 
        stop("'mu1.minus.mu2' must be a single finite numeric value")
    if (!exact) {
        if (!is.vector(n.permutations, mode = "numeric") || is.factor(n.permutations) || 
            length(n.permutations) != 1 || n.permutations != 
            trunc(n.permutations) || n.permutations < 1) 
            stop("'n.permutations' must be a positive integer")
        if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 
            1 || seed != trunc(seed) || seed < 0 || seed > 1000)) 
            stop("'seed' must be an integer between 0 and 1000")
    }
    if (!is.numeric(x)) 
        stop("'x' must be a numeric vector")
    x.name <- deparse(substitute(x))
    if (!is.numeric(y)) 
        stop("'y' must be a numeric vector")
    y.name <- deparse(substitute(y))
    if (paired) {
        if (length(y) != length(x)) 
            stop("'x' and 'y' must be the same length when paired=T")
        if ((bad.obs <- sum(!(ok <- is.finite(x) & is.finite(y)))) > 
            0) {
            is.not.finite.warning(x)
            is.not.finite.warning(y)
            x <- x[ok]
            y <- y[ok]
            warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' and 'y' removed."))
        }
    }
    else {
        if ((bad.obs <- sum(!(x.ok <- is.finite(x)))) > 0) {
            is.not.finite.warning(x)
            x <- x[x.ok]
            warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' removed."))
        }
        if ((bad.obs <- sum(!(y.ok <- is.finite(y)))) > 0) {
            is.not.finite.warning(y)
            y <- y[y.ok]
            warning(paste(bad.obs, "observations with NA/NaN/Inf in 'y' removed."))
        }
    }
    nx <- length(x)
    ny <- length(y)
    if (nx < 2 || ny < 2) 
        stop("'x' and 'y' must contain at least two non-missing observations")
    data.name <- c(x.name, y.name)
    names(data.name) <- c("x", "y")
    if (paired) {
        ret.list <- oneSamplePermutationTest(x = x - y, alternative = alternative, 
            mu = mu1.minus.mu2, exact = exact, n.permutations = n.permutations, 
            seed = seed)
        names(ret.list$estimate) <- "Mean (Median) of Differences"
        method <- "Paired-Sample Permutation Test"
        if (exact) {
            ret.list$method <- paste(method, "\n", space(33), 
                "(Exact)", sep = "")
        }
        else {
            ret.list$method <- paste(method, "\n", space(33), 
                "(Based on Sampling", "\n", space(33), "Permutation Distribution", 
                "\n", space(33), n.permutations, " Times)", sep = "")
        }
        if (ret.list$alternative == "two.sided") {
            names(ret.list$statistic) <- ifelse(mu1.minus.mu2 == 
                0, "|Sum(x-y)|", paste("|Sum(x-y) - ", mu1.minus.mu2, 
                "|", sep = ""))
        }
        else {
            names(ret.list$statistic) <- ifelse(mu1.minus.mu2 == 
                0, "Sum(x-y)", paste("Sum(x-y) -", mu1.minus.mu2))
        }
        names(ret.list$null.value) <- "Mean (Median) of Differences"
        ret.list$data.name <- data.name
    }
    else {
        fcn <- match.arg(fcn, c("mean", "median"))
        fcn.name <- fcn
        if (fcn.name == "mean") 
            fcn <- get("mean", pos = "package:base")
        else fcn <- get("median", pos = "package:stats")
        string <- ifelse(fcn.name == "mean", "Means", "Medians")
        method <- paste("Two-Sample Permutation Test\n", space(33), 
            "Based on Differences in ", string, sep = "")
        estimate <- c(fcn(x), fcn(y))
        names(estimate) <- c(paste(fcn.name, "of x"), paste(fcn.name, 
            "of y"))
        x <- x - mu1.minus.mu2
        xy <- c(x, y)
        n <- nx + ny
        if (exact) {
            if (mu1.minus.mu2 == 0 && fcn.name == "mean") {
                if (n < 20) {
                  comb.mat <- t(combn(n, nx))
                  x.comb.mat <- matrix(xy[comb.mat], nrow = choose(n, 
                    nx))
                  sum.x <- apply(x.comb.mat, 1, sum)
                  stat.dist <- sum.x * (1/nx + 1/ny) - sum(xy)/ny
                }
                else {
                  stop(paste("When fcn=\"mean\" and mu1.minus.mu2=0,", 
                    "exact method available only for", "combined sample size size less than 20"))
                }
            }
            else {
                if (n < 10) {
                  perm.mat <- permutations(n)
                  xy.perm.mat <- matrix(xy[perm.mat], nrow = factorial(n))
                  x.fcn <- apply(xy.perm.mat[, 1:nx], 1, fcn)
                  y.fcn <- apply(xy.perm.mat[, -(1:nx)], 1, fcn)
                  stat.dist <- x.fcn - y.fcn
                }
                else {
                  stop(paste("When fcn=\"median\", or when", 
                    "fcn=\"mean\" and mu1.minus.mu2!=0,", "exact method available only for", 
                    "combined sample size size less than 10"))
                }
            }
            method <- paste(method, "\n", space(33), "(Exact)", 
                sep = "")
        }
        else {
            set.seed(seed)
            if ((n * n.permutations) <= 15 * (2^15)) {
                perm.mat <- t(sapply(1:n.permutations, function(x, 
                  n) sample(n), n = n))
                xy.perm.mat <- matrix(xy[perm.mat], nrow = n.permutations)
                x.fcn <- apply(xy.perm.mat[, 1:nx], 1, fcn)
                y.fcn <- apply(xy.perm.mat[, -(1:nx)], 1, fcn)
                stat.dist <- x.fcn - y.fcn
            }
            else {
                stat.dist <- numeric(n.permutations)
                perm.vec <- 1:n
                for (i in 1:n.permutations) {
                  perm.vec[1:n] <- sample(n)
                  stat.dist[i] <- fcn(xy[perm.vec][1:nx]) - fcn(xy[perm.vec][-(1:nx)])
                }
            }
            method <- paste(method, "\n", space(33), "(Based on Sampling", 
                "\n", space(33), "Permutation Distribution", 
                "\n", space(33), n.permutations, " Times)", sep = "")
        }
        stat <- fcn(x) - fcn(y)
        if (alternative == "two.sided") {
            stat.dist <- abs(stat.dist)
            stat <- abs(stat)
            if (mu1.minus.mu2 == 0) 
                names(stat) <- paste("|", fcn.name, ".x - ", 
                  fcn.name, ".y|", sep = "")
            else names(stat) <- paste("|", fcn.name, ".x - ", 
                fcn.name, ".y - ", mu1.minus.mu2, "|", sep = "")
            p.value <- mean(stat.dist >= stat - tol)
        }
        else {
            names(stat) <- paste(fcn.name, ".x - ", fcn.name, 
                ".y", sep = "")
            if (mu1.minus.mu2 != 0) 
                names(stat) <- paste(names(stat), " - ", mu1.minus.mu2, 
                  sep = "")
            if (alternative == "less") 
                p.value <- mean(stat.dist <= stat + tol)
            else p.value <- mean(stat.dist >= stat - tol)
        }
        parameters <- NULL
        null.value <- mu1.minus.mu2
        names(null.value) <- "mu.x-mu.y"
        sample.size <- c(nx = nx, ny = ny)
        ret.list <- list(statistic = stat, parameters = parameters, 
            p.value = p.value, estimate = estimate, null.value = null.value, 
            alternative = alternative, method = method, estimation.method = NULL, 
            sample.size = sample.size, data.name = data.name, 
            bad.obs = bad.obs, stat.dist = stat.dist, exact = exact)
        if (!exact) 
            ret.list <- c(ret.list, list(seed = seed))
        oldClass(ret.list) <- "permutationTest"
    }
    ret.list
}

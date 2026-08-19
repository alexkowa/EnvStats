#' Two-Sample Linear Rank Test to Detect a Difference Between Two Distributions
#' @description
#' Two-sample linear rank test to detect a difference (usually a shift) between two
#'   distributions.  The \link[=wilcox.test]{Wilcoxon Rank Sum test} is a special case of
#'   a linear rank test.  The function \cr
#'   \code{twoSampleLinearRankTest} is part of
#'   \pkg{EnvStats} mainly because this help file gives the necessary background to
#'   explain two-sample linear rank tests for censored data (see \cr
#'   \code{\link{twoSampleLinearRankTestCensored}}).
#' @usage
#' twoSampleLinearRankTest(x, y, location.shift.null = 0, scale.shift.null = 1,
#'     alternative = "two.sided", test = "wilcoxon", shift.type = "location")
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   numeric vector of values for the first sample.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf},
#'   \code{-Inf}) values are allowed but will be removed.
#' }
#'   \item{y}{
#'   numeric vector of values for the second sample.
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf},
#'   \code{-Inf}) values are allowed but will be removed.
#' }
#'   \item{location.shift.null}{
#'   numeric scalar indicating the hypothesized value of \eqn{\Delta}, the location
#'   shift between the two distributions, under the null hypothesis.  The default value is
#'   \code{location.shift.null=0}.  This argument is ignored if \code{shift.type="scale"}.
#' }
#'   \item{scale.shift.null}{
#'   numeric scalar indicating the hypothesized value of \eqn{\tau}, the scale shift
#'   between the two distributions, under the null hypothesis.  The default value is \cr
#'   \code{scale.shift.null=1}.  This argument is ignored if \code{shift.type="location"}.
#' }
#'   \item{alternative}{
#'   character string indicating the kind of alternative hypothesis.  The possible values
#'   are \code{"two.sided"} (the default), \code{"less"}, and \code{"greater"}.  See the
#'   DETAILS section below for more information.
#' }
#'   \item{test}{
#'   character string indicating which linear rank test to use.  The possible values are:
#'   \code{"wilcoxon"} (the default), \code{"normal.scores"}, \code{"moods.median"}, and
#'   \code{"savage.scores"}.
#' }
#'   \item{shift.type}{
#'   character string indicating which kind of shift is being tested.  The possible values
#'   are \code{"location"} (the default) and \code{"scale"}.
#' }
#' }
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{twoSampleLinearRankTest}.
#' @rawRd
#' \value{
#'   a list of class \code{"htestEnvStats"} containing the results of the hypothesis test.
#'   See the help file for \code{\link{htestEnvStats.object}} for details.
#' }
#' @rawRd
#' \references{
#'   Conover, W.J. (1980).  \emph{Practical Nonparametric Statistics}.  Second Edition.
#'   John Wiley and Sons, New York, Chapter 4.
#'
#'   Divine, G., H.J. Norton, R. Hunt, and J. Dinemann. (2013).  A Review of Analysis
#'   and Sample Size Calculation Considerations for Wilcoxon Tests.  \emph{Anesthesia
#'   & Analgesia} \bold{117}, 699--710.
#'
#'   Hettmansperger, T.P. (1984).  \emph{Statistical Inference Based on Ranks}.
#'   John Wiley and Sons, New York, 323pp.
#'
#'   Hollander, M., and D.A. Wolfe. (1999). \emph{Nonparametric Statistical Methods,
#'   Second Edition}.  John Wiley and Sons, New York.
#'
#'   Millard, S.P., and S.J. Deverel. (1988).  Nonparametric Statistical Methods for
#'   Comparing Two Sites Based on Data With Multiple Nondetect Limits.
#'   \emph{Water Resources Research}, \bold{24}(12), 2087--2098.
#'
#'   Millard, S.P., and N.K. Neerchal. (2001).  \emph{Environmental Statistics with
#'   S-PLUS}.  CRC Press, Boca Raton, FL, pp.432--435.
#'
#'   Prentice, R.L. (1985).  Linear Rank Tests.  In Kotz, S., and N.L. Johnson, eds.
#'   \emph{Encyclopedia of Statistical Science}.  John Wiley and Sons, New York.
#'   Volume 5, pp.51--58.
#'
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
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
#'   The \link[=wilcox.test]{Wilcoxon Rank Sum test}, also known as the Mann-Whitney U
#'   test, is the standard nonparametric test used to test for differences between two
#'   groups (e.g., Zar, 2010; USEPA, 2009, pp.16-14 to 16-20).  Other possible
#'   nonparametric tests include linear rank tests based on scores other than the ranks,
#'   including the \dQuote{normal scores} test and the \dQuote{Savage scores} tests.
#'   The normal scores test is actually slightly more powerful than the Wilcoxon Rank Sum
#'   test for detecting small shifts in location if the underlying distribution is normal
#'   or lognormal.  In general, however, there will be little difference between these
#'   two tests.
#'
#'   The results of calling the function \code{twoSampleLinearRankTest} with the
#'   argument \code{test="wilcoxon"} will match those of calling the built-in
#'   \R function \code{\link{wilcox.test}} with the arguments \code{exact=FALSE} and
#'   \code{correct=FALSE}.  In general, it is better to use the built-in function
#'   \code{\link{wilcox.test}} for performing the Wilcoxon Rank Sum test, since this
#'   function can compute exact (rather than approximate) p-values.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{wilcox.test}}, \code{\link{twoSampleLinearRankTestCensored}},
#'   \code{\link{htestEnvStats.object}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 15 observations from a normal distribution with parameters
#'   # mean=3 and sd=1.  Call these the observations from the reference group.
#'   # Generate 10 observations from a normal distribution with parameters
#'   # mean=3.5 and sd=1.  Call these the observations from the treatment group.
#'   # Compare the results of calling wilcox.test to those of calling
#'   # twoSampleLinearRankTest with test="normal.scores".
#'   # (The call to set.seed allows you to reproduce this example.)
#'
#'   set.seed(346)
#'   x <- rnorm(15, mean = 3)
#'   y <- rnorm(10, mean = 3.5)
#'
#'   wilcox.test(x, y)
#'
#'   #Results of Hypothesis Test
#'   #--------------------------
#'   #
#'   #Null Hypothesis:                 location shift = 0
#'   #
#'   #Alternative Hypothesis:          True location shift is not equal to 0
#'   #
#'   #Test Name:                       Wilcoxon rank sum test
#'   #
#'   #Data:                            x and y
#'   #
#'   #Test Statistic:                  W = 32
#'   #
#'   #P-value:                         0.0162759
#'
#'
#'   twoSampleLinearRankTest(x, y, test = "normal.scores")
#'
#'   #Results of Hypothesis Test
#'   #--------------------------
#'   #
#'   #Null Hypothesis:                 Fy(t) = Fx(t)
#'   #
#'   #Alternative Hypothesis:          Fy(t) != Fx(t) for at least one t
#'   #
#'   #Test Name:                       Two-Sample Linear Rank Test:
#'   #                                 Normal Scores Test
#'   #                                 Based on Normal Approximation
#'   #
#'   #Data:                            x = x
#'   #                                 y = y
#'   #
#'   #Sample Sizes:                    nx = 15
#'   #                                 ny = 10
#'   #
#'   #Test Statistic:                  z = -2.431099
#'   #
#'   #P-value:                         0.01505308
#'
#'   #----------
#'
#'   # Clean up
#'   #---------
#'   rm(x, y)
#'
#'   #==========
#'
#'   # Following Example 6.6 on pages 6.22-6.26 of USEPA (1994b), perform the
#'   # Wilcoxon Rank Sum test for the TcCB data (stored in EPA.94b.tccb.df).
#'   # There are m=47 observations from the reference area and n=77 observations
#'   # from the cleanup unit.  Then compare the results using the other available
#'   # linear rank tests.  Note that Mood's median test yields a p-value less
#'   # than 0.10, while the other tests yield non-significant p-values.
#'   # In this case, Mood's median test is picking up the residual contamination
#'   # in the cleanup unit. (See the example in the help file for quantileTest.)
#'
#'   names(EPA.94b.tccb.df)
#'   #[1] "TcCB.orig" "TcCB"      "Censored"  "Area"
#'
#'   summary(EPA.94b.tccb.df$Area)
#'   #  Cleanup Reference
#'   #       77        47
#'
#'   with(EPA.94b.tccb.df,
#'     twoSampleLinearRankTest(TcCB[Area=="Cleanup"], TcCB[Area=="Reference"]))
#'
#'   #Results of Hypothesis Test
#'   #--------------------------
#'   #
#'   #Null Hypothesis:                 Fy(t) = Fx(t)
#'   #
#'   #Alternative Hypothesis:          Fy(t) != Fx(t) for at least one t
#'   #
#'   #Test Name:                       Two-Sample Linear Rank Test:
#'   #                                 Wilcoxon Rank Sum Test
#'   #                                 Based on Normal Approximation
#'   #
#'   #Data:                            x = TcCB[Area == "Cleanup"]
#'   #                                 y = TcCB[Area == "Reference"]
#'   #
#'   #Sample Sizes:                    nx = 77
#'   #                                 ny = 47
#'   #
#'   #Test Statistic:                  z = -1.171872
#'   #
#'   #P-value:                         0.2412485
#'
#'   with(EPA.94b.tccb.df,
#'     twoSampleLinearRankTest(TcCB[Area=="Cleanup"],
#'       TcCB[Area=="Reference"], test="normal.scores"))$p.value
#'   #[1] 0.3399484
#'
#'   with(EPA.94b.tccb.df,
#'     twoSampleLinearRankTest(TcCB[Area=="Cleanup"],
#'       TcCB[Area=="Reference"], test="moods.median"))$p.value
#'   #[1] 0.09707393
#'
#'   with(EPA.94b.tccb.df,
#'     twoSampleLinearRankTest(TcCB[Area=="Cleanup"],
#'       TcCB[Area=="Reference"], test="savage.scores"))$p.value
#'   #[1] 0.2884351
#' }
#' @rawRd
#' \keyword{htestEnvStats}
#' @rawRd
#' \keyword{nonparametric}
#' @rawRd
#' \keyword{regression}

twoSampleLinearRankTest <-
function (x, y, location.shift.null = 0, scale.shift.null = 1,
    alternative = "two.sided", test = "wilcoxon", shift.type = "location")
{
    alternative <- match.arg(alternative, c("two.sided", "less",
        "greater"))
    test <- match.arg(test, c("wilcoxon", "normal.scores", "moods.median",
        "savage.scores"))
    shift.type <- match.arg(shift.type, c("location", "scale"))
    data.name <- c(deparse(substitute(x)), deparse(substitute(y)))
    names(data.name) <- c("x", "y")
    if (!is.vector(x, mode = "numeric") || is.factor(x))
        stop("'x' must be a numeric vector")
    if ((bad.obs.x <- sum(!(ok <- is.finite(x)))) > 0) {
        is.not.finite.warning(x)
        x <- x[ok]
        warning(paste(bad.obs.x, "observations with NA/NaN/Inf in 'x' removed."))
    }
    if (length(unique(x)) < 2)
        stop("'x' must contain at least two distinct non-missing observations.")
    if (!is.vector(y, mode = "numeric") || is.factor(y))
        stop("'y' must be a numeric vector")
    if ((bad.obs.y <- sum(!(ok <- is.finite(y)))) > 0) {
        is.not.finite.warning(y)
        y <- y[ok]
        warning(paste(bad.obs.y, "observations with NA/NaN/Inf in 'y' removed."))
    }
    if (length(unique(y)) < 2)
        stop("'y' must contain at least two distinct non-missing observations.")
    if (shift.type == "location" && !missing(location.shift.null)) {
        if ((length(location.shift.null) != 1) || !is.finite(location.shift.null))
            stop("'location.shift.null' must be a single finite numeric value")
        x <- x - location.shift.null
    }
    if (shift.type == "scale" && !missing(scale.shift.null)) {
        if ((length(scale.shift.null) != 1) || !is.finite(scale.shift.null) ||
            scale.shift.null <= 0)
            stop("'scale.shift.null' must be a single finite positive numeric value")
        x <- x/scale.shift.null
    }
    m <- length(x)
    n <- length(y)
    N <- m + n
    R <- 1:N
    scores <- switch(test, wilcoxon = (2/(N + 1)) * R - 1, normal.scores = qnorm(R/(N +
        1)), moods.median = sign(R - (N + 1)/2), savage.scores = cumsum(1/(N -
        R + 1)))
    z <- c(x, y)
    index <- c(rep(1, m), rep(2, n))
    index <- index[order(z)]
    z <- sort(z)
    R <- rank(z)
    if (any(table(R) > 1)) {
        vec <- sapply(split(scores, R), mean)
        scores <- vec[match(R, as.numeric(names(vec)))]
    }
    V <- sum(scores[index == 1])
    E.V <- m * mean(scores)
    Var.V <- ((m * n)/N) * var(scores)
    z <- (V - E.V)/sqrt(Var.V)
    p.value <- switch(alternative, two.sided = 2 * (1 - pnorm(abs(z))),
        less = pnorm(z), greater = 1 - pnorm(z))
    stat <- z
    names(stat) <- "z"
    parameters <- NULL
    string <- switch(alternative, two.sided = "!=", less = "<",
        greater = ">")
    null.value <- "Fx(t)"
    if (shift.type == "location")
        names(null.value) <- ifelse(location.shift.null == 0,
            "Fy(t)", paste("Fy(t - ", location.shift.null, ")",
                sep = ""))
    else {
        names(null.value) <- ifelse(scale.shift.null == 1, "Fy(t)",
            paste("Fy(t / ", scale.shift.null, ")", sep = ""))
    }
    alternative <- paste(names(null.value), string, "Fx(t) for at least one t")
    string1 <- switch(test, wilcoxon = "Wilcoxon Rank Sum Test",
        normal.scores = "Normal Scores Test", savage.scores = "Savage Scores Test",
        moods.median = "Mood's Median Test")
    method <- paste("Two-Sample Linear Rank Test:", string1,
        "Based on Normal Approximation", sep = paste("\n", space(33),
            sep = ""))
    ret.list <- list(statistic = stat, parameters = parameters,
        p.value = p.value, estimate = NULL, null.value = null.value,
        alternative = alternative, method = method, estimation.method = NULL,
        sample.size = c(nx = m, ny = n), data.name = data.name,
        bad.obs = c(x = bad.obs.x, y = bad.obs.y))
    oldClass(ret.list) <- "htestEnvStats"
    ret.list
}

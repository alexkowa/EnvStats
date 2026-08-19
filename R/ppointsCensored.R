#' Plotting Positions for Type I Censored Data
#' @description
#' Returns a list of \dQuote{ordered} observations and associated plotting positions based on
#'   Type I left-censored or right-censored data.  These plotting positions may be used to
#'   construct empirical cumulative distribution plots or quantile-quantile plots, or to estimate
#'   distribution parameters.
#' @usage
#' ppointsCensored(x, censored, censoring.side = "left",
#'     prob.method = "michael-schucany", plot.pos.con = 0.375)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   numeric vector of observations.  Missing (\code{NA}), undefined (\code{NaN}), and
#'   infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
#' }
#'   \item{censored}{
#'   numeric or logical vector indicating which values of \code{x} are censored.  This must be the
#'   same length as \code{x}.  If the mode of \code{censored} is \code{"logical"}, \code{TRUE} values
#'   correspond to elements of \code{x} that are censored, and \code{FALSE} values correspond to
#'   elements of \code{x} that are not censored.  If the mode of \code{censored} is \code{"numeric"},
#'   it must contain only \code{1}'s and \code{0}'s; \code{1} corresponds to \code{TRUE} and
#'   \code{0} corresponds to \code{FALSE}.  Missing (\code{NA}) values are allowed but will be removed.
#' }
#'   \item{censoring.side}{
#'   character string indicating on which side the censoring occurs.  The possible values are
#'   \code{"left"} (the default) and \code{"right"}.
#' }
#'   \item{prob.method}{
#'   character string indicating what method to use to compute the plotting positions
#'   (empirical probabilities).  Possible values are: \cr
#'   \code{"kaplan-meier"} (product-limit method of Kaplan and Meier (1958)), \cr
#'   \code{"modified kaplan-meier"} (modification of Kaplan-Meier method), \cr
#'   \code{"nelson"} (hazard plotting method of Nelson (1972)), \cr
#'   \code{"michael-schucany"} (generalization of the product-limit method due to Michael and Schucany (1986)), and \cr
#'   \code{"hirsch-stedinger"} (generalization of the product-limit method due to Hirsch and Stedinger (1987)). \cr
#'
#'   The default value is \code{prob.method="michael-schucany"}.
#'
#'   The \code{"nelson"} method is only available for \code{censoring.side="right"}, and
#'   the \code{"modified kaplan-meier"} method is only available for \cr
#'   \code{censoring.side="left"}.  See the DETAILS section for more explanation.
#' }
#'   \item{plot.pos.con}{
#'   numeric scalar between 0 and 1 containing the value of the plotting position constant.
#'   The default value is \code{plot.pos.con=0.375}.  See the DETAILS section for more information.
#'   This argument is used only if \code{prob.method} is equal to \code{"michael-schucany"} or
#'   \code{"hirsch-stedinger"}.
#' }
#' }
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{ppointsCensored}.
#' @rawRd
#' \value{
#'   \code{ppointsCensored} returns a list with the following components:
#'
#'   \item{Order.Statistics}{numeric vector of the \dQuote{ordered} observations.}
#'   \item{Cumulative.Probabilities}{numeric vector of the associated plotting positions.}
#'   \item{Censored}{logical vector indicating which of the ordered observations are censored.}
#'   \item{Censoring.Side}{character string indicating whether the data are left- or right-censored.
#'     This is same value as the argument \code{censoring.side}.}
#'   \item{Prob.Method}{character string indicating what method was used to compute the plotting positions.
#'     This is the same value as the argument \code{prob.method}.}
#'
#'   Optional Component (only present when \code{prob.method="michael-schucany"} or \cr
#'   \code{prob.method="hirsch-stedinger"}):
#'   \item{Plot.Pos.Con}{numeric scalar containing the value of the plotting position constant that was used.
#'     This is the same as the argument \code{plot.pos.con}.}
#' }
#' @rawRd
#' \references{
#'   Chambers, J.M., W.S. Cleveland, B. Kleiner, and P.A. Tukey. (1983).
#'   \emph{Graphical Methods for Data Analysis}. Duxbury Press, Boston, MA, pp.11-16.
#'
#'   Cleveland, W.S. (1993). \emph{Visualizing Data}. Hobart Press, Summit, New Jersey, 360pp.
#'
#'   D'Agostino, R.B. (1986a). Graphical Analysis.
#'   In: D'Agostino, R.B., and M.A. Stephens, eds. \emph{Goodness-of Fit Techniques}.
#'   Marcel Dekker, New York, Chapter 2, pp.7-62.
#'
#'   Gillespie, B.W., Q. Chen, H. Reichert, A. Franzblau, E. Hedgeman, J. Lepkowski,
#'   P. Adriaens, A. Demond, W. Luksemburg, and D.H. Garabrant. (2010).  Estimating Population
#'   Distributions When Some Data Are Below a Limit of Detection by Using a Reverse
#'   Kaplan-Meier Estimator.  \emph{Epidemiology} \bold{21}(4), S64--S70.
#'
#'   Helsel, D.R. (2012). \emph{Statistics for Censored Environmental Data Using Minitab and R,
#'   Second Edition}.  John Wiley & Sons, Hoboken, New Jersey.
#'
#'   Helsel, D.R., and T.A. Cohn. (1988). Estimation of Descriptive Statistics for Multiply Censored
#'   Water Quality Data. \emph{Water Resources Research} \bold{24}(12), 1997-2004.
#'
#'   Hirsch, R.M., and J.R. Stedinger. (1987). Plotting Positions for Historical Floods and Their Precision.
#'   \emph{Water Resources Research} \bold{23}(4), 715-727.
#'
#'   Kaplan, E.L., and P. Meier. (1958). Nonparametric Estimation From Incomplete Observations.
#'   \emph{Journal of the American Statistical Association} \bold{53}, 457-481.
#'
#'   Lee, E.T., and J. Wang. (2003). \emph{Statistical Methods for Survival Data Analysis,
#'   Third Edition}.  John Wiley and Sons, New York.
#'
#'   Michael, J.R., and W.R. Schucany. (1986). Analysis of Data from Censored Samples.
#'   In D'Agostino, R.B., and M.A. Stephens, eds. \emph{Goodness-of Fit Techniques}.
#'   Marcel Dekker, New York, 560pp, Chapter 11, 461-496.
#'
#'   Nelson, W. (1972). Theory and Applications of Hazard Plotting for Censored Failure Data.
#'   \emph{Technometrics} \bold{14}, 945-966.
#'
#'   USEPA. (2009).  \emph{Statistical Analysis of Groundwater Monitoring Data at RCRA Facilities, Unified Guidance}.
#'   EPA 530/R-09-007, March 2009.  Office of Resource Conservation and Recovery Program Implementation and Information Division.
#'   U.S. Environmental Protection Agency, Washington, D.C. Chapter 15.
#'
#'   USEPA. (2010).  \emph{Errata Sheet - March 2009 Unified Guidance}.
#'   EPA 530/R-09-007a, August 9, 2010.  Office of Resource Conservation and Recovery, Program Information and Implementation Division.
#'   U.S. Environmental Protection Agency, Washington, D.C.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   For censored data sets, plotting positions may be used to construct empirical cumulative distribution
#'   plots (see \code{\link{ecdfPlotCensored}}), construct quantile-quantile plots
#'   (see \code{\link{qqPlotCensored}}), or to estimate distribution parameters
#'   (see \code{\link{FcnsByCatCensoredData}}).
#'
#'   The function \code{\link[survival]{survfit}} in the built-in \R library
#'   \pkg{survival} computes the survival function for right-censored, left-censored, or
#'   interval-censored data.  Calling \code{\link[survival]{survfit}} with
#'   \code{type="kaplan-meier"} will produce similar results to calling
#'   \code{ppointsCensored} with \code{prob.method="kaplan-meier"}.  Also, calling
#'   \code{\link[survival]{survfit}} with \code{type="fh2"} will produce similar results
#'   to calling \code{ppointsCensored} with \code{prob.method="nelson"}.
#'
#'   Helsel and Cohn (1988, p.2001) found very little effect of changing the value of the plotting position
#'   constant when using the method of Hirsch and Stedinger (1987) to compute plotting positions for
#'   multiply left-censored data.  In general, there will be very little difference between plotting positions
#'   computed by the different methods except in the case of very small samples and a large amount of
#'   censoring.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{ppoints}}, \code{\link{ecdfPlot}}, \code{\link{qqPlot}},
#'   \code{\link{ecdfPlotCensored}}, \code{\link{qqPlotCensored}},
#'   \code{\link[survival]{survfit}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 20 observations from a normal distribution with mean=20 and sd=5,
#'   # censor all observations less than 18, then compute plotting positions for
#'   # this data set.  Compare the plotting positions to the plotting positions
#'   # for the uncensored data set.  Note that the plotting positions for the
#'   # censored data set start at the first ordered uncensored observation and
#'   # that for values of x > 18 the plotting positions for the two data sets are
#'   # exactly the same. This is because there is only one censoring level and
#'   # no uncensored observations fall below the censored observations.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(333)
#'   x <- rnorm(20, mean=20, sd=5)
#'   censored <- x < 18
#'   censored
#'   # [1] FALSE FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE
#'   #[13] FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE
#'
#'   sum(censored)
#'   #[1] 7
#'
#'   new.x <- x
#'   new.x[censored] <- 18
#'   round(sort(new.x),1)
#'   # [1] 18.0 18.0 18.0 18.0 18.0 18.0 18.0 18.1 18.7 19.6 20.2 20.3 20.6 21.4
#'   #[15] 21.8 21.8 23.2 26.2 26.8 29.7
#'
#'   p.list <- ppointsCensored(new.x, censored)
#'   p.list
#'   #$Order.Statistics
#'   # [1] 18.00000 18.00000 18.00000 18.00000 18.00000 18.00000 18.00000 18.09771
#'   # [9] 18.65418 19.58594 20.21931 20.26851 20.55296 21.38869 21.76359 21.82364
#'   #[17] 23.16804 26.16527 26.84336 29.67340
#'   #
#'   #$Cumulative.Probabilities
#'   # [1] 0.3765432 0.3765432 0.3765432 0.3765432 0.3765432 0.3765432 0.3765432
#'   # [8] 0.3765432 0.4259259 0.4753086 0.5246914 0.5740741 0.6234568 0.6728395
#'   #[15] 0.7222222 0.7716049 0.8209877 0.8703704 0.9197531 0.9691358
#'   #
#'   #$Censored
#'   # [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE
#'   #[13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#'   #
#'   #$Censoring.Side
#'   #[1] "left"
#'   #
#'   #$Prob.Method
#'   #[1] "michael-schucany"
#'   #
#'   #$Plot.Pos.Con
#'   #[1] 0.375
#'
#'   #----------
#'
#'   # Round off plotting positions to two decimal places
#'   # and compare to plotting positions that ignore censoring
#'   #--------------------------------------------------------
#'
#'   round(p.list$Cum, 2)
#'   # [1] 0.38 0.38 0.38 0.38 0.38 0.38 0.38 0.38 0.43 0.48 0.52 0.57 0.62 0.67
#'   #[15] 0.72 0.77 0.82 0.87 0.92 0.97
#'
#'   round(ppoints(x, a=0.375), 2)
#'   # [1] 0.03 0.08 0.13 0.18 0.23 0.28 0.33 0.38 0.43 0.48 0.52 0.57 0.62 0.67
#'   #[15] 0.72 0.77 0.82 0.87 0.92 0.97
#'
#'   #----------
#'
#'   # Clean up
#'   #---------
#'   rm(x, censored, new.x, p.list)
#'
#'   #----------------------------------------------------------------------------
#'
#'   # Reproduce the example in Appendix B of Helsel and Cohn (1988).  The data
#'   # are stored in Helsel.Cohn.88.appb.df.  This data frame contains 18
#'   # observations, of which 9 are censored below one of 2 distinct censoring
#'   # levels.
#'
#'   Helsel.Cohn.88.app.b.df
#'   #   Conc.orig Conc Censored
#'   #1         <1    1     TRUE
#'   #2         <1    1     TRUE
#'   #...
#'   #17        33   33    FALSE
#'   #18        50   50    FALSE
#'
#'   p.list <- with(Helsel.Cohn.88.app.b.df,
#'     ppointsCensored(Conc, Censored, prob.method="hirsch-stedinger", plot.pos.con=0))
#'   lapply(p.list[1:2], round, 3)
#'   #$Order.Statistics
#'   # [1]  1  1  1  1  1  1  3  7  9 10 10 10 12 15 20 27 33 50
#'   #
#'   #$Cumulative.Probabilities
#'   # [1] 0.063 0.127 0.190 0.254 0.317 0.381 0.500 0.556 0.611 0.167 0.333 0.500
#'   #[13] 0.714 0.762 0.810 0.857 0.905 0.952
#'
#'   # Clean up
#'   #---------
#'   rm(p.list)
#'
#'   #----------------------------------------------------------------------------
#'
#'   # Example 15-1 of USEPA (2009, page 15-10) gives an example of
#'   # computing plotting positions based on censored manganese
#'   # concentrations (ppb) in groundwater collected at 5 monitoring
#'   # wells.  The data for this example are stored in
#'   # EPA.09.Ex.15.1.manganese.df.
#'
#'   EPA.09.Ex.15.1.manganese.df
#'   #   Sample   Well Manganese.Orig.ppb Manganese.ppb Censored
#'   #1       1 Well.1                 <5           5.0     TRUE
#'   #2       2 Well.1               12.1          12.1    FALSE
#'   #3       3 Well.1               16.9          16.9    FALSE
#'   #4       4 Well.1               21.6          21.6    FALSE
#'   #5       5 Well.1                 <2           2.0     TRUE
#'   #...
#'   #21      1 Well.5               17.9          17.9    FALSE
#'   #22      2 Well.5               22.7          22.7    FALSE
#'   #23      3 Well.5                3.3           3.3    FALSE
#'   #24      4 Well.5                8.4           8.4    FALSE
#'   #25      5 Well.5                 <2           2.0     TRUE
#'
#'   p.list.EPA <- with(EPA.09.Ex.15.1.manganese.df,
#'     ppointsCensored(Manganese.ppb, Censored,
#'       prob.method = "kaplan-meier"))
#'   data.frame(Mn = p.list.EPA$Order.Statistics, Censored = p.list.EPA$Censored,
#'     CDF = p.list.EPA$Cumulative.Probabilities)
#'   #      Mn Censored  CDF
#'   #1    2.0     TRUE 0.21
#'   #2    2.0     TRUE 0.21
#'   #3    2.0     TRUE 0.21
#'   #4    3.3    FALSE 0.28
#'   #5    5.0     TRUE 0.28
#'   #6    5.0     TRUE 0.28
#'   #7    5.0     TRUE 0.28
#'   #8    5.3    FALSE 0.32
#'   #9    6.3    FALSE 0.36
#'   #10   7.7    FALSE 0.40
#'   #11   8.4    FALSE 0.44
#'   #12   9.5    FALSE 0.48
#'   #13  10.0    FALSE 0.52
#'   #14  11.9    FALSE 0.56
#'   #15  12.1    FALSE 0.60
#'   #16  12.6    FALSE 0.64
#'   #17  16.9    FALSE 0.68
#'   #18  17.9    FALSE 0.72
#'   #19  21.6    FALSE 0.76
#'   #20  22.7    FALSE 0.80
#'   #21  34.5    FALSE 0.84
#'   #22  45.9    FALSE 0.88
#'   #23  53.6    FALSE 0.92
#'   #24  77.2    FALSE 0.96
#'   #25 106.3    FALSE 1.00
#'
#'   #----------
#'
#'   # Clean up
#'   #---------
#'   rm(p.list.EPA)
#' }
#' @rawRd
#' \keyword{distribution}
#' @rawRd
#' \keyword{dplot}

ppointsCensored <-
function (x, censored, censoring.side = "left", prob.method = "michael-schucany", 
    plot.pos.con = 0.375) 
{
    if (!is.vector(x, mode = "numeric")) 
        stop("'x' must be a numeric vector")
    if (!is.vector(censored, mode = "numeric") & !is.vector(censored, 
        mode = "logical")) 
        stop("'censored' must be a logical or numeric vector")
    if (length(censored) != length(x)) 
        stop("'censored' must be the same length as 'x'")
    if ((bad.obs <- sum(!(ok <- is.finite(x) & is.finite(as.numeric(censored))))) > 
        0) {
        is.not.finite.warning(x)
        is.not.finite.warning(as.numeric(censored))
        x <- x[ok]
        censored <- censored[ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' and 'censored' removed."))
    }
    if (is.numeric(censored)) {
        if (!all(censored == 0 | censored == 1)) 
            stop(paste("When 'censored' is a numeric vector, all values of", 
                "'censored' must be 0 (not censored) or 1 (censored)."))
        censored <- as.logical(censored)
    }
    N <- length(x)
    n <- sum(!censored)
    n.cen <- N - n
    if (n.cen == 0) 
        stop(paste("No censored values indicated by 'censored'. ", 
            "Use the function 'ppoints'."))
    x.no.cen <- x[!censored]
    if (length(unique(x.no.cen)) < 1) 
        stop("'x' must contain at least one non-missing, uncensored value.")
    prob.method <- match.arg(prob.method, c("michael-schucany", 
        "hirsch-stedinger", "kaplan-meier", "modified kaplan-meier", 
        "nelson"))
    censoring.side <- match.arg(censoring.side, c("left", "right"))
    if (censoring.side == "left" && prob.method == "nelson") 
        stop("Nelson method not available for censoring.side='left'")
    if (censoring.side == "right" && prob.method == "modified kaplan-meier") 
        stop("Modified Kaplan-Meier method not available for censoring.side='right'")
    if (!is.vector(plot.pos.con, mode = "numeric") || length(plot.pos.con) != 
        1 || plot.pos.con < 0 || plot.pos.con > 1) 
        stop("'plot.pos.con' must be a numeric scalar between 0 and 1")
    x.cen <- x[censored]
    c.vec <- table(x.cen)
    cen.levels <- sort(unique(x.cen))
    K <- length(cen.levels)
    ord.stats <- sort(x)
    p <- numeric(N)
    temp <- x
    diffs <- diff(ord.stats)
    eps <- min(diffs[diffs > 0])/2
    if (censoring.side == "left") {
        temp[censored] <- x[censored] - eps
        ord.stats.cen <- censored[order(temp)]
        switch(prob.method, `michael-schucany` = {
            j <- rev((1:N)[!ord.stats.cen])
            cond.probs <- (j - plot.pos.con)/(j - plot.pos.con + 
                1)
            p.no.cen <- rev(((N - plot.pos.con + 1)/(N - 2 * 
                plot.pos.con + 1)) * cumprod(cond.probs))
        }, `hirsch-stedinger` = {
            new.cen.levels <- c(-Inf, cen.levels, Inf)
            A <- numeric(K + 1)
            B <- A
            for (j in 0:K) {
                B[j + 1] <- sum(temp < new.cen.levels[j + 1])
                A[j + 1] <- sum(new.cen.levels[j + 1] <= x.no.cen & 
                  x.no.cen < new.cen.levels[j + 2])
            }
            S <- numeric(K + 2)
            S[1] <- 1
            S[K + 2] <- 0
            for (j in K:1) {
                S[j + 1] <- S[j + 2] + (A[j + 1]/(A[j + 1] + 
                  B[j + 1])) * (1 - S[j + 2])
            }
            p.no.cen <- numeric(n)
            i <- 1
            for (j in 0:K) {
                if ((m <- A[j + 1]) > 0) {
                  p.no.cen[(i - 1) + (1:m)] <- (1 - S[j + 1]) + 
                    (S[j + 1] - S[j + 2]) * ppoints(n = m, a = plot.pos.con)
                  i <- i + m
                }
            }
        }, `modified kaplan-meier` = {
            ord.stats.no.cen <- ord.stats[!ord.stats.cen]
            rle.list <- rle(ord.stats)
            y.j <- rle.list$values
            m.j <- rle.list$lengths
            n.j <- sapply(y.j, function(z) sum(z >= ord.stats))
            d.j <- sapply(y.j, function(z) sum(z == ord.stats.no.cen))
            p <- c(rev(cumprod(rev((n.j - d.j)/n.j)))[-1], (N - 
                0.375)/(N + 0.25))
            p <- rep(p, times = m.j)
        }, `kaplan-meier` = {
            ord.stats.no.cen <- ord.stats[!ord.stats.cen]
            rle.list <- rle(ord.stats)
            y.j <- rle.list$values
            m.j <- rle.list$lengths
            n.j <- sapply(y.j, function(z) sum(z >= ord.stats))
            d.j <- sapply(y.j, function(z) sum(z == ord.stats.no.cen))
            p <- c(rev(cumprod(rev((n.j - d.j)/n.j)))[-1], 1)
            p <- rep(p, times = m.j)
        })
    }
    else {
        temp[censored] <- x[censored] + eps
        ord.stats.cen <- censored[order(temp)]
        j <- (1:N)[!ord.stats.cen]
        switch(prob.method, `michael-schucany` = {
            cond.probs <- (N - j - plot.pos.con + 1)/(N - j - 
                plot.pos.con + 2)
            p.no.cen <- 1 - ((N - plot.pos.con + 1)/(N - 2 * 
                plot.pos.con + 1)) * cumprod(cond.probs)
        }, `kaplan-meier` = {
            cond.probs <- (N - j)/(N - j + 1)
            p.no.cen <- 1 - cumprod(cond.probs)
            ord.stats.not.censored <- ord.stats[!ord.stats.cen]
            rle.list <- rle(ord.stats.not.censored)
            lengths <- rle.list$lengths
            values <- rle.list$values
            index <- (1:length(lengths))[lengths > 1]
            if (n.ties <- length(index)) {
                for (i in 1:n.ties) {
                  index2 <- ord.stats.not.censored == values[index[i]]
                  p.no.cen[index2] <- max(p.no.cen[index2])
                }
            }
        }, nelson = {
            cond.probs <- exp(-1/(N - j + 1))
            p.no.cen <- 1 - cumprod(cond.probs)
        }, `hirsch-stedinger` = {
            new.cen.levels <- c(-Inf, cen.levels, Inf)
            A <- numeric(K + 1)
            B <- A
            for (j in 0:K) {
                B[j + 1] <- sum(temp > new.cen.levels[j + 2])
                A[j + 1] <- sum(new.cen.levels[j + 1] < x.no.cen & 
                  x.no.cen <= new.cen.levels[j + 2])
            }
            S <- numeric(K + 2)
            S[1] <- 1
            S[K + 2] <- 0
            for (j in 1:K) {
                S[j + 1] <- S[j] * (1 - A[j]/(A[j] + B[j]))
            }
            p.no.cen <- numeric(n)
            i <- 1
            for (j in 0:K) {
                if ((m <- A[j + 1]) > 0) {
                  p.no.cen[(i - 1) + (1:m)] <- (1 - S[j + 1]) + 
                    (S[j + 1] - S[j + 2]) * ppoints(n = m, a = plot.pos.con)
                  i <- i + m
                }
            }
        })
    }
    if (!(censoring.side == "left" & prob.method %in% c("kaplan-meier", 
        "modified kaplan-meier"))) {
        p[!ord.stats.cen] <- p.no.cen
        if (prob.method == "hirsch-stedinger") {
            if (censoring.side == "right") {
                for (j in 1:K) {
                  index <- ord.stats.cen & (ord.stats == cen.levels[j])
                  p[index] <- rev(1 - S[j + 1] * ppoints(n = c.vec[j], 
                    a = plot.pos.con))
                }
            }
            else {
                for (j in 1:K) {
                  index <- ord.stats.cen & (ord.stats == cen.levels[j])
                  p[index] <- (1 - S[j + 1]) * ppoints(n = c.vec[j], 
                    a = plot.pos.con)
                }
            }
        }
        else {
            ord.stats.no.cen <- ord.stats[!ord.stats.cen]
            if (censoring.side == "right") {
                for (j in 1:K) {
                  Tj <- cen.levels[j]
                  index <- ord.stats.cen & (ord.stats == Tj)
                  index1 <- ord.stats.no.cen <= Tj
                  if (!any(index1)) 
                    p[index] <- 0
                  else p[index] <- p.no.cen[max((1:n)[index1])]
                }
            }
            else {
                for (j in 1:K) {
                  Tj <- cen.levels[j]
                  index <- ord.stats.cen & (ord.stats == Tj)
                  index1 <- ord.stats.no.cen >= Tj
                  if (!any(index1)) 
                    p[index] <- 1
                  else p[index] <- p.no.cen[min((1:n)[index1])]
                }
            }
        }
    }
    ret.list <- list(Order.Statistics = ord.stats, Cumulative.Probabilities = p, 
        Censored = ord.stats.cen, Censoring.Side = censoring.side, 
        Prob.Method = prob.method)
    if (prob.method %in% c("michael-schucany", "hirsch-stedinger")) 
        ret.list <- c(ret.list, list(Plot.Pos.Con = plot.pos.con))
    ret.list
}

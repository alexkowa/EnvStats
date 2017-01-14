ci.qnpar <-
function (x, p, lcl.rank = NULL, ucl.rank = NULL, lb = -Inf, 
    ub = Inf, ci.type = "two-sided", ci.method = "interpolate", 
    digits = getOption("digits"), approx.conf.level = 0.95, min.coverage = TRUE, 
    tol = 0) 
{
    x <- sort(x)
    n <- length(x)
    is.null.lcl.rank <- is.null(lcl.rank)
    is.null.ucl.rank <- is.null(ucl.rank)
    if (!is.null.lcl.rank || !is.null.ucl.rank) {
        if (!is.null.lcl.rank && (length(lcl.rank) > 1 || lcl.rank != 
            trunc(lcl.rank) || lcl.rank < 1 || lcl.rank > n)) 
            stop(paste("'lcl.rank' must be an integer between 1 and", 
                n))
        if (!is.null.ucl.rank && (length(ucl.rank) > 1 || ucl.rank != 
            trunc(ucl.rank) || ucl.rank < 1 || ucl.rank > n)) 
            stop(paste("'ucl.rank' must be an integer between 1 and", 
                n))
        if (!is.null.lcl.rank && is.null.ucl.rank) {
            lcl <- x[lcl.rank]
            ucl <- ub
            ucl.rank <- NA
            ci.type <- "lower"
            conf.level <- 1 - pbinom(lcl.rank - 1, n, p)
        }
        else if (is.null.lcl.rank && !is.null.ucl.rank) {
            lcl <- lb
            lcl.rank <- NA
            ucl <- x[ucl.rank]
            ci.type <- "upper"
            conf.level <- pbinom(ucl.rank - 1, n, p)
        }
        else {
            if (lcl.rank >= ucl.rank) 
                stop("'lcl.rank' must be stricly less than 'ucl.rank'")
            lcl <- x[lcl.rank]
            ucl <- x[ucl.rank]
            ci.type <- "two-sided"
            conf.level <- pbinom(ucl.rank - 1, n, p) - pbinom(lcl.rank - 
                1, n, p)
        }
        ci.method <- "exact"
        ci.limits <- c(lcl, ucl)
        names(ci.limits) <- c("LCL", "UCL")
        limit.ranks <- c(lcl.rank, ucl.rank)
    }
    else {
        ci.type <- match.arg(ci.type, c("two-sided", "lower", 
            "upper"))
        ci.method <- match.arg(ci.method, c("interpolate", "exact", 
            "normal.approx"))
        arg.list <- list(x = x, n = n, p = p, lb = lb, ub = ub, 
            ci.type = ci.type, approx.conf.level = approx.conf.level)
        if (ci.method == "exact") 
            arg.list <- c(arg.list, list(min.coverage = min.coverage, 
                tol = tol))
        ci.list <- do.call(paste("ci.qnpar", ci.method, sep = "."), 
            arg.list)
        ci.limits <- ci.list$ci.limits
        limit.ranks <- ci.list$limit.ranks
        conf.level <- ci.list$conf.level
        ci.method <- ci.list$method
    }
    pct <- round(100 * p, digits)
    ci.parameter <- paste(pct, number.suffix(pct), " %ile", sep = "")
    ret.obj <- list(name = "Confidence", parameter = ci.parameter, 
        limit.ranks = limit.ranks, limits = ci.limits, type = ci.type, 
        method = ci.method, conf.level = conf.level, sample.size = n)
    oldClass(ret.obj) <- "intervalEstimate"
    ret.obj
}

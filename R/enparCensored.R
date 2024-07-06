enparCensored <-
function (x, censored, censoring.side = "left", correct.se = TRUE, 
    restricted = FALSE, left.censored.min = "Censoring Level", 
    right.censored.max = "Censoring Level", 
    ci = FALSE, ci.method = "normal.approx", ci.type = "two-sided", conf.level = 0.95, 
    pivot.statistic = "t", ci.sample.size = "Total", n.bootstraps = 1000, seed = NULL, 
    warn = FALSE) 
{
    if (!is.vector(x, mode = "numeric")) 
        stop("'x' must be a numeric vector")
    if (!is.vector(censored, mode = "numeric") & !is.vector(censored, 
        mode = "logical")) 
        stop("'censored' must be a logical or numeric vector")
    if (length(x) == 0)
        stop("'x' has length 0")
    if (length(censored) != length(x)) 
        stop("'censored' must be the same length as 'x'")
    data.name <- deparse(substitute(x))
    censoring.name <- deparse(substitute(censored))
    if ((bad.obs <- sum(!(ok <- is.finite(x) & is.finite(as.numeric(censored))))) > 
        0) {
        x <- x[ok]
        censored <- censored[ok]
        if(warn) {
            warning(paste(bad.obs, 
                "observations with NA/NaN/Inf in 'x' and 'censored' removed."))
        }
    }
    if (length(x) == 0)
        stop("'x' has length 0 after omiting observations with NA/NaN/Inf")
    if (any(x <= 0)) 
        stop("All values of 'x' must be positive")
    if (is.numeric(censored)) {
        if (!all(censored == 0 | censored == 1)) {
            stop(paste("When 'censored' is a numeric vector, all values of", 
                "'censored' must be 0 (not censored) or 1 (censored)."))
        }
        censored <- as.logical(censored)
    }
    n.cen <- sum(censored)
    if (n.cen == 0) 
        stop("No censored values indicated by 'censored'.  Use the function enpar()")
    x.no.cen <- x[!censored]
    if (length(unique(x.no.cen)) < 2) 
        stop("'x' must contain at least 2 non-missing, uncensored, distinct values.")
    if (!is.logical(correct.se) || length(correct.se) != 1)
        stop("'correct.se' must be a logical scalar")
    if (!is.logical(restricted) || length(restricted) != 1)
        stop("'restricted' must be a logical scalar")
    N <- length(x)
    censoring.side <- match.arg(censoring.side, c("left", "right"))
    x.cen <- x[censored]
    cen.levels <- sort(unique(x.cen))
    ci.method <- match.arg(ci.method, c("normal.approx", "bootstrap"))
    ci.type <- match.arg(ci.type, c("two-sided", "lower", "upper"))
    pivot.statistic <- match.arg(pivot.statistic, c("z", "t"))
    ci.sample.size <- match.arg(ci.sample.size, c("Total", "Uncensored"))
    param.ci.list <- NULL
    method <- NULL
    if (restricted) {
        if (censoring.side == "left") {
            min.cen.level <- min(cen.levels)
            if (min.cen.level <= min(x.no.cen)) {
                if (length(left.censored.min) != 1 || 
                    (!is.character(left.censored.min) & 
                    !is.numeric(left.censored.min))) {
                    stop(paste("The argument 'left.censored.min' must have length 1", 
                         "and be a character string or a numeric scalar."))
                }
                if (is.character(left.censored.min)) {
                    if (left.censored.min != "Censoring Level") 
                      stop(paste("When the argument 'left.censored.min' is a", 
                        "character string, it must equal 'Censoring Level'"))
                }
                else {
                    if (left.censored.min > min.cen.level || left.censored.min <= 0) 
                      stop(paste("When 'left.censored.min' is a numeric scalar", 
                        "it must be less than or equal to the smallest", 
                        "censoring level and greater than 0"))
                }        
                method <- "Kaplan-Meier (Restricted Mean)"
                method <- paste(method, "\n", space(33), 
                    "Smallest censored value(s)", "\n", space(33),
                        paste("  set to", left.censored.min), 
                        sep = "")
                index.which.cen.and.eq.min.cen.level <- 
                    which(censored & (x == min.cen.level))
                if(sum(censored[-index.which.cen.and.eq.min.cen.level]) == 0) {
                    if(warn) {
                        warning(paste("No censored observations left after applying", 
                            "the argument left.censored.min, so enpar() called."))
                    }
                    param.ci.list <- enpar(x, ci = ci, ci.method = ci.method, 
                        ci.type = ci.type, conf.level = conf.level, 
                        pivot.statistic = pivot.statistic, n.bootstraps = n.bootstraps, 
                        seed = seed)
                }
                else {
                    if (is.numeric(left.censored.min)) {
                        x[index.which.cen.and.eq.min.cen.level] <- left.censored.min
                    }
                    censored[index.which.cen.and.eq.min.cen.level] <- FALSE
                }
            }
        }
        else {
            max.cen.level <- max(cen.levels)
            if (max.cen.level >= max(x.no.cen)) {
                if (length(right.censored.max) != 1 || 
                    (!is.character(right.censored.max) & 
                    !is.numeric(right.censored.max))) {
                    stop(paste("The argument 'right.censored.max' must have length 1", 
                        "and be a character string or a numeric scalar."))
                }
                if (is.character(right.censored.max)) {
                    if (right.censored.max != "Censoring Level") 
                      stop(paste("When the argument 'right.censored.max' is a", 
                        "character string, it must equal 'Censoring Level'"))
                }
                else {
                    if (right.censored.max < max.cen.level) 
                      stop(paste("When 'right.censored.max' is a numeric scalar", 
                        "it must be greater than or equal to the largest", 
                        "censoring level"))
                }
                method <- "Kaplan-Meier (Restricted Mean)"
                method <- paste(method, "\n", space(33), 
                    "Largest censored value(s)", "\n", space(33),
                    paste("  set to", right.censored.max), 
                    sep = "")
                index.which.cen.and.eq.max.cen.level <- which(censored & (x == max.cen.level))
                if(sum(censored[-index.which.cen.and.eq.max.cen.level]) == 0) {
                    if(warn) {
                        warning(paste("No censored observations left after applying", 
                            "the argument right.censored.max, so enpar() called."))
                    }
                    param.ci.list <- enpar(x, ci = ci, ci.method = ci.method, 
                        ci.type = ci.type, conf.level = conf.level, 
                        pivot.statistic = pivot.statistic, n.bootstraps = n.bootstraps, 
                        seed = seed)
                }
                else {
                    if (is.numeric(right.censored.max)) {
                        x[index.which.cen.and.eq.max.cen.level] <- right.censored.max
                    }
                    censored[index.which.cen.and.eq.max.cen.level] <- FALSE
                }
            }
        }
    }
    if(is.null(param.ci.list)) {
        if(is.null(method)) {
            method <- "Kaplan-Meier"
        }
        if (correct.se) {
            method <- paste(method, "\n", space(33), "(Bias-corrected se.mean)", 
                sep = "")
        }
        if (!ci || ci.method != "bootstrap") {
            param.ci.list <- enparCensored.km(x = x, censored = censored, 
                censoring.side = censoring.side, correct.se = correct.se, 
                ci = ci, ci.type = ci.type, conf.level = conf.level, 
                pivot.statistic = pivot.statistic, ci.sample.size = ci.sample.size)
        }
        else {
            param.ci.list <- enparCensored.km(x = x, censored = censored, 
                censoring.side = censoring.side, correct.se = correct.se, 
                ci = FALSE)
            ci.list <- enparCensored.bootstrap.ci(x = x, censored = censored, 
                censoring.side = censoring.side, correct.se = correct.se, 
                est.fcn = "enparCensored.km", ci.type = ci.type, 
                conf.level = conf.level, n.bootstraps = n.bootstraps, 
                obs.mean = param.ci.list$parameters["mean"], 
                obs.se.mean = param.ci.list$parameters["se.mean"], seed = seed)
            param.ci.list <- c(param.ci.list, list(ci.obj = ci.list))
        }
    }
    ret.list <- list(distribution = "None", sample.size = N, 
        censoring.side = censoring.side, censoring.levels = cen.levels, 
        percent.censored = (100 * n.cen)/N, parameters = param.ci.list$parameters, 
        n.param.est = 2, method = method, data.name = data.name, 
        censoring.name = censoring.name, bad.obs = bad.obs)
    if (ci) {
        ret.list <- c(ret.list, list(interval = param.ci.list$ci.obj))
        if (!is.null(param.ci.list$var.cov.params)) 
            ret.list <- c(ret.list, list(var.cov.params = param.ci.list$var.cov.params))
    }
    oldClass(ret.list) <- "estimateCensored"
    ret.list
}

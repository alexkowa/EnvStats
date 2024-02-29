enpar <-
function (x, ci = FALSE, ci.method = "bootstrap", ci.type = "two-sided", 
    conf.level = 0.95, pivot.statistic = "z", n.bootstraps = 1000, seed = NULL) 
{
    if (!is.vector(x, mode = "numeric")) 
        stop("'x' must be a numeric vector")
    data.name <- deparse(substitute(x))
    if ((bad.obs <- sum(!(ok <- is.finite(x)))) > 0) {
        x <- x[ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' removed."))
    }
    if (length(unique(x)) < 2) 
        stop("'x' must contain at least 2 non-missing, distinct values.")
    N <- length(x)
    est.fcn <- function(x) {
        sd.x <- sd(x)
        c(mean = mean(x), sd = sd.x, se.mean = sd.x / sqrt(length(x)))
    }
    parameters <- est.fcn(x)
    param.ci.list <- list(parameters = parameters)
    if(ci) {
        ci.method <- match.arg(ci.method, c("normal.approx", "bootstrap"))
        ci.type <- match.arg(ci.type, c("two-sided", "lower", "upper"))
        if(ci.method == "normal.approx") {
            pivot.statistic <- match.arg(pivot.statistic, c("z", "t"))
            ci.obj <- ci.normal.approx(theta.hat = parameters["mean"], 
                sd.theta.hat = parameters["se.mean"], n = N, 
                df = N - 1, ci.type = ci.type, alpha = 1 - conf.level, 
                test.statistic = pivot.statistic)
            ci.obj$parameter <- "mean"
        }
        else {
            ci.obj <- enpar.bootstrap.ci(x = x, est.fcn = est.fcn, ci.type = ci.type, 
                conf.level = conf.level, n.bootstraps = n.bootstraps, 
                obs.mean = param.ci.list$parameters["mean"], 
                obs.se.mean = param.ci.list$parameters["se.mean"], seed = seed)
        }
        param.ci.list <- c(param.ci.list, list(ci.obj = ci.obj))
    }
    method <- "Sample Mean"
    ret.list <- list(distribution = "None", sample.size = N, 
        parameters = param.ci.list$parameters, n.param.est = 2, 
        method = method, data.name = data.name, 
        bad.obs = bad.obs)
    if (ci) {
        ret.list <- c(ret.list, list(interval = param.ci.list$ci.obj))
    }
    oldClass(ret.list) <- "estimate"
    ret.list
}



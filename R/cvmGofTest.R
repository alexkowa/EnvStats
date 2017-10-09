cvmGofTest <-
function (x, distribution = c("norm", "lnorm", "lnormAlt", "zmnorm", 
    "zmlnorm", "zmlnormAlt"), est.arg.list = NULL) 
{
    if (!is.vector(x, mode = "numeric") || is.factor(x)) 
        stop("'x' must be a numeric vector")
    data.name <- deparse(substitute(x))
    if ((bad.obs <- sum(!(x.ok <- is.finite(x)))) > 0) {
        is.not.finite.warning(x)
        x <- x[x.ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' removed."))
    }
    distribution <- match.arg(distribution)
    if (any(distribution == c("lnorm", "lnormAlt")) && any(x <= 
        0)) 
        stop("All values of 'x' must be positive for a lognormal distribution")
    if (any(distribution == c("zmlnorm", "zmlnormAlt")) && any(x < 
        0)) 
        stop(paste("All values of 'x' must be non-negative for a", 
            "zero-modified lognormal distribution"))
    if (length(unique(x)) < 7) 
        stop(paste("'x' must contain at least 7 distinct non-missing values.", 
            "This is not true for 'x' =", data.name))
    est.fcn <- paste("e", distribution, sep = "")
    ret.list <- do.call(est.fcn, c(list(x = x), est.arg.list))
    nrl <- names(ret.list)
    names(ret.list)[match("parameters", nrl)] <- "distribution.parameters"
    names(ret.list)[match("method", nrl)] <- "estimation.method"
    ret.list$data.name <- data.name
    ret.list$bad.obs <- bad.obs
    ret.list$dist.abb <- distribution
    new.x <- switch(distribution, norm = x, lnorm = , lnormAlt = log(x), 
        zmnorm = x[x != 0], zmlnorm = , zmlnormAlt = log(x[x > 
            0]))
    n <- length(new.x)
    test.list <- nortest::cvm.test(new.x)
    ret.list <- c(ret.list, list(statistic = test.list$statistic, 
        parameters = n, p.value = test.list$p.value, alternative = paste("True cdf does not equal the\n", 
            space(33), ret.list$distribution, " Distribution.", 
            sep = ""), method = "Cramer-von Mises GOF", data = x))
    names(ret.list$parameters) <- "n"
    ret.list <- ret.list[c("distribution", "dist.abb", "distribution.parameters", 
        "n.param.est", "estimation.method", "statistic", "sample.size", 
        "parameters", "p.value", "alternative", "method", "data", 
        "data.name", "bad.obs")]
    oldClass(ret.list) <- "gof"
    ret.list
}

ksGammaProUCLGofTest <-
function (x, distribution = c("gamma", "gammaAlt")) 
{
    if (!is.vector(x, mode = "numeric") || is.factor(x)) 
        stop("'x' must be a numeric vector")
    data.name <- deparse(substitute(x))
    if ((bad.obs <- sum(!(x.ok <- is.finite(x)))) > 0) {
        is.not.finite.warning(x)
        x <- x[x.ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' removed."))
    }
    if (any(x <= 0)) 
        stop("All values of 'x' must be positive for a gamma distribution")
    n <- length(x)
    if (n < 5 || length(unique(x)) < 3) 
        stop(paste("'x' must contain at least 5 non-missing values,", 
            "and at least 3 distinct values."))
    if (n > 1000) 
        stop(paste("Too many observations.  This test method only works", 
            "if the number of observations is between 5 and 1000"))
    distribution <- match.arg(distribution)
    egamma.list <- egamma(x, method = "mle")
    shape <- egamma.list$parameters["shape"]
    scale <- egamma.list$parameters["scale"]
    if (shape < 0.025) 
        stop("Estimated shape parameter must be greater than or equal to 0.025")
    if (distribution == "gammaAlt") {
        egammaAlt.list <- egammaAlt(x, method = "mle")
        ret.list <- egammaAlt.list
    }
    else {
        ret.list <- egamma.list
    }
    nrl <- names(ret.list)
    names(ret.list)[match("parameters", nrl)] <- "distribution.parameters"
    names(ret.list)[match("method", nrl)] <- "estimation.method"
    ret.list$n.param.est <- 2
    ret.list$data.name <- data.name
    ret.list$bad.obs <- bad.obs
    ret.list$dist.abb <- distribution
    n.vec <- dimnames(EnvStats::ProUCL.Crit.Vals.for.KS.Test.for.Gamma.array)[[1]]
    n.vec <- as.numeric(substr(n.vec, 6, nchar(n.vec)))
    n.rows <- 1:length(n.vec)
    if (any(index <- n == n.vec)) 
        row.index.n <- n.rows[index]
    else {
        row.index.n <- rep(as.integer(NA), 2)
        index.GT <- n > n.vec
        index.LT <- n < n.vec
        row.index.n[1] <- max(n.rows[index.GT])
        row.index.n[2] <- min(n.rows[index.LT])
    }
    n.vec <- n.vec[row.index.n]
    k.vec <- dimnames(EnvStats::ProUCL.Crit.Vals.for.KS.Test.for.Gamma.array)[[2]]
    k.vec <- as.numeric(substr(k.vec, 6, nchar(k.vec)))
    n.k <- length(k.vec)
    k.cols <- 1:n.k
    if (shape > 50) {
        col.index.k <- n.k
    }
    else if (any(index <- shape == k.vec)) 
        col.index.k <- k.cols[index]
    else {
        col.index.k <- rep(as.integer(NA), 2)
        index.GT <- shape > k.vec
        index.LT <- shape < k.vec
        col.index.k[1] <- max(k.cols[index.GT])
        col.index.k[2] <- min(k.cols[index.LT])
    }
    k.vec <- k.vec[col.index.k]
    crit.vals.names <- dimnames(EnvStats::ProUCL.Crit.Vals.for.KS.Test.for.Gamma.array)[[3]]
    p.vec <- as.numeric(substr(crit.vals.names, 3, nchar(crit.vals.names)))
    interp.array <- EnvStats::ProUCL.Crit.Vals.for.KS.Test.for.Gamma.array[row.index.n, 
        col.index.k, , drop = FALSE]
    dimnames(interp.array) <- list(NULL, NULL, NULL)
    interp.fcn <- function(mat, n.vec, k.vec, n, k) {
        length.n.vec <- length(n.vec)
        length.k.vec <- length(k.vec)
        if (length.n.vec == 1 && length.k.vec == 1) {
            ret.val <- mat
        }
        else {
            digits <- min(nchar(sapply(strsplit(sub("0+$", "", 
                as.character(mat)), ".", fixed = TRUE), function(x) x[2])))
            if (digits < 3) 
                digits <- 3
            if (length.n.vec == 1) {
                ret.val <- approx(x = k.vec, y = mat, xout = k)$y
            }
            else if (length.k.vec == 1) {
                ret.val <- approx(x = n.vec, y = mat, xout = n)$y
            }
            else {
                dum.list <- apply(mat, 2, approx, x = n.vec, 
                  xout = n)
                dum.vec <- sapply(dum.list, function(z) z$y)
                ret.val <- approx(x = k.vec, y = dum.vec, xout = k)$y
            }
            ret.val <- round(ret.val, digits)
        }
        ret.val
    }
    ks.crit.vals <- apply(interp.array, 3, interp.fcn, n.vec = n.vec, 
        k.vec = k.vec, n = n, k = shape)
    names(ks.crit.vals) <- crit.vals.names
    ks.stat <- ks.test(x, "pgamma", shape = shape, scale = scale)$statistic
    if (ks.stat > ks.crit.vals["D.0.01"]) 
        p <- "< 0.01"
    else if (ks.stat > ks.crit.vals["D.0.05"]) 
        p <- "0.01 <= p < 0.05"
    else if (ks.stat > ks.crit.vals["D.0.10"]) 
        p <- "0.05 <= p < 0.10"
    else p <- ">= 0.10"
    ret.list <- c(ret.list, list(statistic = ks.stat, parameters = n, 
        crit.vals = ks.crit.vals, p.value = p, alternative = paste("True cdf does not equal the\n", 
            space(33), ret.list$distribution, " Distribution.", 
            sep = ""), method = "ProUCL Kolmogorov-Smirnov Gamma GOF", 
        data = x))
    names(ret.list$parameters) <- "n"
    ret.list <- ret.list[c("distribution", "dist.abb", "distribution.parameters", 
        "n.param.est", "estimation.method", "statistic", "sample.size", 
        "parameters", "crit.vals", "p.value", "alternative", 
        "method", "data", "data.name", "bad.obs")]
    oldClass(ret.list) <- "gof"
    ret.list
}

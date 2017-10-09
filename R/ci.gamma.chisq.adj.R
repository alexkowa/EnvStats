ci.gamma.chisq.adj <-
function (x, shape, shape.est.method, ci.type, conf.level) 
{
    x.bar <- mean(x)
    n <- length(x)
    alpha <- 1 - conf.level
    n.vec <- dimnames(EnvStats::Grice.Bain.80.mat)[[1]]
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
    alpha.vec <- dimnames(EnvStats::Grice.Bain.80.mat)[[2]]
    alpha.vec <- as.numeric(substr(alpha.vec, 9, nchar(alpha.vec)))
    n.alpha <- length(alpha.vec)
    alpha.cols <- 1:n.alpha
    if (any(index <- alpha == alpha.vec)) 
        col.index.alpha <- alpha.cols[index]
    else {
        col.index.alpha <- rep(as.integer(NA), 2)
        index.GT <- alpha > alpha.vec
        index.LT <- alpha < alpha.vec
        col.index.alpha[1] <- max(alpha.cols[index.GT])
        col.index.alpha[2] <- min(alpha.cols[index.LT])
    }
    alpha.vec <- alpha.vec[col.index.alpha]
    interp.mat <- EnvStats::Grice.Bain.80.mat[row.index.n, col.index.alpha, 
        drop = FALSE]
    dimnames(interp.mat) <- list(NULL, NULL)
    interp.fcn <- function(mat, n.vec, alpha.vec, n, alpha) {
        length.n.vec <- length(n.vec)
        length.alpha.vec <- length(alpha.vec)
        if (length.n.vec == 1 && length.alpha.vec == 1) {
            ret.val <- mat
        }
        else {
            if (length.n.vec == 1) {
                ret.val <- approx(x = alpha.vec, y = mat, xout = alpha)$y
            }
            else if (length.alpha.vec == 1) {
                if (all(is.finite(n.vec))) {
                  ret.val <- approx(x = n.vec, y = mat, xout = n)$y
                }
                else {
                  ret.val <- approx(x = 1/n.vec, y = mat, xout = 1/n)$y
                }
            }
            else {
                if (all(is.finite(n.vec))) {
                  dum.list <- apply(mat, 2, approx, x = n.vec, 
                    xout = n)
                }
                else {
                  dum.list <- apply(mat, 2, approx, x = 1/n.vec, 
                    xout = 1/n)
                }
                dum.vec <- sapply(dum.list, function(z) z$y)
                ret.val <- approx(x = alpha.vec, y = dum.vec, 
                  xout = alpha)$y
            }
            ret.val <- round(ret.val, 4)
        }
        ret.val
    }
    alpha.adj <- interp.fcn(mat = interp.mat, n.vec = n.vec, 
        alpha.vec = alpha.vec, n = n, alpha = alpha)
    names(alpha.adj) <- NULL
    switch(ci.type, `two-sided` = {
        alpha <- (1 - conf.level)/2
        LCL <- 2 * n * shape * x.bar/qchisq(p = 1 - alpha.adj/2, 
            df = 2 * n * shape)
        UCL <- 2 * n * shape * x.bar/qchisq(p = alpha.adj/2, 
            df = 2 * n * shape)
    }, lower = {
        LCL <- 2 * n * shape * x.bar/qchisq(p = 1 - alpha.adj, 
            df = 2 * n * shape)
        UCL <- Inf
    }, upper = {
        LCL <- 0
        UCL <- 2 * n * shape * x.bar/qchisq(p = alpha.adj, df = 2 * 
            n * shape)
    })
    ci.limits <- c(LCL, UCL)
    names(ci.limits) <- c("LCL", "UCL")
    interval <- list(name = "Confidence", parameter = "mean", 
        limits = ci.limits, type = ci.type, method = paste("Chi-square adjusted\n", 
            space(33), "using ", shape.est.method, " of 'shape'", 
            sep = ""), conf.level = conf.level, sample.size = length(x))
    oldClass(interval) <- "intervalEstimate"
    interval
}

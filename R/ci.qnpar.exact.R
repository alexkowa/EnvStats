ci.qnpar.exact <-
function (x, n, p, lb = -Inf, ub = Inf, ci.type = "two-sided", 
    approx.conf.level = 0.95, min.coverage = TRUE, tol = 0) 
{
    approx.alpha <- 1 - approx.conf.level
    switch(ci.type, `two-sided` = {
        if (min.coverage) {
            max.conf.level <- pbinom(n - 1, n, p) - pbinom(0, 
                n, p)
            if (max.conf.level < approx.conf.level) stop(paste("Minimum coverage of", 
                approx.conf.level, "is not possible with the given sample size."))
        }
        ao2 <- approx.alpha/2
        lcl.rank.init <- (qbinom(ao2, n, p) + 1) + -2:2
        ucl.rank.init <- qbinom(1 - ao2, n, p) + -2:2
        lcl.vec <- unique(lcl.rank.init[lcl.rank.init >= 1 & 
            lcl.rank.init < n])
        ucl.vec <- unique(ucl.rank.init[ucl.rank.init > 1 & ucl.rank.init <= 
            n])
        rank.mat <- as.matrix(expand.grid(lcl.rank = lcl.vec, 
            ucl.rank = ucl.vec))
        rank.mat <- rank.mat[rank.mat[, 1] < rank.mat[, 2], , 
            drop = FALSE]
        conf.level.exact <- as.vector(diff(t(pbinom(q = rank.mat - 
            1, size = n, prob = p))))
        if (min.coverage) {
            index <- conf.level.exact >= approx.conf.level
        } else {
            index <- conf.level.exact <= approx.conf.level + 
                tol
        }
        rank.mat <- rank.mat[index, , drop = FALSE]
        conf.level.exact <- conf.level.exact[index]
        abs.diff <- abs(conf.level.exact - approx.conf.level)
        index <- which(abs.diff == min(abs.diff))[1]
        conf.level <- conf.level.exact[index]
        cl.ranks <- rank.mat[index, , drop = TRUE]
        lcl.rank <- cl.ranks[1]
        ucl.rank <- cl.ranks[2]
        lcl <- x[lcl.rank]
        ucl <- x[ucl.rank]
    }, lower = {
        if (min.coverage) {
            max.conf.level <- 1 - pbinom(0, n, p)
            if (max.conf.level < approx.conf.level) stop(paste("Minimum coverage of", 
                approx.conf.level, "is not possible with the given sample size."))
        }
        lcl.rank.init <- qbinom(approx.alpha, n, p) + -2:2
        lcl.vec <- unique(lcl.rank.init[lcl.rank.init >= 1 & 
            lcl.rank.init <= n])
        conf.level.exact <- 1 - pbinom(lcl.vec - 1, n, p)
        if (min.coverage) {
            index <- conf.level.exact >= approx.conf.level
        } else {
            index <- conf.level.exact <= approx.conf.level + 
                tol
        }
        lcl.vec <- lcl.vec[index]
        conf.level.exact <- conf.level.exact[index]
        abs.diff <- abs(conf.level.exact - approx.conf.level)
        index <- which(abs.diff == min(abs.diff))[1]
        conf.level <- conf.level.exact[index]
        lcl.rank <- lcl.vec[index]
        ucl.rank <- NA
        lcl <- x[lcl.rank]
        ucl <- ub
    }, upper = {
        if (min.coverage) {
            max.conf.level <- pbinom(n - 1, n, p)
            if (max.conf.level < approx.conf.level) stop(paste("Minimum coverage of", 
                approx.conf.level, "is not possible with the given sample size."))
        }
        ucl.rank.init <- qbinom(approx.conf.level, n, p) + -2:2
        ucl.vec <- ucl.rank.init[ucl.rank.init >= 1 & ucl.rank.init <= 
            n]
        conf.level.exact <- pbinom(ucl.vec - 1, n, p)
        if (min.coverage) {
            index <- conf.level.exact >= approx.conf.level
        } else {
            index <- conf.level.exact <= approx.conf.level + 
                tol
        }
        ucl.vec <- ucl.vec[index]
        conf.level.exact <- conf.level.exact[index]
        abs.diff <- abs(conf.level.exact - approx.conf.level)
        index <- which(abs.diff == min(abs.diff))[1]
        conf.level <- conf.level.exact[index]
        ucl.rank <- ucl.vec[index]
        lcl.rank <- NA
        lcl <- lb
        ucl <- x[ucl.rank]
    })
    ci.limits <- c(lcl, ucl)
    names(ci.limits) <- c("LCL", "UCL")
    ret.obj <- list(limit.ranks = c(lcl.rank, ucl.rank), ci.limits = ci.limits, 
        type = ci.type, method = "exact", conf.level = conf.level, 
        sample.size = n)
    ret.obj
}

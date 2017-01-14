ci.qnpar.interpolate <-
function (x, n, p, lb = -Inf, ub = Inf, ci.type = "two-sided", 
    approx.conf.level = 0.95) 
{
    approx.alpha <- 1 - approx.conf.level
    switch(ci.type, `two-sided` = {
        max.conf.level <- pbinom(n - 1, n, p) - pbinom(0, n, 
            p)
        if (max.conf.level < approx.conf.level) {
            stop(paste("Minimum coverage of", approx.conf.level, 
                "is not possible with the given sample size."))
        }
        limits.over.list <- ci.qnpar.exact(x = x, n = n, p = p, 
            ci.type = "two-sided", approx.conf.level = approx.conf.level, 
            min.coverage = TRUE)
        limit.ranks.over <- limits.over.list$limit.ranks
        if (diff(limit.ranks.over) == 1) {
            stop(paste("Coverage obtained with limit ranks differing by 1,", 
                "therefore not possible to implement ci.method='interpolate'."))
        }
        ci.limits.over <- limits.over.list$ci.limits
        limit.ranks.under <- limit.ranks.over + c(1, -1)
        conf.level.under <- pbinom(limit.ranks.under[2] - 1, 
            n, p) - pbinom(limit.ranks.under[1] - 1, n, p)
        while (conf.level.under >= approx.conf.level) {
            limit.ranks.over <- limit.ranks.under
            limit.ranks.under <- limit.ranks.over + c(1, -1)
            if (diff(limit.ranks.under) < 1) {
                stop(paste("Coverage obtained with limit ranks differing by 1,", 
                  "therefore not possible to implement ci.method='interpolate'."))
            }
            conf.level.under <- pbinom(limit.ranks.under[2] - 
                1, n, p) - pbinom(limit.ranks.under[1] - 1, n, 
                p)
        }
        ao2 <- approx.alpha/2
        lambda.lower <- nyblom.lambda(beta = ao2, p = p, r = limit.ranks.over[1], 
            n = n)
        lambda.upper <- nyblom.lambda(beta = 1 - ao2, p = p, 
            r = limit.ranks.under[2], n = n)
        lcl <- (1 - lambda.lower) * x[limit.ranks.over[1]] + 
            lambda.lower * x[limit.ranks.under[1]]
        ucl <- (1 - lambda.upper) * x[limit.ranks.under[2]] + 
            lambda.upper * x[limit.ranks.over[2]]
    }, lower = {
        max.conf.level <- 1 - pbinom(0, n, p)
        if (max.conf.level < approx.conf.level) {
            stop(paste("Minimum coverage of", approx.conf.level, 
                "is not possible with the given sample size."))
        }
        limits.over.list <- ci.qnpar.exact(x = x, n = n, p = p, 
            ci.type = "lower", approx.conf.level = approx.conf.level, 
            min.coverage = TRUE)
        limit.ranks.over <- limits.over.list$limit.ranks
        if (limit.ranks.over[1] == n) {
            stop(paste("Coverage obtained with lower limit rank = n,", 
                "therefore not possible to implement ci.method='interpolate'."))
        }
        ci.limits.over <- limits.over.list$ci.limits
        limit.ranks.under <- limit.ranks.over + 1
        conf.level.under <- 1 - pbinom(limit.ranks.under[1] - 
            1, n, p)
        while (conf.level.under >= approx.conf.level) {
            limit.ranks.over <- limit.ranks.under
            limit.ranks.under <- limit.ranks.over + 1
            if (limit.ranks.under[1] == n) {
                stop(paste("Coverage obtained with lower limit rank = n,", 
                  "therefore not possible to implement ci.method='interpolate'."))
            }
            conf.level.under <- 1 - pbinom(limit.ranks.under[1] - 
                1, n, p)
        }
        lambda.lower <- nyblom.lambda(beta = approx.alpha, p = p, 
            r = limit.ranks.over[1], n = n)
        lcl <- (1 - lambda.lower) * x[limit.ranks.over[1]] + 
            lambda.lower * x[limit.ranks.under[1]]
        ucl <- ub
    }, upper = {
        max.conf.level <- pbinom(n - 1, n, p)
        if (max.conf.level < approx.conf.level) {
            stop(paste("Minimum coverage of", approx.conf.level, 
                "is not possible with the given sample size."))
        }
        limits.over.list <- ci.qnpar.exact(x = x, n = n, p = p, 
            ci.type = "upper", approx.conf.level = approx.conf.level, 
            min.coverage = TRUE)
        limit.ranks.over <- limits.over.list$limit.ranks
        if (limit.ranks.over[2] == 1) {
            stop(paste("Coverage obtained with upper limit rank = 1,", 
                "therefore not possible to implement ci.method='interpolate'."))
        }
        ci.limits.over <- limits.over.list$ci.limits
        limit.ranks.under <- limit.ranks.over - 1
        conf.level.under <- pbinom(limit.ranks.under[2] - 1, 
            n, p)
        while (conf.level.under >= approx.conf.level) {
            limit.ranks.over <- limit.ranks.under
            limit.ranks.under <- limit.ranks.over - 1
            if (limit.ranks.under[2] == 1) {
                stop(paste("Coverage obtained with upper limit rank = 1,", 
                  "therefore not possible to implement ci.method='interpolate'."))
            }
            conf.level.under <- pbinom(limit.ranks.under[2] - 
                1, n, p)
        }
        lambda.upper <- nyblom.lambda(beta = 1 - approx.alpha, 
            p = p, r = limit.ranks.under[2], n = n)
        ucl <- (1 - lambda.upper) * x[limit.ranks.under[2]] + 
            lambda.upper * x[limit.ranks.over[2]]
        lcl <- lb
    })
    ci.limits <- c(lcl, ucl)
    names(ci.limits) <- c("LCL", "UCL")
    ret.obj <- list(limit.ranks = rbind(limit.ranks.over, limit.ranks.under), 
        ci.limits = ci.limits, type = ci.type, method = "interpolate (Nyblom, 1992)", 
        conf.level = approx.conf.level, sample.size = n)
    ret.obj
}

ci.qnpar.normal.approx <-
function (x, n, p, lb = -Inf, ub = Inf, ci.type = "two-sided", 
    approx.conf.level = 0.95) 
{
    approx.alpha <- 1 - approx.conf.level
    vec <- ci.normal.approx(theta.hat = n * p, sd.theta.hat = sqrt(n * 
        p * (1 - p)), n = n, df = n - 1, ci.type = ci.type, alpha = approx.alpha)$limits
    if (p < 0.5 && ci.type == "lower") {
        vec[1] <- ceiling(vec[1])
    }
    else if (p > 0.5 && ci.type == "upper") {
        vec[2] <- floor(vec[2])
    }
    else {
        vec[1] <- floor(vec[1])
        vec[2] <- ceiling(vec[2])
    }
    switch(ci.type, `two-sided` = {
        lcl.rank <- max(1, vec[1])
        ucl.rank <- min(n, vec[2])
        if (ucl.rank < n && ((pbinom(ucl.rank, n, p) - pbinom(lcl.rank - 
            1, n, p)) <= approx.conf.level)) ucl.rank <- ucl.rank + 
            1
        if (lcl.rank > 1 && ((pbinom(ucl.rank - 1, n, p) - pbinom(lcl.rank - 
            2, n, p)) <= approx.conf.level)) lcl.rank <- lcl.rank - 
            1
        conf.level <- pbinom(ucl.rank - 1, n, p) - pbinom(lcl.rank - 
            1, n, p)
        lcl <- x[lcl.rank]
        ucl <- x[ucl.rank]
    }, lower = {
        lcl.rank <- max(1, vec[1])
        ucl.rank <- NA
        if (lcl.rank > 1 && ((1 - pbinom(lcl.rank - 2, n, p)) <= 
            approx.conf.level)) lcl.rank <- lcl.rank - 1
        conf.level <- 1 - pbinom(lcl.rank - 1, n, p)
        lcl <- x[lcl.rank]
        ucl <- ub
    }, upper = {
        lcl.rank <- NA
        ucl.rank <- min(n, vec[2])
        if (ucl.rank < n && (pbinom(ucl.rank, n, p) <= approx.conf.level)) ucl.rank <- ucl.rank + 
            1
        conf.level <- pbinom(ucl.rank - 1, n, p)
        lcl <- lb
        ucl <- x[ucl.rank]
    })
    ci.limits <- c(lcl, ucl)
    names(ci.limits) <- c("LCL", "UCL")
    ret.obj <- list(limit.ranks = c(lcl.rank, ucl.rank), ci.limits = ci.limits, 
        type = ci.type, method = "normal.approx", conf.level = conf.level, 
        sample.size = n)
    ret.obj
}

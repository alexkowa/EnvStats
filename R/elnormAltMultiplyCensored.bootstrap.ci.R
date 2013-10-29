elnormAltMultiplyCensored.bootstrap.ci <-
function (x, censored, N, cen.levels, K, c.vec, n.cen, censoring.side, 
    est.fcn, ci.type, conf.level, n.bootstraps, use.acc.con, 
    obs.mean, ...) 
{
    boot.vec <- numeric(n.bootstraps)
    too.few.obs.count <- 0
    no.cen.obs.count <- 0
    for (i in 1:n.bootstraps) {
        index <- sample(N, replace = TRUE)
        new.x <- x[index]
        new.censored <- censored[index]
        new.n.cen <- sum(new.censored)
        if ((N - new.n.cen) < 2) {
            too.few.obs.count <- too.few.obs.count + 1
            i <- i - 1
            next
        }
        if (new.n.cen == 0) {
            boot.vec[i] <- elnormAlt(new.x)$parameters[1]
            no.cen.obs.count <- no.cen.obs.count + 1
        }
        else {
            new.x.cen <- new.x[new.censored]
            new.c.vec <- table(new.x.cen)
            new.cen.levels <- sort(unique(new.x.cen))
            new.K <- length(new.cen.levels)
            boot.vec[i] <- do.call(est.fcn, list(x = new.x, censored = new.censored, 
                N = N, cen.levels = new.cen.levels, K = new.K, 
                c.vec = new.c.vec, n.cen = new.n.cen, censoring.side = censoring.side, 
                ci = FALSE, ...))$parameters[1]
        }
    }
    alpha <- 1 - conf.level
    ci.limits <- quantile(boot.vec, probs = c(alpha/2, 1 - alpha/2))
    zao2 <- qnorm(alpha/2)
    z0 <- qnorm(sum(boot.vec <= obs.mean)/n.bootstraps)
    if (use.acc.con) {
        jack.vec <- elnormAltMultiplyCensored.jackknife(x = x, 
            censored = censored, N = N, cen.levels = cen.levels, 
            K = K, c.vec = c.vec, n.cen = n.cen, censoring.side = censoring.side, 
            est.fcn = est.fcn, ci.type = ci.type, conf.level = conf.level, 
            ...)
        num <- sum(as.vector(scale(jack.vec, scale = FALSE))^3)
        denom <- 6 * (((length(jack.vec) - 1) * var(jack.vec))^(3/2))
        a <- num/denom
    }
    else a <- 0
    alpha1 <- pnorm(z0 + (z0 + zao2)/(1 - a * (z0 + zao2)))
    alpha2 <- pnorm(z0 + (z0 - zao2)/(1 - a * (z0 - zao2)))
    ci.limits <- c(ci.limits, quantile(boot.vec, probs = c(alpha1, 
        alpha2)))
    names(ci.limits) <- c("LCL", "UCL", "BCLCL", "BCUCL")
    ret.obj <- list(name = "Confidence", parameter = "mean", 
        limits = ci.limits, type = ci.type, method = "Bootstrap", 
        conf.level = 1 - alpha, n.bootstraps = n.bootstraps, 
        too.few.obs.count = too.few.obs.count, no.cen.obs.count = no.cen.obs.count, 
        acceleration.constant.used = use.acc.con)
    oldClass(ret.obj) <- "intervalEstimateCensored"
    ret.obj
}

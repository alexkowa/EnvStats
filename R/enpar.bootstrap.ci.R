enpar.bootstrap.ci <-
function (x, est.fcn, ci.type, conf.level, n.bootstraps, obs.mean, obs.se.mean, 
    seed)
{
    N <- length(x)
    boot.vec.mean <- numeric(n.bootstraps)
    boot.vec.t <- numeric(n.bootstraps)
    if(!is.null(seed)) {
        set.seed(seed)
    }
    for (i in 1:n.bootstraps) {
        index <- sample(N, replace = TRUE)
        new.x <- x[index]
        params <- do.call(est.fcn, list(x = new.x))
        mu.hat <- params["mean"]
        boot.vec.mean[i] <- mu.hat
        boot.vec.t[i] <- (mu.hat - obs.mean)/params["se.mean"]
    }
    alpha <- 1 - conf.level
    if (ci.type == "two-sided")
        alpha <- alpha/2
    ci.limits.pct <- switch(ci.type, 
        "two-sided" = quantile(boot.vec.mean, probs = c(alpha, 1 - alpha)), 
        lower = c(quantile(boot.vec.mean, probs = alpha), Inf), 
        upper = c(0, quantile(boot.vec.mean, probs = conf.level))
    )
    compute.bca <- N >= 3
    if (compute.bca) {
        za <- qnorm(alpha)
        z0 <- qnorm(sum(boot.vec.mean <= obs.mean)/n.bootstraps)
        jack.vec <- enpar.jackknife(x = x)
        num <- sum(as.vector(scale(jack.vec, scale = FALSE))^3)
        denom <- 6 * (((length(jack.vec) - 1) * var(jack.vec))^(3/2))
        a <- num/denom
        ci.limits.bca <- switch(ci.type, "two-sided" = {
            alpha1 <- pnorm(z0 + (z0 + za)/(1 - a * (z0 + za)))
            alpha2 <- pnorm(z0 + (z0 - za)/(1 - a * (z0 - za)))
            quantile(boot.vec.mean, probs = c(alpha1, alpha2))
        }, lower = {
            alpha1 <- pnorm(z0 + (z0 + za)/(1 - a * (z0 + za)))
            c(quantile(boot.vec.mean, probs = alpha1), Inf)
        }, upper = {
            alpha2 <- pnorm(z0 + (z0 - za)/(1 - a * (z0 - za)))
            c(0, quantile(boot.vec.mean, probs = alpha2))
        })
    }
    else ci.limits.bca <- switch(ci.type, "two-sided" = 
        c(NA, NA), lower = c(NA, Inf), upper = c(0, NA))
    ci.limits.t <- switch(ci.type, 
        "two-sided" = {
            t.quantiles <- quantile(boot.vec.t, probs = c(1 - alpha,
                alpha))
            c(obs.mean - t.quantiles[1] * obs.se.mean, obs.mean -
                t.quantiles[2] * obs.se.mean)
        }, 
        lower = {
            t.quantiles <- quantile(boot.vec.t, probs = 1 - alpha)
            c(obs.mean - t.quantiles * obs.se.mean, Inf)
        }, 
        upper = {
            t.quantiles <- quantile(boot.vec.t, probs = alpha)
            c(0, obs.mean - t.quantiles * obs.se.mean)
        })
    ci.limits <- c(ci.limits.pct, ci.limits.bca, ci.limits.t)
    names(ci.limits) <- c("Pct.LCL", "Pct.UCL", "BCa.LCL", "BCa.UCL",
        "t.LCL", "t.UCL")
    ret.obj <- list(name = "Confidence", parameter = "mean",
        limits = ci.limits, type = ci.type, method = "Bootstrap",
        conf.level = conf.level, sample.size = N, n.bootstraps = n.bootstraps)
    oldClass(ret.obj) <- "intervalEstimate"
    ret.obj
}

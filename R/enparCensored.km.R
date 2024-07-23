enparCensored.km <-
function (x, censored, censoring.side, correct.se, ci, ci.type = "two-sided", 
    conf.level = 0.95, pivot.statistic = "t", ci.sample.size = "Total")
{
    ppoints.list <- ppointsCensored(x = x, censored = censored, 
        censoring.side = censoring.side, prob.method = "kaplan-meier")
    ord.stats <- ppoints.list$Order.Statistics
    F.x <- ppoints.list$Cumulative.Probabilities
    Censored <- ppoints.list$Censored
    N <- length(ord.stats)
    n.cen <- sum(Censored)
    n.not.cen <- N - n.cen
    ord.stats.no.cen <- ord.stats[!Censored]
    F.x.no.cen <- F.x[!Censored]
    count <- rle(ord.stats.no.cen)
    m <- count$lengths
    y <- count$values
    F.y <- F.x.no.cen[match(y, ord.stats.no.cen)]
    diff.F.y <- diff(c(0, F.y))
    mu.hat <- sum(y * diff.F.y)
    sigma.hat <- sqrt(sum((y - mu.hat)^2 * diff.F.y))
    if(censoring.side == "left") {
        r <- sapply(y[-1], function(x) sum(ord.stats <= x))
        F.y.times.diff.y <- F.y[-length(F.y)] * diff(y)
        A <- cumsum(F.y.times.diff.y)
        se.muhat <- sqrt(sum(A^2 * m[-1]/(r * (r - m[-1]))))
    }
    else {
        y <- ord.stats.no.cen
        F.y <- F.x.no.cen
        r <- (1:N)[!Censored][-n.not.cen]
        y <- c(0, y)
        F.y <- c(0, F.y)
        S.y <- 1 - F.y
        diff.y <- diff(y)
        S.y.times.diff.y <- S.y[-length(S.y)] * diff.y
        A <- rev(cumsum(rev(S.y.times.diff.y[-1])))
        se.muhat <- sqrt(sum(A^2/((N - r) * (N - r + 1))))
    }
    if (correct.se) 
        se.muhat <- sqrt(n.not.cen/(n.not.cen - 1)) * se.muhat
    parameters <- c(mean = mu.hat, sd = sigma.hat, se.mean = se.muhat)
    if (ci) {
        ci.ub <- Inf
        if (pivot.statistic == "z") {
            ci.sample.size <- N
        }
        else {
          ci.sample.size <- ifelse(ci.sample.size == "Total", N, n.not.cen)
        }
        ci.obj <- ci.normal.approx(theta.hat = parameters["mean"], 
            sd.theta.hat = parameters["se.mean"], n = ci.sample.size, 
            df = ci.sample.size - 1, ci.type = ci.type, alpha = 1 - 
                conf.level, lb = 0, ub = ci.ub, test.statistic = pivot.statistic)
        ci.obj$parameter <- "mean"
        return(list(parameters = parameters, ci.obj = ci.obj))
    }
    else return(list(parameters = parameters))
}

evNormOrdStatsScalar <-
function (r = 1, n = 1, method = "royston", lower = -9, inc = 0.025, 
    warn = TRUE, alpha = 3/8, nmc = 2000, conf.level = 0.95, 
    seed = 47, approximate = NULL) 
{
    if (any(length.list(r, n) != 1) || r != trunc(r) || n != 
        trunc(n) || r < 1 || n < 1 || r > n) 
        stop(paste("'r' and 'n' must be positive integers,", 
            "and 'r' must be between 1 and 'n'"))
    if (!is.null(approximate) && missing(method)) {
        method <- ifelse(approximate, "blom", "royston")
    }
    else {
        method <- match.arg(method, c("royston", "blom", "mc"))
    }
    if (method == "royston") {
        if (!is.numeric(lower) || length(lower) != 1 || lower > 
            -9) 
            stop("The value of 'lower' show be less than or equal to -9")
        if (!is.numeric(inc) || length(inc) != 1 || inc < .Machine$double.eps || 
            inc > 0.025) 
            stop(paste("The value of 'inc' should be", "less than or equal to 0.025 and greater than .Machine$double.eps"))
        if (n > 2000 && warn) 
            warning(paste("The 'royston' method has not been validated", 
                "for sample sizes greater than 2000", "using the default value of inc = 0.025.", 
                "You may want to make the value of 'inc' less than 0.025."))
    }
    if (method == "blom") {
        if (!is.numeric(alpha) || length(alpha) != 1 || alpha < 
            0 || alpha > 0.5) 
            stop("'alpha' must be a numeric scalar between 0 and 0.5")
    }
    if (method == "mc") {
        if (!is.numeric(nmc) || length(nmc) != 1 || nmc != trunc(nmc) || 
            nmc < 100) 
            stop("'nmc' must be an integer greater than or equal to 100")
        if (!is.numeric(conf.level) || length(conf.level) != 
            1 || conf.level <= 0 || conf.level >= 1) 
            stop("'conf.level' must be a numeric scalar between 0 and 1")
        if (!is.numeric(seed) || length(seed) != 1 || seed != 
            trunc(seed) || seed < -(2^31 - 1) || seed > (2^31 - 
            1)) 
            stop("'seed' must be an integer between -(2^31 - 1) and (2^31 - 1)")
    }
    if (is.odd(n) && r == (n + 1)/2) {
        ev <- 0
    }
    else {
        ev <- switch(method, royston = {
            Int <- function(x, r, n) {
                x * exp(log(r) + lchoose(n, r) + (r - 1) * log(pnorm(x)) + 
                  (n - r) * log(1 - pnorm(x)) + log(dnorm(x)))
            }
            x.vec <- seq(lower, -lower, by = inc)
            if (r > floor(n/2)) {
                -sum(Int(x = x.vec, r = n - r + 1, n = n)) * 
                  inc
            } else {
                sum(Int(x = x.vec, r = r, n = n)) * inc
            }
        }, blom = {
            qnorm((r - alpha)/(n - 2 * alpha + 1))
        }, mc = {
            set.seed(seed)
            mc.vec <- numeric(nmc)
            for (i in 1:nmc) {
                mc.vec[i] <- sort(rnorm(n))[r]
            }
            dum.list <- enorm(mc.vec, ci = TRUE, conf.level = conf.level)
            ev <- dum.list$parameters["mean"]
            names(ev) <- NULL
            ci <- dum.list$interval$limits
            names(ci) <- paste(round(100 * dum.list$interval$conf.level), 
                "%", names(ci), sep = "")
            attr(ev, "confint") <- ci
            attr(ev, "nmc") <- nmc
            ev
        })
    }
    ev
}

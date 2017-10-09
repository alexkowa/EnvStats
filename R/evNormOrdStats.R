evNormOrdStats <-
function (n = 1, method = "royston", lower = -9, inc = 0.025, 
    warn = TRUE, alpha = 3/8, nmc = 2000, seed = 47, approximate = NULL) 
{
    if (length(n) > 1 || n != trunc(n) || n < 1) 
        stop("'n' must be a positive integer")
    if (n == 1) 
        return(0)
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
        if (!is.numeric(seed) || length(seed) != 1 || seed != 
            trunc(seed) || seed < -(2^31 - 1) || seed > (2^31 - 
            1)) 
            stop("'seed' must be an integer between -(2^31 - 1) and (2^31 - 1)")
    }
    Z <- switch(method, blom = {
        qnorm(ppoints(n, a = alpha))
    }, royston = {
        Z <- numeric(n)
        mid.point <- ifelse(ion <- is.odd(n), (n + 1)/2, n/2)
        if (ion) {
            for (i in 1:(mid.point - 1)) {
                Z[i] <- evNormOrdStatsScalar(i, n, method = "royston", 
                  lower = lower, inc = inc, warn = FALSE)
            }
            Z[(mid.point + 1):n] <- -Z[(mid.point - 1):1]
            Z[mid.point] <- 0
        } else {
            for (i in 1:mid.point) {
                Z[i] <- evNormOrdStatsScalar(i, n, method = "royston", 
                  lower = lower, inc = inc, warn = FALSE)
            }
            Z[(mid.point + 1):n] <- -Z[mid.point:1]
        }
        Z
    }, mc = {
        set.seed(seed)
        mid.point <- ifelse(ion <- is.odd(n), (n + 1)/2, n/2)
        if (ion) {
            num <- mid.point - 1
            Z <- numeric(num)
            for (i in 1:nmc) {
                Z <- Z + sort(rnorm(n))[1:num]/nmc
            }
            Z <- c(Z, 0, -rev(Z))
        } else {
            Z <- numeric(mid.point)
            for (i in 1:nmc) {
                Z <- Z + sort(rnorm(n))[1:mid.point]/nmc
            }
            Z <- c(Z, -rev(Z))
        }
        Z
    })
    Z
}

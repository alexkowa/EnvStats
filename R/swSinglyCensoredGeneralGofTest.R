swSinglyCensoredGeneralGofTest <-
function (x, censored, censoring.side = "left", distribution, 
    est.arg.list) 
{
    if (!is.vector(x, mode = "numeric") || is.factor(x)) 
        stop("'x' must be a numeric vector")
    if (!((is.vector(censored, mode = "numeric") && !is.factor(censored)) || 
        is.vector(censored, mode = "logical"))) 
        stop("'censored' must be a logical or numeric vector")
    if (length(censored) != length(x)) 
        stop("'censored' must be the same length as 'x'")
    data.name <- deparse(substitute(x))
    censoring.name <- deparse(substitute(censored))
    if ((bad.obs <- sum(!(ok <- is.finite(x) & is.finite(as.numeric(censored))))) > 
        0) {
        is.not.finite.warning(x)
        is.not.finite.warning(as.numeric(censored))
        x <- x[ok]
        censored <- censored[ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' and 'censored' removed."))
    }
    if (is.numeric(censored)) {
        if (!all(censored == 0 | censored == 1)) 
            stop(paste("When 'censored' is a numeric vector, all values of", 
                "'censored' must be 0 (not censored) or 1 (censored)."))
        censored <- as.logical(censored)
    }
    est.fcn <- paste("e", distribution, "Censored", sep = "")
    est.list <- do.call(est.fcn, c(list(x = x, censored = censored, 
        censoring.side = censoring.side), est.arg.list))
    params <- est.list$parameters
    Z <- do.call(paste("p", distribution, sep = ""), c(list(q = x), 
        as.list(params)))
    Y <- qnorm(Z)
    ret.list <- swSinglyCensoredGofTest(Y, censored)
    ret.list$data <- x
    ret.list$data.name <- data.name
    ret.list$censored <- censored
    ret.list$censoring.name <- censoring.name
    ret.list$censoring.levels <- est.list$censoring.levels
    ret.list$bad.obs <- bad.obs
    ret.list$dist.abb <- distribution
    ret.list$distribution <- EnvStats::Distribution.df[distribution, 
        "Name"]
    ret.list$distribution.parameters <- params
    ret.list$n.param.est <- length(params)
    ret.list$estimation.method <- est.list$method
    sep.string <- paste("\n", space(33), sep = "")
    ret.list$alternative <- paste("True cdf does not equal the", 
        paste(ret.list$distribution, "Distribution."), sep = sep.string)
    ret.list$method <- paste("Shapiro-Wilk GOF", "(Singly Censored Data)", 
        "Based on Chen & Balakrisnan (1995)", sep = sep.string)
    ret.list
}

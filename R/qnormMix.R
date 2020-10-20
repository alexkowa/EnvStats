qnormMix <-
function (p, mean1 = 0, sd1 = 1, mean2 = 0, sd2 = 1, p.mix = 0.5)
{
    if (any(p < 0 | p > 1))
        stop("All non-missing values of 'p' must be between 0 and 1.")
    if (any(c(sd1, sd2) < .Machine$double.eps))
        stop("All non-missing values of 'sd1' and 'sd2' must be positive.")
    if (any(p.mix < 0 | p.mix > 1))
        stop("All non-missing values of 'p.mix' must be between 0 and 1.")
    sapply(p, function(y)
        uniroot(function(x) pnormMix(x,mean1,sd1,mean2,sd2,p.mix)-y,
            interval = range(qnorm(y,mean1,sd1),qnorm(y,mean2,sd2)),
            tol = 10^{-16})$root)
}

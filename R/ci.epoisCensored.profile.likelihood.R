ci.epoisCensored.profile.likelihood <-
function (x, censored, censoring.side, lambda.mle, conf.level, 
    LCL.start, UCL.start) 
{
    loglik.at.mle <- loglikCensored(theta = lambda.mle, x = x, 
        censored = censored, censoring.side = censoring.side, 
        distribution = "pois")
    fcn <- function(CL, loglik.at.mle, x, censored, censoring.side) {
        (2 * (loglik.at.mle - loglikCensored(theta = CL, x = x, 
            censored = censored, censoring.side = censoring.side, 
            distribution = "pois")) - qchisq(conf.level, df = 1))^2
    }
    LCL <- nlminb(start = LCL.start, objective = fcn, lower = .Machine$double.eps, 
        upper = lambda.mle, loglik.at.mle = loglik.at.mle, x = x, 
        censored = censored, censoring.side = censoring.side)$par
    UCL <- nlminb(start = UCL.start, objective = fcn, lower = lambda.mle, 
        loglik.at.mle = loglik.at.mle, x = x, censored = censored, 
        censoring.side = censoring.side)$par
    ci.limits <- c(LCL, UCL)
    names(ci.limits) <- c("LCL", "UCL")
    interval <- list(name = "Confidence", parameter = "lambda", 
        limits = ci.limits, type = "two-sided", method = "Profile Likelihood", 
        conf.level = conf.level)
    oldClass(interval) <- "intervalEstimate"
    interval
}

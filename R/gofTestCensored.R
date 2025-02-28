gofTestCensored <-
function (x, censored, censoring.side = "left", test = "sf", 
    distribution = "norm", est.arg.list = NULL, prob.method = "hirsch-stedinger", 
    plot.pos.con = 0.375, keep.data = TRUE, data.name = NULL, 
    censoring.name = NULL) 
{
    distribution <- check.distribution.args(distribution, check.params = FALSE)$dist.abb
    if (!is.vector(x, mode = "numeric")) 
        stop("'x' must be a numeric vector")
    if (is.null(data.name)) 
        data.name <- deparse(substitute(x))
    if (!is.vector(censored, mode = "numeric") & !is.vector(censored, 
        mode = "logical")) 
        stop("'censored' must be a logical or numeric vector")
    if (length(censored) != length(x)) 
        stop("'censored' must be the same length as 'x'")
    if (is.numeric(censored)) {
        index <- is.finite(censored)
        if (!all(is.element(censored[index], 0:1))) 
            stop(paste("When 'censored' is a numeric vector, all non-missing values of", 
                "'censored' must be 0 (not censored) or 1 (censored)."))
    }
    if (is.null(censoring.name)) 
        censoring.name <- deparse(substitute(censored))
    censoring.side <- match.arg(censoring.side, c("left", "right"))
    test <- match.arg(test, c("sw", "sf", "ppcc"))
    if (test == "ppcc") 
        test <- "ppccNorm"
    if ((bad.obs <- sum(!(ok <- is.finite(x) & is.finite(as.numeric(censored))))) > 
        0) {
        is.not.finite.warning(x)
        is.not.finite.warning(as.numeric(censored))
        x <- x[ok]
        censored <- censored[ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' and/or 'censored' removed."))
    }
    if (is.numeric(censored)) 
        censored <- as.logical(censored)
    n.cen <- sum(censored)
    if (n.cen == 0) {
        warning(paste("No censored values indicated by 'censored',", 
            "so the function 'gofTest' was called."))
        ret.list <- gofTest(y = x, test = test, distribution = distribution, 
            est.arg.list = est.arg.list)
        ret.list$data.name <- data.name
        ret.list$bad.obs <- bad.obs
        return(ret.list)
    }
    x.no.cen <- x[!censored]
    if (length(unique(x.no.cen)) < 2) 
        stop("'x' must contain at least 2 non-missing, uncensored, distinct values.")
    if (any(distribution == c("lnorm", "lnormAlt")) && any(x <= 
        0)) 
        stop("All non-missing values of 'x' must be positive for a lognormal distribution")
    multiple <- TRUE
    T.vec <- unique(x[censored])
    if (length(T.vec) == 1) {
        if (censoring.side == "left") {
            if (T.vec <= min(x.no.cen)) 
                multiple <- FALSE
        }
        else {
            if (T.vec >= max(x.no.cen)) 
                multiple <- FALSE
        }
    }
    if (multiple) {
        if (test == "sw") 
            stop(paste("Shapiro-Wilk test not available for multiply censored data.", 
                "Set test='sf' or test='ppcc'."))
        prob.method <- match.arg(prob.method, c("hirsch-stedinger", 
            "michael-schucany", "modified kaplan-meier", "nelson"))
        if (censoring.side == "left" & prob.method == "nelson") 
            stop("Nelson Method not available when censoring.side='left'")
        if (censoring.side == "right" & prob.method == "modified kaplan-meier") 
            stop("Modified Kaplan-Meier Method not available when censoring.side='right'")
        if (!is.vector(plot.pos.con, mode = "numeric") || length(plot.pos.con) != 
            1 || plot.pos.con < 0 || plot.pos.con > 1) 
            stop("'plot.pos.con' must be a numeric scalar between 0 and 1")
        censoring.type <- "MultiplyCensored"
    }
    else {
        censoring.type <- "SinglyCensored"
    }
    test.name <- paste(test, censoring.type, "GofTest", sep = "")
    if (!(distribution %in% c("norm", "lnorm", "lnormAlt"))) {
        efcn <- paste("e", distribution, sep = "")
        if (EnvStats::Distribution.df[distribution, "Type"] != 
            "Continuous" || !exists(efcn, where = "package:EnvStats")) 
            stop(paste("When the argument distribution is not equal to", 
                "'norm', 'lnorm', or 'lnormAlt',", "it must indicate a continuous distribution, and", 
                "there must exist an associated function", "to estimate the parameters in the presence of censored data.", 
                "See the help file for 'EnvStats::Distribution.df' for more information."))
        test.name <- paste(test, censoring.type, "GeneralGofTest", 
            sep = "")
    }
    arg.list <- list(x = x, censored = censored, censoring.side = censoring.side, 
        distribution = distribution, est.arg.list = est.arg.list)
    if (multiple) 
        arg.list <- c(arg.list, list(prob.method = prob.method, 
            plot.pos.con = plot.pos.con))
    ret.list <- do.call(test.name, args = arg.list)
    if (!keep.data) {
        ret.list <- ret.list[!(names(ret.list) %in% c("data", 
            "censored"))]
        oldClass(ret.list) <- "gofCensored"
    }
    ret.list$data.name <- data.name
    ret.list$censoring.name <- censoring.name
    if (any(bad.obs > 0)) 
        ret.list$bad.obs <- bad.obs
    else ret.list$bad.obs <- NULL
    ret.list
}

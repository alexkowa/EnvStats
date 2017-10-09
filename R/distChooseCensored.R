distChooseCensored <-
function (x, censored, censoring.side = "left", alpha = 0.05, 
    method = "sf", choices = c("norm", "gamma", "lnorm"), est.arg.list = NULL, 
    prob.method = "hirsch-stedinger", plot.pos.con = 0.375, warn = TRUE, 
    keep.data = TRUE, data.name = NULL, censoring.name = NULL) 
{
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
    if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 
        0 || alpha >= 1) 
        stop("'alpha' must be a numeric scalar between 0 and 1")
    method <- match.arg(method, c("sw", "sf", "ppcc", "proucl"))
    if ((bad.obs <- sum(!(ok <- is.finite(x) & is.finite(as.numeric(censored))))) > 
        0) {
        x <- x[ok]
        censored <- censored[ok]
        if (warn) {
            is.not.finite.warning(x)
            is.not.finite.warning(as.numeric(censored))
            warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' and/or 'censored' removed."))
        }
    }
    if (is.numeric(censored)) 
        censored <- as.logical(censored)
    n.cen <- sum(censored)
    if (n.cen == 0) {
        warning(paste("No censored values indicated by 'censored',", 
            "so the function 'distChoose' was called."))
        ret.list <- distChoose(y = x, alpha = alpha, method = method, 
            choices = choices, est.arg.list = est.arg.list, warn = warn, 
            keep.data = keep.data, data.name = data.name)
        ret.list$bad.obs <- bad.obs
        return(ret.list)
    }
    if (method == "proucl") {
        ret.list <- prouclDistChooseCensored(x = x, censored = censored, 
            censoring.side = censoring.side, alpha = alpha, data.name = data.name, 
            censoring.name = censoring.name)
    }
    else {
        ret.list <- envstatsDistChooseCensored(x = x, censored = censored, 
            censoring.side = censoring.side, alpha = alpha, method = method, 
            choices = choices, est.arg.list = est.arg.list, prob.method = prob.method, 
            plot.pos.con = plot.pos.con, data.name = data.name, 
            censoring.name = censoring.name)
    }
    if (keep.data) 
        ret.list <- c(ret.list, list(data = x, censored = censored))
    ret.list$data.name <- data.name
    ret.list$censoring.name <- censoring.name
    if (any(bad.obs > 0)) 
        ret.list$bad.obs <- bad.obs
    else ret.list$bad.obs <- NULL
    oldClass(ret.list) <- "distChooseCensored"
    ret.list
}

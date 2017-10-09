distChoose.default <-
function (y, alpha = 0.05, method = "sw", choices = c("norm", 
    "gamma", "lnorm"), est.arg.list = NULL, warn = TRUE, keep.data = TRUE, 
    data.name = NULL, parent.of.data = NULL, subset.expression = NULL, 
    ...) 
{
    if (is.null(data.name)) 
        data.name <- deparse(substitute(y))
    y <- as.vector(unlist(y))
    if (!is.numeric(y) | length(y) == 0) 
        stop("'y' must be non-empty and all elements of 'y' must be a numeric")
    if ((bad.obs.y <- sum(!(y.ok <- is.finite(y)))) > 0) {
        if (warn) {
            warning(paste(bad.obs.y, "observations with NA/NaN/Inf in 'y' removed."))
        }
        y <- y[y.ok]
    }
    if (length(y) == 0) 
        stop("All observations in 'y' are NA/Nan/Inf")
    if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 
        0 || alpha >= 1) 
        stop("'alpha' must be a numeric scalar between 0 and 1")
    method <- match.arg(method, c("sw", "sf", "ppcc", "proucl"))
    if (method == "proucl") {
        ret.list <- prouclDistChoose(y = y, alpha = alpha, data.name = data.name, 
            parent.of.data = parent.of.data, subset.expression = subset.expression)
    }
    else {
        ret.list <- envstatsDistChoose(y = y, alpha = alpha, 
            method = method, choices = choices, est.arg.list = est.arg.list, 
            data.name = data.name, parent.of.data = parent.of.data, 
            subset.expression = subset.expression)
    }
    if (keep.data) 
        ret.list <- c(ret.list, list(data = y))
    ret.list$data.name <- data.name
    if (!is.null(parent.of.data)) 
        ret.list$parent.of.data <- parent.of.data
    if (!is.null(subset.expression)) 
        ret.list$subset.expression <- subset.expression
    if (bad.obs.y > 0) 
        ret.list$bad.obs <- bad.obs.y
    oldClass(ret.list) <- "distChoose"
    ret.list
}

predict.lm <-
function (object, newdata, se.fit = FALSE, scale = NULL, df = Inf, 
    interval = c("none", "confidence", "prediction"), level = 0.95, 
    type = c("response", "terms"), terms = NULL, na.action = na.pass, 
    pred.var = NULL, weights = 1, ...) 
{
    interval <- match.arg(interval)
    type <- match.arg(type)
    arg.list <- list(object = object)
    if (!missing(newdata)) 
        arg.list <- list(object = object, newdata = newdata)
    else arg.list <- list(object = object)
    arg.list <- c(arg.list, se.fit = se.fit, scale = scale, df = df, 
        interval = interval, level = level, type = type, terms = terms, 
        na.action = na.action)
    if (!missing(pred.var)) 
        arg.list <- c(arg.list, pred.var = pred.var)
    arg.list <- c(arg.list, weights = weights)
    if (length(list(...))) 
        arg.list <- c(arg.list, ...)
    ret.val <- do.call(stats::predict.lm, args = arg.list)
    if (se.fit) 
        ret.val <- c(ret.val, n.coefs = object$rank)
    ret.val
}

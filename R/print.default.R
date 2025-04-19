print.default <-
function (x, digits = NULL, quote = TRUE, na.print = NULL, print.gap = NULL, 
    right = FALSE, max = NULL, width = NULL, useSource = TRUE, 
    ...) 
{
    dc <- data.class(x)
    if(dc %in% c("htestEnvStats", "htest")) {
        if(dc == "htestEnvStats") print.htestEnvStats(x, ...)
        else print.htest(x, ...)
    }
    else {
        base::print.default(x = x, digits = digits, quote = quote, na.print = na.print, 
	print.gap = print.gap, right = right, max = max, width = width, useSource = useSource, ...)
    }
}

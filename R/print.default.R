print.default <-
function (x, ...) 
{
    dc <- data.class(x)
    if(dc %in% c("htestEnvStats", "htest")) {
        if(dc == "htestEnvStats") print.htestEnvStats(x, ...)
        else print.htest(x, ...)
    }
    else {
        base::print.default(x = x, ...)
    }
}

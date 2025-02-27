print.default <-
function (x, digits = NULL, quote = TRUE, na.print = NULL, print.gap = NULL, 
    right = FALSE, max = NULL, width = NULL, useSource = TRUE, 
    ...) 
{
    if(data.class(x) == "htest") { 
        print.htest(x, ...)
    }
    else {
        base::print.default(x = x, digits = digits, quote = quote, na.print = na.print, 
	print.gap = print.gap, right = right, max = max, width = width, useSource = useSource, ...)
    }
}

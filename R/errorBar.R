errorBar <-
function (x, y = NULL, lower, upper, incr = TRUE, bar.ends = TRUE, 
    gap = TRUE, add = FALSE, horizontal = FALSE, bar.ends.size = 1, 
    col = 1, ..., xlab = deparse(substitute(x)), xlim, ylim) 
{
    draw.null.warn <- function(draw, gap) {
        if (!any(draw)) {
            warning("Not enough room for a gap.")
            draw <- !draw
            gap <- 0
        }
        invisible(list(draw = draw, gap = gap))
    }
    if (missing(x)) 
        stop("no data for x or y")
    if (missing(y)) {
        if (missing(xlab)) 
            xlab <- "Index"
        y <- x
        x <- time(x)
    }
    n <- length(x)
    if (length(y) != n) 
        stop("length of y must equal the length of x")
    center <- if (horizontal) 
        x
    else y
    if (missing(lower)) 
        stop("you must provide lower")
    if (length(lower) > 1 && length(lower) != n) 
        stop("length of lower must be 1 or equal to the length of x")
    if (incr) 
        lower <- center - abs(lower)
    else lower <- rep(lower, length = n)
    if (any(lower >= center)) 
        warning(paste("There are values of 'lower' which are greater or equal to ", 
            if (horizontal) 
                "x"
            else "y"))
    if (missing(upper)) 
        upper <- 2 * center - lower
    else {
        if (length(upper) > 1 && length(upper) != n) 
            stop("length of upper must be 1 or equal to the length of x")
        if (incr) 
            upper <- center + upper
        else upper <- rep(upper, length = n)
    }
    if (any(upper <= center)) 
        warning(paste("There are values of 'upper' which are smaller or\nequal to ", 
            if (horizontal) 
                "x"
            else "y"))
    if (!add) {
        if (horizontal) {
            if (missing(ylim)) 
                plot(x, y, xlim = if (missing(xlim)) 
                  range(c(lower, upper), na.rm = TRUE)
                else xlim, xlab = xlab, col = col, ...)
            else plot(x, y, xlim = if (missing(xlim)) 
                range(c(lower, upper), na.rm = TRUE)
            else xlim, ylim = ylim, xlab = xlab, col = col, ...)
        }
        else {
            if (missing(xlim)) 
                plot(x, y, ylim = if (missing(ylim)) 
                  range(c(lower, upper), na.rm = TRUE)
                else ylim, xlab = xlab, col = col, ...)
            else plot(x, y, ylim = if (missing(ylim)) 
                range(c(lower, upper), na.rm = TRUE)
            else ylim, xlim = xlim, xlab = xlab, col = col, ...)
        }
    }
    col <- rep(col, length = n)
    if (horizontal) {
        if (gap) 
            gap <- 0.75 * par("cxy")[1]
        if (bar.ends) 
            size.bar <- bar.ends.size * par("cxy")[2]
        draw <- x - lower > gap
        z <- draw.null.warn(draw, gap)
        draw <- z$draw
        gap <- z$gap
        segments(lower[draw], y[draw], x[draw] - gap, y[draw], 
            col = col[draw], ...)
        if (bar.ends) 
            segments(lower[draw], y[draw] - size.bar, lower[draw], 
                y[draw] + size.bar, col = col[draw], ...)
        draw <- upper - x > gap
        z <- draw.null.warn(draw, gap)
        draw <- z$draw
        gap <- z$gap
        segments(x[draw] + gap, y[draw], upper[draw], y[draw], 
            col = col[draw], ...)
        if (bar.ends) 
            segments(upper[draw], y[draw] - size.bar, upper[draw], 
                y[draw] + size.bar, col = col[draw], ...)
    }
    else {
        if (gap) 
            gap <- 0.75 * par("cxy")[2]
        if (bar.ends) 
            size.bar <- bar.ends.size * par("cxy")[1]
        draw <- upper - y > gap
        z <- draw.null.warn(draw, gap)
        draw <- z$draw
        gap <- z$gap
        segments(x[draw], y[draw] + gap, x[draw], upper[draw], 
            col = col[draw], ...)
        if (bar.ends) 
            segments(x[draw] - size.bar, upper[draw], x[draw] + 
                size.bar, upper[draw], col = col[draw], ...)
        draw <- y - lower > gap
        z <- draw.null.warn(draw, gap)
        draw <- z$draw
        gap <- z$gap
        segments(x[draw], y[draw] - gap, x[draw], lower[draw], 
            col = col[draw], ...)
        if (bar.ends) 
            segments(x[draw] - size.bar, lower[draw], x[draw] + 
                size.bar, lower[draw], col = col[draw], ...)
    }
}

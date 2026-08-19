#' Plot Probability Density Function
#' @rawRd \alias{Probability Density}
#' @rawRd \alias{Plot Probability Density}
#' @rawRd \alias{Plot PDF}
#' @description
#' Produce a probability density function (pdf) plot for a user-specified distribution.
#' @usage
#' pdfPlot(distribution = "norm", param.list = list(mean = 0, sd = 1),
#'     left.tail.cutoff = ifelse(is.finite(supp.min), 0, 0.001),
#'     right.tail.cutoff = ifelse(is.finite(supp.max), 0, 0.001),
#'     plot.it = TRUE, add = FALSE, n.points = 1000, pdf.col = "black",
#'     pdf.lwd = 3 * par("cex"), pdf.lty = 1, curve.fill = !add,
#'     curve.fill.col = "cyan", x.ticks.at.all.x.max = 15,
#'     hist.col = ifelse(add, "black", "cyan"), density = 5,
#'     digits = .Options$digits, ..., type = "l", main = NULL, xlab = NULL,
#'     ylab = NULL, xlim = NULL, ylim = NULL)
#' @rawRd
#' \arguments{
#'   \item{distribution}{
#'   a character string denoting the distribution abbreviation.  The default value is
#'   \code{distribution="norm"}.  See the help file for \code{\link{Distribution.df}} for a
#'   list of possible distribution abbreviations.
#' }
#'   \item{param.list}{
#'   a list with values for the parameters of the distribution.  The default value is
#'   \code{param.list=list(mean=0, sd=1)}.  See the help file for
#'   \code{\link{Distribution.df}} for the names and possible values of the parameters
#'   associated with each distribution.
#' }
#'   \item{left.tail.cutoff}{
#'   a numeric scalar indicating what proportion of the left-tail of the probability
#'   distribution to omit from the plot.  For densities with a finite support minimum
#'   (e.g., \link{Lognormal}) the default value is \code{0}; for all other densities the default
#'   value is \code{0.001}.
#' }
#'   \item{right.tail.cutoff}{
#'   a scalar indicating what proportion of the right-tail of the probability
#'   distribution to omit from the plot.  For densities with a finite support maximum
#'   (e.g., \link{Binomial}) the default value is \code{0}; for all other densities the
#'   default value is \code{0.001}.
#' }
#'   \item{plot.it}{
#'   a logical scalar indicating whether to create a plot or add to the existing plot
#'   (see \code{add}) on the current graphics device.  If \code{plot.it=FALSE}, no
#'   plot is produced, but a list of \eqn{(x, y)} values is returned (see the section
#'   VALUE below). The default value is \code{plot.it=TRUE}.
#' }
#'   \item{add}{
#'   a logical scalar indicating whether to add the probability density curve to the
#'   existing plot (\code{add=TRUE}), or to create a new plot
#'   (\code{add=FALSE}; the default).  This argument is ignored if \code{plot.it=FALSE}.
#' }
#'   \item{n.points}{
#'   a numeric scalar specifying at how many evenly-spaced points the probability
#'   density function will be evaluated.  The default value is \code{n.points=1000}.
#' }
#'   \item{pdf.col}{
#'   for continuous distributions, a numeric scalar or character string determining
#'   the color of the pdf line in the plot.
#'   The default value is \code{pdf.col="black"}.  See the entry for \code{col} in the
#'   help file for \code{\link{par}} for more information.
#' }
#'   \item{pdf.lwd}{
#'   for continuous distributions, a numeric scalar determining the width of the pdf
#'   line in the plot.
#'   The default value is \code{pdf.lwd=3*par("cex")}.
#'   See the entry for \code{lwd} in the help file for \code{\link{par}}
#'   for more information.
#' }
#'   \item{pdf.lty}{
#'   for continuous distributions, a numeric scalar determining the line type of
#'   the pdf line in the plot.
#'   The default value is \code{pdf.lty=1}.  See the entry for
#'   \code{lty} in the help file for \code{\link{par}} for more information.
#' }
#'   \item{curve.fill}{
#'   for continuous distributions, a logical value indicating whether to fill in
#'   the area below the probability density curve with the color specified by
#'   \code{curve.fill.col}.
#'   The default value is \code{TRUE} unless \code{add=TRUE}.
#' }
#'   \item{curve.fill.col}{
#'   for continuous distributions, when \code{curve.fill=TRUE},
#'   a numeric scalar or character string
#'   indicating what color to use to fill in the
#'   area below the probability density curve.  The default value is
#'   \code{curve.fill.col="cyan"}.  See the entry for \code{col} in the
#'   help file for \code{\link{par}} for more information.
#' }
#'   \item{x.ticks.at.all.x.max}{
#'   a numeric scalar indicating the maximum number of ticks marks on the \eqn{x}-axis.
#'   The default value is \code{x.ticks.at.all.x.max=15}.
#' }
#'   \item{hist.col}{
#'   for discrete distributions, a numeric scalar or character string indicating
#'   what color to use to fill in the histogram if \code{add=FALSE}, or the color
#'   of the shading lines if \code{add=TRUE}.  The default is \code{"cyan"} if
#'   \code{add=FALSE} and \code{"black"} if \code{add=TRUE}.
#'   See the entry for \code{col} in the
#'   help file for \code{\link{par}} for more information.
#' }
#'   \item{density}{
#'   for discrete distributions, a scalar indicting the density of line shading for
#'   the histogram when \code{add=TRUE}.  This argument is ignored if \code{add=FALSE}.
#' }
#'   \item{digits}{
#'   a scalar indicating how many significant digits to print for the distribution
#'   parameters.  The default value is \code{digits=.Options$digits}.
#' }
#'   \item{type, main, xlab, ylab, xlim, ylim, \dots}{
#'   additional graphical parameters.  See \code{\link{plot.default}} and
#'   \code{\link{par}}).
#' }
#' }
#' @rawRd
#' \details{
#'   The \bold{\emph{probability density function (pdf)}} of a random variable \eqn{X},
#'   usually denoted \eqn{f}, is defined as:
#'   \deqn{f(x) = \frac{dF(x)}{dx} \;\;\;\;\;\; (1)}
#'   where \eqn{F} is the cumulative distribution function (cdf) of \eqn{X}.
#'   That is, \eqn{f(x)} is the derivative of the cdf
#'   \eqn{F} with respect to \eqn{x} (where this derivative exists).
#'
#'   For discrete distributions, the probability density function is simply:
#'   \deqn{f(x) = Pr(X = x) \;\;\;\;\;\; (2)}
#'   In this case, \eqn{f} is sometimes called the \bold{\emph{probability function}} or
#'   \bold{\emph{probability mass function}}.
#'
#'   The probability that the random variable \eqn{X} takes on a value in the interval
#'   \eqn{[a, b]} is simply the (Lebesgue) integral of the pdf evaluated between
#'   \eqn{a} and \eqn{b}. That is,
#'   \deqn{Pr(a \le X \le b) = \int_a^b f(x) dx \;\;\;\;\;\; (3)}
#'   For discrete distributions, Equation (3) translates to summing up the
#'   probabilities of all values in this interval:
#'   \deqn{Pr(a \le X \le b) = \sum_{x \in [a,b]} f(x) = \sum_{x \in [a,b]} Pr(X = x) \;\;\;\;\;\; (4)}
#'
#'   A \bold{\emph{probability density function (pdf) plot}} plots the values of the
#'   pdf against quantiles of the specified distribution.  Theoretical pdf plots
#'   are sometimes plotted along with \link[=epdfPlot]{empirical pdf plots}
#'   (density plots), histograms or bar graphs to visually assess whether data
#'   have a particular distribution.
#' }
#' @rawRd
#' \value{
#'   \code{pdfPlot} invisibly returns a list giving coordinates of the points
#'   that have been or would have been plotted:
#'   \item{Quantiles}{The quantiles used for the plot.}
#'   \item{Probability.Densities}{The values of the pdf associated with the quantiles.}
#' }
#' @rawRd
#' \references{
#'   Forbes, C., M. Evans, N. Hastings, and B. Peacock. (2011).  Statistical Distributions.
#'   Fourth Edition. John Wiley and Sons, Hoboken, NJ.
#'
#'   Johnson, N. L., S. Kotz, and A.W. Kemp. (1992).  \emph{Univariate
#'   Discrete Distributions, Second Edition}.  John Wiley and Sons, New York.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1994).
#'   \emph{Continuous Univariate Distributions, Volume 1}.
#'   Second Edition. John Wiley and Sons, New York.
#'
#'   Johnson, N. L., S. Kotz, and N. Balakrishnan. (1995).
#'   \emph{Continuous Univariate Distributions, Volume 2}.
#'   Second Edition. John Wiley and Sons, New York.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{Distribution.df}}, \code{\link{epdfPlot}}, \code{\link{cdfPlot}}.
#' }
#' @rawRd
#' \examples{
#'   # Plot the pdf of the standard normal distribution
#'   #-------------------------------------------------
#'   dev.new()
#'   pdfPlot()
#'
#'   #==========
#'
#'   # Plot the pdf of the standard normal distribution
#'   # and a N(2, 2) distribution on the sample plot.
#'   #-------------------------------------------------
#'   dev.new()
#'   pdfPlot(param.list = list(mean=2, sd=2),
#'     curve.fill = FALSE, ylim = c(0, dnorm(0)), main = "")
#'
#'   pdfPlot(add = TRUE, pdf.col = "red")
#'
#'   legend("topright", legend = c("N(2,2)", "N(0,1)"),
#'     col = c("black", "red"), lwd = 3 * par("cex"))
#'
#'   title("PDF Plots for Two Normal Distributions")
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   graphics.off()
#' }
#' @rawRd
#' \keyword{distribution}
#' @rawRd
#' \keyword{hplot}

pdfPlot <-
function (distribution = "norm", param.list = list(mean = 0, 
    sd = 1), left.tail.cutoff = ifelse(is.finite(supp.min), 0, 
    0.001), right.tail.cutoff = ifelse(is.finite(supp.max), 0, 
    0.001), plot.it = TRUE, add = FALSE, n.points = 1000, pdf.col = "black", 
    pdf.lwd = 3 * par("cex"), pdf.lty = 1, curve.fill = !add, 
    curve.fill.col = "cyan", x.ticks.at.all.x.max = 15, hist.col = ifelse(add, 
        "black", "cyan"), density = 5, digits = .Options$digits, 
    ..., type = "l", main = NULL, xlab = NULL, ylab = NULL, xlim = NULL, 
    ylim = NULL) 
{
    if (!is.list(param.list)) 
        stop("'param.list' must be a list.")
    check.da.list <- check.distribution.args(distribution, param.list)
    dist.abb <- check.da.list$dist.abb
    dist.name <- check.da.list$dist.name
    dist.type <- check.da.list$dist.type
    n.dist.params <- check.da.list$n.dist.params
    dist.params.names <- check.da.list$dist.params.names
    param.list <- check.da.list$param.list
    supp.min <- eval(parse(text = EnvStats::Distribution.df[dist.abb, 
        "Support.Min"]), envir = param.list)
    supp.max <- eval(parse(text = EnvStats::Distribution.df[dist.abb, 
        "Support.Max"]), envir = param.list)
    qname <- paste("q", dist.abb, sep = "")
    dname <- paste("d", dist.abb, sep = "")
    if (left.tail.cutoff == 0) {
        if (supp.min == -Inf) 
            stop(paste("The value of 'left.tail.cutoff' must be greater", 
                "than 0 for the", dist.name, "distribution since the support", 
                "on the left-hand tail is infinite."))
        else x.min <- supp.min
    }
    else x.min <- do.call(qname, c(list(p = left.tail.cutoff), 
        param.list))
    if (right.tail.cutoff == 0) {
        if (supp.max == Inf) 
            stop(paste("The value of 'right.tail.cutoff' must be greater", 
                "than 0 for the", dist.name, "distribution since the support", 
                "on the right-hand tail is infinite."))
        else x.max <- supp.max
    }
    else x.max <- do.call(qname, c(list(p = 1 - right.tail.cutoff), 
        param.list))
    if (add) {
        usr <- par("usr")
        x.min <- max(usr[1], x.min)
        x.max <- min(usr[2], x.max)
    }
    else if (!is.null(xlim)) {
        x.min <- max(xlim[1], x.min)
        x.max <- min(xlim[2], x.max)
    }
    discrete <- any(dist.type == c("Discrete", "Finite Discrete"))
    if (plot.it && !add) {
        if (is.null(xlab)) 
            xlab <- "Value of Random Variable"
        if (is.null(ylab)) 
            ylab <- ifelse(discrete, "Probability", "Relative Frequency")
        check.gp.list <- checkGraphicsPars(...)
        gp.names <- check.gp.list$gp.names
        n.gp <- check.gp.list$n.gp
        gen.gp.list <- check.gp.list$gen.gp.list
        if (is.null(main)) {
            if (any(dist.abb == c("beta", "chisq", "f", "t"))) {
                if (param.list$ncp > 0) 
                  main <- paste("Non-central ", dist.name, " Density\n", 
                    "(", paste(paste(dist.params.names, signif(unlist(param.list), 
                      digits), sep = "="), collapse = ", "), 
                    ")", sep = "")
                else {
                  main <- paste(dist.name, " Density\n", "(", 
                    paste(paste(dist.params.names[-n.dist.params], 
                      signif(unlist(param.list[-n.dist.params]), 
                        digits), sep = "="), collapse = ", "), 
                    ")", sep = "")
                }
            }
            else main <- paste(dist.name, " Density\n", "(", 
                paste(paste(dist.params.names, signif(unlist(param.list), 
                  digits), sep = "="), collapse = ", "), ")", 
                sep = "")
        }
    }
    if (dist.type == "Continuous") {
        x <- seq(x.min, x.max, len = n.points)
        y <- do.call(dname, c(list(x = x), param.list))
        if (any(dist.abb == c("beta", "exp", "gamma", "gammaAlt", 
            "weibull")) && any(index <- x == 0) && any(abs(y[index] - 
            do.call(dname, c(list(x = .Machine$double.eps), param.list))) > 
            .Machine$double.eps)) {
            x <- x[!index]
            y <- y[!index]
        }
        if (dist.abb == "beta" && any(index <- x == 1) && any(abs(y[index] - 
            do.call(dname, c(list(x = .Machine$double.eps), param.list))) > 
            .Machine$double.eps)) {
            x <- x[!index]
            y <- y[!index]
        }
        n.points <- length(x)
        if (plot.it) {
            if (!add) {
                if (is.null(xlim)) 
                  xlim <- range(x)
                if (is.null(ylim)) 
                  ylim <- c(0, max(y))
                plot(x, y, type = "n", ..., xlab = xlab, ylab = ylab, 
                  main = main, xlim = xlim, ylim = ylim)
                arg.list <- list(x = x, y = y)
                arg.list <- c(arg.list, gen.gp.list, list(type = type, 
                  col = pdf.col, lwd = pdf.lwd, lty = pdf.lty))
                do.call("lines", arg.list)
            }
            else {
                lines(x, y, ..., type = type, col = pdf.col, 
                  lwd = pdf.lwd, lty = pdf.lty)
            }
            if (curve.fill) {
                polygon(c(x, rev(x)), c(y, rep(0, n.points)), 
                  border = FALSE, col = curve.fill.col)
            }
        }
    }
    else if (discrete) {
        x <- ceiling(x.min):floor(x.max)
        y <- do.call(dname, c(list(x = x), param.list))
        nx <- length(x)
        con <- 0.4 + (0.1 * (nx - 2))/nx
        xleft <- x - con
        xright <- x + con
        ybottom <- rep(0, nx)
        if (plot.it) {
            if (!add) {
                if (is.null(xlim)) 
                  xlim.to.use <- c(min(xleft), max(xright))
                if (is.null(ylim)) 
                  ylim <- c(0, max(y))
                plot(x, y, type = "n", xaxt = "n", bty = "n", 
                  ..., xlim = xlim.to.use, ylim = ylim, xlab = xlab, 
                  ylab = ylab, main = main)
                rect(xleft = xleft, ybottom = ybottom, xright = xright, 
                  ytop = y, col = hist.col, border = pdf.col, 
                  ...)
                if (is.null(xlim) && length(x) <= x.ticks.at.all.x.max) {
                  arg.list <- list(side = 1, at = x, labels = x)
                }
                else {
                  arg.list <- list(side = 1)
                }
                arg.list <- c(arg.list, gen.gp.list)
                do.call("axis", arg.list)
            }
            else {
                o.par <- par(new = TRUE, xaxs = "d", yaxs = "d")
                on.exit(par(o.par))
                rect(xleft = xleft, ybottom = ybottom, xright = xright, 
                  ytop = y, col = hist.col, density = density, 
                  border = pdf.col, ...)
            }
        }
    }
    else {
        if (dist.name == "Zero-Modified Lognormal (Delta)" && 
            left.tail.cutoff != 0) 
            stop(paste("The value of 'left.tail.cutoff' must be 0", 
                "for the Zero-Modified Lognormal (Delta) distribution."))
        x <- seq(x.min, x.max, len = n.points)
        y <- do.call(dname, c(list(x = x), param.list))
        if (dist.name == "Zero-Modified Normal") {
            if (any(index <- x == 0)) {
                x <- c(0, x[!index])
                y <- c(y[index], y[!index])
            }
            else {
                x <- c(0, x)
                y <- c(do.call(dname, c(list(x = 0), param.list)), 
                  y)
                n.points <- n.points + 1
            }
        }
        if (plot.it) {
            if (!add) {
                if (is.null(xlim)) 
                  xlim <- range(x)
                if (is.null(ylim)) 
                  ylim <- c(0, max(y))
                plot(x[-1], y[-1], type = "n", ..., xlab = xlab, 
                  ylab = ylab, xlim = xlim, ylim = ylim, main = main)
                arg.list <- list(x = x[-1], y = y[-1])
                arg.list <- c(arg.list, gen.gp.list, list(type = type, 
                  col = pdf.col, lwd = pdf.lwd, lty = pdf.lty))
                do.call("lines", arg.list)
                if (curve.fill) {
                  polygon(c(x[-1], rev(x[-1])), c(y[-1], rep(0, 
                    n.points - 1)), border = FALSE, col = curve.fill.col)
                }
                arg.list <- list(x = x[1], y = y[1], type = "h")
                arg.list <- c(arg.list, gen.gp.list, list(col = pdf.col, 
                  lwd = pdf.lwd, lty = pdf.lty))
                do.call("points", arg.list)
            }
            else {
                lines(x[-1], y[-1], ..., type = type, col = pdf.col, 
                  lwd = pdf.lwd, lty = pdf.lty)
                if (curve.fill) {
                  polygon(c(x[-1], rev(x[-1])), c(y[-1], rep(0, 
                    n.points - 1)), border = FALSE, col = curve.fill.col)
                }
                points(x[1], y[1], type = "h", ..., col = pdf.col, 
                  lwd = pdf.lwd, lty = pdf.lty)
            }
        }
    }
    invisible(list(Quantiles = x, Probability.Densities = y))
}

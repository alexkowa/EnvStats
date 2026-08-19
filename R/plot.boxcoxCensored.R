#' Plot Results of Box-Cox Transformations Based on Type I Censored Data
#' @description
#' Plot the results of calling the function \code{\link{boxcoxCensored}},
#'   which returns an object of class \cr
#'   \code{"boxcoxCensored"}.  Three different kinds of plots are available.
#'
#'   The function \code{plot.boxcoxCensored} is automatically called by \code{\link{plot}}
#'   when given an object of class \code{"boxcoxCensored"}.
#' @usage
#' \method{plot}{boxcoxCensored}(x, plot.type = "Objective vs. lambda", same.window = TRUE,
#'     ask = same.window & plot.type != "Ojective vs. lambda",
#'     prob.method = "michael-schucany", plot.pos.con = 0.375,
#'     estimate.params = FALSE, equal.axes = qq.line.type == "0-1" || estimate.params,
#'     add.line = TRUE, qq.line.type = "least squares",
#'     duplicate.points.method = "standard", points.col = 1, line.col = 1,
#'     line.lwd = par("cex"), line.lty = 1, digits = .Options$digits,
#'     cex.main = 1.4 * par("cex"), cex.sub = par("cex"),
#'     main = NULL, sub = NULL, xlab = NULL, ylab = NULL, xlim = NULL,
#'     ylim = NULL, ...)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   an object of class \code{"boxcoxCensored"}.  See \code{\link{boxcoxCensored.object}}
#'   for details.
#' }
#'   \item{plot.type}{
#'   character string indicating what kind of plot to create.  Only one particular
#'   plot type will be created, unless \code{plot.type="All"}, in which case all plots
#'   will be created sequentially.  The possible values of \code{plot.type} are: \cr
#'   \code{"Objective vs. lambda"} (the default), \cr
#'   \code{"Q-Q Plots"}, \cr
#'   \code{"Tukey M-D Q-Q Plots"}, and \cr
#'   \code{"All"}.
#' }
#'   \item{same.window}{
#'   logical scalar indicating whether to produce all plots in the same graphics
#'   window (\code{same.window=TRUE}; the default), or to create a new graphics
#'   window for each separate plot (\code{same.window=FALSE}).  The argument is
#'   relevant only when \code{plot.type} produces more than one plot (i.e., when
#'   \code{plot.type} is not equal to \code{"Objective vs. lambda"}).
#' }
#'   \item{ask}{
#'   logical scalar supplied to the function \code{\link{devAskNewPage}}, indicating
#'   whether to prompt the user before creating a new plot within a single graphics
#'   window.  This argument is ignored when \code{plot.type="Objective vs. lambda"}
#'   (since only one plot is produced) or when \code{same.window=FALSE}, otherwise
#'   the default value is \code{ask=TRUE}.
#' }
#'   \item{points.col}{
#'   numeric scalar determining the color of the points in the plot.  The default
#'   value is \code{points.col=1}. See the entry for \code{col} in the \R help file for
#'   \code{\link{par}} for more information.
#' }
#'
#'
#' \bold{The following arguments can be supplied when \code{plot.type="Q-Q Plots"},} \cr
#' \bold{\code{plot.type="Tukey M-D Q-Q Plots"}, or \code{plot.type="All"}
#' (supplied to \code{\link{qqPlot}}):}
#'
#'   \item{prob.method}{
#'   character string indicating what method to use to compute the plotting positions
#'   for Q-Q plots or Tukey Mean-Difference Q-Q plots.
#'   Possible values are
#'   \code{"kaplan-meier"} (product-limit method of Kaplan and Meier (1958)),
#'   \code{"nelson"} (hazard plotting method of Nelson (1972)),
#'   \code{"michael-schucany"} (generalization of the product-limit method due to Michael and Schucany (1986)), and
#'   \code{"hirsch-stedinger"} (generalization of the product-limit method due to Hirsch and Stedinger (1987)).
#'   The default value is \code{prob.method="michael-schucany"}.
#'
#'   The \code{"nelson"} method is only available for objects that are the result of
#'   calling \code{\link{boxcoxCensored}} with the argument \code{censoring.side="right"}.
#'   See the help file for \code{\link{qqPlotCensored}} for more information.
#'
#'   This argument is ignored if \code{plot.type="Objective vs. lambda"}.
#' }
#'   \item{plot.pos.con}{
#'   numeric scalar between 0 and 1 containing the value of the plotting position
#'   constant used to construct the Q-Q plots and/or Tukey Mean-Difference Q-Q plots.
#'   The default value is \code{plot.pos.con=0.375}.  See the help file for
#'   \code{\link{qqPlotCensored}} for more information.
#' }
#'   \item{estimate.params}{
#'   logical scalar indicating whether to compute quantiles based on estimating the
#'   distribution parameters (\code{estimate.params=TRUE}) or using the
#'   distribution parameters for a standard normal distribution (i.e,
#'   \code{mean=0}, \code{sd=1}).  The default value is \code{estimate.params=FALSE}
#'   because a standard normal Q-Q plot will yield roughly a straight line if the
#'   observations are from \emph{any} normal distribution.  If you specify
#'   \code{plot.type="Tukey M-D Q-Q Plots"}, then you need to set
#'   \code{estiamte.params=TRUE} unless you want to assume the transformed data come
#'   from a standard normal distribution.
#' }
#'   \item{equal.axes}{
#'   logical scalar indicating whether to use the same range on the \eqn{x}- and
#'   \eqn{y}-axes when \code{plot.type="Q-Q Plots"}.  The default value is
#'   \code{TRUE} if \cr
#'   \code{qq.line.type="0-1"} or \code{estimate.params=TRUE},
#'   otherwise it is \code{FALSE}.
#' }
#'   \item{add.line}{
#'   logical scalar indicating whether to add a line to the plot.  If \code{add.line=TRUE}
#'   and \code{plot.type="Q-Q Plots"}, a line determined by the value of
#'   \code{qq.line.type} is added to the plot.  If \code{add.line=TRUE} and
#'   \code{plot.type="Tukey M-D Q-Q Plots"}, a horizontal line at \eqn{y=0} is added to
#'   the plot.  The default value is \code{add.line=TRUE}.
#' }
#'   \item{qq.line.type}{
#'   character string determining what kind of line to add to the plot when \cr
#'   \code{plot.type="Q-Q Plots"}.  Possible values are \code{"least squares"}
#'   (a least squares line; the default),
#'   \code{"0-1"} (a line with intercept 0 and slope 1),
#'   and \code{"robust"} (a line is fit through the first and third quartiles of the
#'   \eqn{x} and \eqn{y} data).  This argument is ignored if \code{add.line=FALSE}.
#' }
#'   \item{duplicate.points.method}{
#'   a character string denoting how to plot points with duplicate \eqn{(x,y)} values.
#'   Possible values are \code{"standard"} (a single plotting symbol is plotted;
#'   the default), \code{"jitter"} (a separate plotting symbol is plotted for
#'   each duplicate point, where the plotting symbols cluster around the true value
#'   of \eqn{x} and \eqn{y}), and \code{"number"} (a single number is plotted at
#'   \eqn{(x,y)} that represents how many duplicate points are at that \eqn{(x,y)}
#'   coordinate).
#' }
#'   \item{line.col}{
#'   numeric scalar determining the color of the line in the plot.  The default value
#'   is \code{line.col=1}.  See the entry for \code{col} in the \R help file for
#'   \code{\link{par}} for more information.  This argument is ignored if
#'   \code{add.line=FALSE}.
#' }
#'   \item{line.lwd}{
#'   numeric scalar determining the width of the line in the plot.  The default value
#'   is \code{line.lwd=par("cex")}.  See the entry for \code{lwd} in the \R help file for
#'   \code{\link{par}} for more information.  This argument is ignored if
#'   \code{add.line=FALSE}.
#' }
#'   \item{line.lty}{
#'   numeric scalar determining the line type (style) of the line in the plot.
#'   The default value is \code{line.lty=1}.  See the entry for \code{lty}
#'   in the \R help file for \code{\link{par}} for more information.
#'   This argument is ignored if \code{add.line=FALSE}.
#' }
#'   \item{digits}{
#'   scalar indicating how many significant digits to print for the distribution
#'   parameters and the value of the objective in the sub-title.  The default
#'   value is the current setting of \code{options("digits")}.
#' }
#'
#' \bold{Graphics parameters:}
#'   \item{cex.main, cex.sub, main, sub, xlab, ylab, xlim, ylim, \dots}{
#'   graphics parameters; see \code{\link{par}} for more information.  The
#'   default value of \code{cex.main} is \code{cex.main=1.4 * par("cex")}.
#'   The default value of \code{cex.sub} is \cr
#'   \code{cex.sub=par("cex")}.
#' }
#' }
#' @rawRd
#' \details{
#'   The function \code{plot.boxcoxCensored} is a method for the generic function
#'   \code{\link{plot}} for the class \cr
#'   \code{"boxcoxCensored"} (see \code{\link{boxcoxCensored.object}}).
#'   It can be invoked by calling \code{\link{plot}} and giving it an object of
#'   class \code{"boxcoxCensored"} as the first argument, or by calling
#'   \code{plot.boxcoxCensored} directly, regardless of the class of the object given
#'   as the first argument to \code{plot.boxcoxCensored}.
#'
#'   Plots associated with Box-Cox transformations are produced on the current graphics
#'   device.  These can be one or all of the following:
#'   \itemize{
#'   \item Objective vs. \eqn{\lambda}.
#'   \item Observed Quantiles vs. Normal Quantiles (Q-Q Plot) for the transformed
#'   observations for each of the values of \eqn{\lambda}.
#'   \item Tukey Mean-Difference Q-Q Plots for the transformed observations for each
#'   of the values of \eqn{\lambda}.
#'   }
#'   See the help files for \code{\link{boxcoxCensored}} and \code{\link{qqPlotCensored}}
#'   for more information.
#' }
#' @rawRd
#' \value{
#'   \code{plot.boxcoxCensored} invisibly returns the first argument, \code{x}.
#' }
#' @rawRd
#' \references{
#'   Chambers, J. M. and Hastie, T. J. (1992).  \emph{Statistical Models in S}.
#'   Wadsworth & Brooks/Cole.
#' }
#' @rawRd
#' \author{
#'   Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{qqPlotCensored}}, \code{\link{boxcoxCensored}},
#'   \code{\link{boxcoxCensored.object}}, \code{\link{print.boxcoxCensored}},
#'   \link{Data Transformations}, \code{\link{plot}}.
#' }
#' @rawRd
#' \examples{
#'   # Generate 15 observations from a lognormal distribution with
#'   # mean=10 and cv=2 and censor the observations less than 2.
#'   # Then generate 15 more observations from this distribution and
#'   # censor the observations less than 4.
#'   # Then call the function boxcoxCensored, and then plot the results.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(250)
#'
#'   x.1 <- rlnormAlt(15, mean = 10, cv = 2)
#'   censored.1 <- x.1 < 2
#'   x.1[censored.1] <- 2
#'
#'   x.2 <- rlnormAlt(15, mean = 10, cv = 2)
#'   censored.2 <- x.2 < 4
#'   x.2[censored.2] <- 4
#'
#'   x <- c(x.1, x.2)
#'   censored <- c(censored.1, censored.2)
#'
#'   # Plot the results based on the PPCC objective
#'   #---------------------------------------------
#'   boxcox.list <- boxcoxCensored(x, censored)
#'   dev.new()
#'   plot(boxcox.list)
#'
#'   # Look at Q-Q Plots for the candidate values of lambda
#'   #-----------------------------------------------------
#'   plot(boxcox.list, plot.type = "Q-Q Plots", same.window = FALSE)
#'
#'
#'   # Look at Tukey Mean-Difference Q-Q Plots
#'   # for the candidate values of lambda
#'   #----------------------------------------
#'   plot(boxcox.list, plot.type = "Tukey M-D Q-Q Plots", same.window = FALSE)
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(x.1, censored.1, x.2, censored.2, x, censored, boxcox.list)
#'   graphics.off()
#' }
#' @rawRd
#' \keyword{plot}

plot.boxcoxCensored <-
function (x, plot.type = "Objective vs. lambda", same.window = TRUE, 
    ask = same.window & plot.type != "Ojective vs. lambda", prob.method = "michael-schucany", 
    plot.pos.con = 0.375, estimate.params = FALSE, equal.axes = qq.line.type == 
        "0-1" || estimate.params, add.line = TRUE, qq.line.type = "least squares", 
    duplicate.points.method = "standard", points.col = 1, line.col = 1, 
    line.lwd = par("cex"), line.lty = 1, digits = .Options$digits, 
    cex.main = 1.4 * par("cex"), cex.sub = par("cex"), main = NULL, 
    sub = NULL, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, 
    ...) 
{
    boxcoxCensored.obj <- x
    plot.type <- match.arg(plot.type, c("All", "Objective vs. lambda", 
        "Q-Q Plots", "Tukey M-D Q-Q Plots"))
    prob.method <- match.arg(prob.method, c("michael-schucany", 
        "hirsch-stedinger", "kaplan-meier", "nelson"))
    qq.line.type <- match.arg(qq.line.type, c("least squares", 
        "0-1", "robust"))
    duplicate.points.method <- match.arg(duplicate.points.method, 
        c("standard", "jitter", "number"))
    lambda <- boxcoxCensored.obj$lambda
    objective <- boxcoxCensored.obj$objective
    objective.name <- boxcoxCensored.obj$objective.name
    check.gp.list <- checkGraphicsPars(...)
    gp.arg.list <- check.gp.list$gp.arg.list
    gen.gp.list <- check.gp.list$gen.gp.list
    data.name <- boxcoxCensored.obj$data.name
    data.name.string <- data.name
    parent.of.data <- boxcoxCensored.obj$parent.of.data
    if (!is.null(parent.of.data)) 
        data.name.string <- paste(data.name, "in", parent.of.data)
    censoring.side <- boxcoxCensored.obj$censoring.side
    user.main <- main
    user.sub <- sub
    user.xlab <- xlab
    user.ylab <- ylab
    user.xlim <- xlim
    user.ylim <- ylim
    if (plot.type != "Objective vs. lambda" & same.window) {
        devAskNewPage(ask = ask)
    }
    if (is.element(plot.type, c("All", "Objective vs. lambda"))) {
        if (is.null(user.xlab)) 
            xlab <- expression(paste(lambda))
        if (is.null(user.ylab)) 
            ylab <- objective.name
        arg.list <- list(x = lambda, y = objective, type = "n", 
            xlab = xlab, ylab = ylab)
        arg.list <- c(arg.list, ...)
        if (!is.null(user.xlim)) 
            arg.list <- c(arg.list, list(xlim = user.xlim))
        if (!is.null(user.ylim)) 
            arg.list <- c(arg.list, list(ylim = user.ylim))
        do.call("plot", arg.list)
        arg.list <- c(list(x = lambda, y = objective, col = points.col), 
            gen.gp.list)
        do.call("points", arg.list)
        if (is.null(user.main)) {
            string1 <- "Box-Cox Transformation Results:"
            string2 <- paste(objective.name, " vs. lambda for ", 
                data.name.string, " (Censored Data)", sep = "")
            main <- paste(string1, string2, sep = "\n")
        }
        else main <- user.main
        arg.list <- c(list(cex.main = cex.main), gen.gp.list, 
            list(main = main))
        do.call("title", arg.list)
    }
    if (is.element(plot.type, c("All", "Q-Q Plots"))) {
        data <- boxcoxCensored.obj$data
        if (is.null(data)) 
            stop("'data' component missing from 'x'")
        censored <- boxcoxCensored.obj$censored
        if (is.null(censored)) 
            stop("'censored' component missing from 'x'")
        eps <- boxcoxCensored.obj$eps
        if (is.null(data)) 
            stop("'data' component missing from 'x'")
        n <- length(lambda)
        for (i in 1:n) {
            if (is.null(user.ylab)) {
                qlab <- paste("[ (", data.name, "^", lambda[i], 
                  "- 1 )", "/", lambda[i], "]")
                ylab <- paste("Quantiles of", qlab)
            }
            y <- boxcoxTransform(data, lambda = lambda[i], eps = eps)
            if (!same.window) 
                dev.new()
            qqPlotCensored(y, censored, censoring.side = censoring.side, 
                prob.method = prob.method, estimate.params = estimate.params, 
                plot.pos.con = plot.pos.con, equal.axes = equal.axes, 
                add.line = add.line, qq.line.type = qq.line.type, 
                duplicate.points.method = duplicate.points.method, 
                points.col = points.col, line.col = line.col, 
                line.lwd = line.lwd, line.lty = line.lty, digits = digits, 
                ..., xlab = user.xlab, ylab = ylab, main = "", 
                xlim = user.xlim, ylim = user.ylim)
            if (is.null(user.main)) {
                string1 <- "Normal Q-Q Plot of Box-Cox Transformation for"
                string2 <- paste(data.name.string, " (Censored Data) with lambda = ", 
                  format(lambda[i], digits = digits), sep = "")
                main <- paste(string1, string2, sep = "\n")
            }
            else main <- user.main
            arg.list <- c(list(cex.main = cex.main), gen.gp.list, 
                list(main = main))
            do.call("title", arg.list)
            if (is.null(user.sub)) {
                sub <- paste(objective.name, "=", format(objective[i], 
                  digits = digits))
            }
            else sub <- user.sub
            mtext(sub, side = 1, line = 4, cex = cex.sub)
        }
    }
    if (is.element(plot.type, c("All", "Tukey M-D Q-Q Plots"))) {
        data <- boxcoxCensored.obj$data
        if (is.null(data)) 
            stop("'data' component missing from 'x'")
        censored <- boxcoxCensored.obj$censored
        if (is.null(censored)) 
            stop("'censored' component missing from 'x'")
        eps <- boxcoxCensored.obj$eps
        if (is.null(data)) 
            stop("'data' component missing from 'x'")
        n <- length(lambda)
        for (i in 1:n) {
            y <- boxcoxTransform(data, lambda = lambda[i], eps = eps)
            if (!same.window) 
                dev.new()
            qqPlotCensored(y, censored, censoring.side = censoring.side, 
                prob.method = prob.method, estimate.params = TRUE, 
                plot.type = "Tukey Mean-Difference Q-Q", plot.pos.con = plot.pos.con, 
                add.line = add.line, duplicate.points.method = duplicate.points.method, 
                points.col = points.col, line.col = line.col, 
                line.lwd = line.lwd, line.lty = line.lty, digits = digits, 
                ..., xlab = user.xlab, ylab = user.ylab, main = "", 
                xlim = user.xlim, ylim = user.ylim)
            if (is.null(user.main)) {
                string1 <- "Mean-Difference Q-Q Plot of Box-Cox Transformation for"
                string2 <- paste(data.name.string, " (Censored Data) with lambda = ", 
                  format(lambda[i], digits = digits), sep = "")
                main <- paste(string1, string2, sep = "\n")
            }
            else main <- user.main
            arg.list <- c(list(cex.main = cex.main), gen.gp.list, 
                list(main = main))
            do.call("title", arg.list)
            if (is.null(user.sub)) {
                sub <- paste(objective.name, "=", format(objective[i], 
                  digits = digits))
            }
            else sub <- user.sub
            mtext(sub, side = 1, line = 4, cex = cex.sub)
        }
    }
    invisible(boxcoxCensored.obj)
}

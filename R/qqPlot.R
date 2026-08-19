#' Quantile-Quantile (Q-Q) Plot
#' @description
#' Produces a quantile-quantile (Q-Q) plot, also called a probability plot.
#'   The \code{qqPlot} function is a modified version of the \R functions
#'   \code{\link{qqnorm}} and \code{\link{qqplot}}.
#'   The \pkg{EnvStats} function \code{qqPlot} allows the user to specify a number of
#'   different distributions in addition to the normal distribution, and to optionally
#'   estimate the distribution parameters of the fitted distribution.
#' @usage
#' qqPlot(x, y = NULL, distribution = "norm", param.list = list(mean = 0, sd = 1),
#'     estimate.params = plot.type == "Tukey Mean-Difference Q-Q",
#'     est.arg.list = NULL, plot.type = "Q-Q", plot.pos.con = NULL, plot.it = TRUE,
#'     equal.axes = qq.line.type == "0-1" || estimate.params, add.line = FALSE,
#'     qq.line.type = "least squares", duplicate.points.method = "standard",
#'     points.col = 1, line.col = 1, line.lwd = par("cex"), line.lty = 1,
#'     digits = .Options$digits, ..., main = NULL, xlab = NULL, ylab = NULL,
#'     xlim = NULL, ylim = NULL)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   numeric vector of observations.  When \code{y} is not supplied, \code{x} represents a sample
#'   from the hypothesized distribution specifed by \code{distribution}.  When \code{y} is supplied,
#'   the distribution of \code{x} is compared with the distribuiton of \code{y}.
#'   Missing (\code{NA}), undefined (\code{NaN}), and
#'   infinite (\code{Inf}, \code{-Inf}) values are allowed but will be removed.
#' }
#'   \item{y}{
#'   optional numeric vector of observations (not necessarily the same lenght as \code{x}).
#'   Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf}, \code{-Inf})
#'   values are allowed but will be removed.
#' }
#'   \item{distribution}{
#'   when \code{y} is not supplied,
#'   a character string denoting the distribution abbreviation.  The default value is
#'   \code{distribution="norm"}.  See the help file for \cr
#'   \code{\link{Distribution.df}} for a
#'   list of possible distribution abbreviations.  This argument is ignored if \code{y}
#'   is supplied.
#' }
#'   \item{param.list}{
#'   when \code{y} is not supplied,
#'   a list with values for the parameters of the distribution.  The default value is
#'   \code{param.list=list(mean=0, sd=1)}.  See the help file for \code{\link{Distribution.df}}
#'   for the names and possible values of the parameters associated with each distribution.
#'   This argument is ignored if \code{y} is supplied or \code{estimate.params=TRUE}.
#' }
#'   \item{estimate.params}{
#'   when \code{y} is not supplied,
#'   a logical scalar indicating whether to compute quantiles based on estimating the
#'   distribution parameters (\code{estimate.params=TRUE}) or using the known
#'   distribution parameters specified in \code{param.list} \cr
#'   (\code{estimate.params=FALSE}).  The default value of \code{estimate.params}
#'   is \code{FALSE} if \code{plot.type="Q-Q"} because the default configuration is a standard normal
#'   (mean=0, sd=1) Q-Q plot, which will yield roughly a straight line if the observations in
#'   \code{x} are from any normal distribution.  The default value of \code{estimate.params}
#'   is \code{TRUE} if \code{plot.type="Tukey Mean-Difference Q-Q"}.  The argument \cr
#'   \code{estimate.params}
#'   is ignored if \code{y} is supplied.
#' }
#'   \item{est.arg.list}{
#'   when \code{y} is not supplied and \code{estimate.params=TRUE},
#'   a list whose components are optional arguments associated with the function used to estimate
#'   the parameters of the assumed distribution (see the help file
#'   \link[=FcnsByCatEstDistParams]{Estimating Distribution Parameters}).
#'   For example, all functions used to estimate distribution parameters have an optional argument
#'   called \code{method} that specifies the method to use to estimate the parameters.
#'   (See the help file for \code{\link{Distribution.df}} for a list of available estimation
#'   methods for each distribution.)  To override the default estimation method, supply the argument
#'   \code{est.arg.list} with a component called \code{method}; for example
#'   \code{est.arg.list=list(method="mle")}.  The default value is
#'   \code{est.arg.list=NULL} so that all default values for the estimating function are used.
#'   This argument is ignored if \code{estimate.params=FALSE} or \code{y} is supplied.
#' }
#'   \item{plot.type}{
#'   a character string denoting the kind of plot.  Possible values are \code{"Q-Q"}
#'   (Quantile-Quantile plot, the default) and \code{"Tukey Mean-Difference Q-Q"}
#'   (Tukey mean-difference Q-Q plot).  This argument may be abbreviated (e.g.,
#'   \code{plot.type="T"} to indicate a Tukey mean-difference Q-Q plot).
#' }
#'   \item{plot.pos.con}{
#'   numeric scalar between 0 and 1 containing the value of the plotting position constant.
#'   The default value of \code{plot.pos.con} depends on whether the argument \code{y} is supplied,
#'   and if not the value of the argument \code{distribution}.  When \code{y} is supplied, the default
#'   value is \code{plot.pos.con=0.5}, corresponding to Hazen plotting positions.  When \code{y} is
#'   not supplied, for the normal, lognormal, three-parameter lognormal, zero-modified normal, and
#'   zero-modified lognormal distributions, the default value is \code{plot.pos.con=0.375}.
#'   For the Type I extreme value (Gumbel) distribution (\code{distribution="evd"}),
#'   the default value is \cr
#'   \code{plot.pos.con=0.44}.  For all other distributions, the default value is \cr
#'   \code{plot.pos.con=0.4}.
#' }
#'   \item{plot.it}{
#'   a logical scalar indicating whether to create a plot on the current graphics device.
#'   The default value is \code{plot.it=TRUE}.
#' }
#'   \item{equal.axes}{
#'   a logical scalar indicating whether to use the same range on the \eqn{x}- and \eqn{y}-axes
#'   when \code{plot.type="Q-Q"}.  The default value is \code{TRUE} if \code{qq.line.type="0-1"} or
#'   \code{estimate.params=TRUE}, otherwise it is \code{FALSE}.  This argument is ignored if
#'   \code{plot.type="Tukey Mean-Difference Q-Q"}.
#' }
#'   \item{add.line}{
#'   a logical scalar indicating whether to add a line to the plot.  If \code{add.line=TRUE} and
#'   \code{plot.type="Q-Q"}, a line determined by the value of \code{qq.line.type} is added to the plot.
#'   If \code{add.line=TRUE} and \cr
#'   \code{plot.type="Tukey Mean-Difference Q-Q"}, a horizontal line at
#'   \eqn{y=0} is added to the plot.  The default value is \code{add.line=FALSE}.
#' }
#'   \item{qq.line.type}{
#'   character string determining what kind of line to add to the Q-Q plot.  Possible values are
#'   \code{"least squares"} (the default), \code{"0-1"} and \code{"robust"}.  For the value
#'   \code{"least squares"}, a least squares line is fit and added.  For the value \code{"0-1"},
#'   a line with intercept 0 and slope 1 is added.  For the value \code{"robust"}, a line is fit through
#'   the first and third quartiles of the \code{x} and \code{y} data.  This argument is ignored if
#'   \code{add.line=FALSE} or \code{plot.type="Tukey Mean-Difference Q-Q"}.
#' }
#'   \item{duplicate.points.method}{
#'   a character string denoting how to plot points with duplicate \eqn{(x,y)} values.  Possible values
#'   are \code{"standard"} (the default), \code{"jitter"}, and \code{"number"}.  For the value
#'   \code{"standard"}, a single plotting symbol is plotted (this is the default behavior of \R).
#'   For the value \code{"jitter"}, a separate plotting symbol is plotted for each duplicate point, where
#'   the plotting symbols cluster around the true value of \eqn{x} and \eqn{y}.  For the value
#'   \code{"number"}, a single number is plotted at \eqn{(x,y)} that represents how many duplicate points
#'   are at that \eqn{(x,y)} coordinate.
#' }
#'   \item{points.col}{
#'   a numeric scalar or character string determining the color of the points in the plot.
#'   The default value is \code{points.col=1}.  See the entry for \code{col} in the help file for
#'   \code{\link{par}} for more information.
#' }
#'   \item{line.col}{
#'   a numeric scalar or character string determining the color of the line in the plot.
#'   The default value is \code{points.col=1}.  See the entry for \code{col} in the help file for
#'   \code{\link{par}} for more information.  This argument is ignored if \code{add.line=FALSE}.
#' }
#'   \item{line.lwd}{
#'   a numeric scalar determining the width of the line in the plot.  The default value is
#'   \code{line.lwd=par("cex")}.  See the entry for \code{lwd} in the help file for \code{\link{par}}
#'   for more information.  This argument is ignored if \code{add.line=FALSE}.
#' }
#'   \item{line.lty}{
#'   a numeric scalar determining the line type of the line in the plot.  The default value is
#'   \code{line.lty=1}.  See the entry for \code{lty} in the help file for \code{\link{par}}
#'   for more information.  This argument is ignored if \code{add.line=FALSE}.
#' }
#'   \item{digits}{
#'   a scalar indicating how many significant digits to print for the distribution parameters.
#'   The default value is \code{digits=.Options$digits}.
#' }
#'   \item{main, xlab, ylab, xlim, ylim, \dots}{
#'   additional graphical parameters (see \code{\link{par}}).
#' }
#' }
#' @details
#' This help page has been shortened to keep function help focused on usage,
#' arguments, return values, and examples. Extended method details, formulas,
#' and background material are available in \code{vignette("extended-function-details", package = "EnvStats")}, section \code{qqPlot}.
#' @rawRd
#' \value{
#'   \code{qqPlot} returns a list with components \code{x} and \code{y}, giving the \eqn{(x,y)}
#'   coordinates of the points that have been or would have been plotted.  There are four cases to
#'   consider:
#'
#'   1. The argument \code{y} is not supplied and \code{plot.type="Q-Q"}.
#'
#'   \item{x}{the quantiles from the theoretical distribution.}
#'   \item{y}{the observed quantiles (order statistics) based on the data in the argument \code{x}.}
#'   \cr
#'
#'   2. The argument \code{y} is not supplied and \code{plot.type="Tukey Mean-Difference Q-Q"}.
#'
#'   \item{x}{the averages of the observed and theoretical quantiles.}
#'   \item{y}{the differences between the observed quantiles (order statistics) and the theoretical quantiles.}
#'   \cr
#'
#'   3. The argument \code{y} is supplied and \code{plot.type="Q-Q"}.
#'
#'   \item{x}{the observed quantiles based on the data in the argument \code{x}.
#'     Note that these are adjusted quantiles if the number of observations in the
#'     argument \code{x} is greater then the number of observations in the argument \code{y}.}
#'   \item{y}{the observed quantiles based on the data in the argument \code{y}.
#'     Note that these are adjusted quantiles if the number of observations in the
#'     argument \code{y} is greater then the number of observations in the argument \code{x}.}
#'   \cr
#'
#'   4. The argument \code{y} is supplied and \code{plot.type="Tukey Mean-Difference Q-Q"}.
#'
#'   \item{x}{the averages of the quantiles based on the argument \code{x} and the quantiles based
#'     on the argument \code{y}.}
#'   \item{y}{the differences between the quantiles based on the argument \code{x} and the quantiles based
#'     on the argument \code{y}.}
#' }
#' @rawRd
#' \references{
#'   Chambers, J.M., W.S. Cleveland, B. Kleiner, and P.A. Tukey. (1983).
#'   \emph{Graphical Methods for Data Analysis}. Duxbury Press, Boston, MA, pp.11-16.
#'
#'   Cleveland, W.S. (1993). \emph{Visualizing Data}. Hobart Press, Summit, New Jersey, 360pp.
#'
#'   D'Agostino, R.B. (1986a). Graphical Analysis.
#'   In: D'Agostino, R.B., and M.A. Stephens, eds. \emph{Goodness-of Fit Techniques}.
#'   Marcel Dekker, New York, Chapter 2, pp.7-62.
#' }
#' @rawRd
#' \author{
#'     Steven P. Millard (\email{EnvStats@ProbStatInfo.com})
#' }
#' @rawRd
#' \note{
#'   A \emph{quantile-quantile (Q-Q) plot}, also called a \emph{probability plot}, is a plot of the observed
#'   order statistics from a random sample (the empirical quantiles) against their (estimated)
#'   mean or median values based on an assumed distribution, or against the empirical quantiles
#'   of another set of data (Wilk and Gnanadesikan, 1968).  Q-Q plots are used to assess whether
#'   data come from a particular distribution, or whether two datasets have the same parent
#'   distribution.  If the distributions have the same shape (but not necessarily the same
#'   location or scale parameters), then the plot will fall roughly on a straight line.  If the
#'   distributions are exactly the same, then the plot will fall roughly on the straight line \eqn{y=x}.
#'
#'   A \emph{Tukey mean-difference Q-Q plot}, also called an \emph{m-d plot}, is a modification of a
#'   Q-Q plot. Rather than plotting observed quantiles vs. theoretical quantiles or observed
#'   \eqn{y}-quantiles vs. observed \eqn{x}-quantiles, a Tukey mean-difference Q-Q plot plots
#'   the difference between the quantiles on the \eqn{y}-axis vs. the average of the quantiles on
#'   the \eqn{x}-axis (Cleveland, 1993, pp.22-23).  If the two sets of quantiles come from the same
#'   parent distribution, then the points in this plot should fall roughly along the horizontal line
#'   \eqn{y=0}.  If one set of quantiles come from the same distribution with a shift in median, then
#'   the points in this plot should fall along a horizontal line above or below the line \eqn{y=0}.
#'   A Tukey mean-difference Q-Q plot enhances our perception of how the points in the Q-Q plot deviate
#'   from a straight line, because it is easier to judge deviations from a horizontal line than from a
#'   line with a non-zero slope.
#'
#'   In a Q-Q plot, the extreme points have more variability than points toward the center.  A U-shaped
#'   Q-Q plot indicates that the underlying distribution for the observations on the \eqn{y}-axis is
#'   skewed to the right relative to the underlying distribution for the observations on the \eqn{x}-axis.
#'   An upside-down-U-shaped Q-Q plot indicates the \eqn{y}-axis distribution is skewed left relative to
#'   the \eqn{x}-axis distribution.  An S-shaped Q-Q plot indicates the \eqn{y}-axis distribution has
#'   shorter tails than the \eqn{x}-axis distribution.  Conversely, a plot that is bent down on the
#'   left and bent up on the right indicates that the \eqn{y}-axis distribution has longer tails than
#'   the \eqn{x}-axis distribution.
#' }
#' @rawRd
#' \seealso{
#'   \code{\link{ppoints}}, \code{\link{ecdfPlot}}, \code{\link{Distribution.df}},
#'   \code{\link{qqPlotGestalt}}, \code{\link{qqPlotCensored}}, \code{\link{qqnorm}}.
#' }
#' @rawRd
#' \examples{
#'   # The guidance document USEPA (1994b, pp. 6.22--6.25)
#'   # contains measures of 1,2,3,4-Tetrachlorobenzene (TcCB)
#'   # concentrations (in parts per billion) from soil samples
#'   # at a Reference area and a Cleanup area.  These data are strored
#'   # in the data frame EPA.94b.tccb.df.
#'   #
#'   # Create an Q-Q plot for the reference area data first assuming a
#'   # normal distribution, then a lognormal distribution, then a
#'   # gamma distribution.
#'
#'   # Assume a normal distribution
#'   #-----------------------------
#'
#'   dev.new()
#'   with(EPA.94b.tccb.df, qqPlot(TcCB[Area == "Reference"]))
#'
#'   dev.new()
#'   with(EPA.94b.tccb.df, qqPlot(TcCB[Area == "Reference"], add.line = TRUE))
#'
#'   dev.new()
#'   with(EPA.94b.tccb.df, qqPlot(TcCB[Area == "Reference"],
#'     plot.type = "Tukey", add.line = TRUE))
#'
#'
#'   # The Q-Q plot based on assuming a normal distribution shows a U-shape,
#'   # indicating the Reference area TcCB data are skewed to the right
#'   # compared to a normal distribuiton.
#'
#'   # Assume a lognormal distribution
#'   #--------------------------------
#'
#'   dev.new()
#'   with(EPA.94b.tccb.df,
#'     qqPlot(TcCB[Area == "Reference"], dist = "lnorm",
#'       digits = 2, points.col = "blue", add.line = TRUE))
#'
#'   dev.new()
#'   with(EPA.94b.tccb.df,
#'     qqPlot(TcCB[Area == "Reference"], dist = "lnorm",
#'       digits = 2, plot.type = "Tukey", points.col = "blue",
#'       add.line = TRUE))
#'
#'   # Alternative parameterization
#'
#'   dev.new()
#'   with(EPA.94b.tccb.df,
#'     qqPlot(TcCB[Area == "Reference"], dist = "lnormAlt",
#'       estimate.params = TRUE, digits = 2, points.col = "blue",
#'       add.line = TRUE))
#'
#'   dev.new()
#'   with(EPA.94b.tccb.df,
#'     qqPlot(TcCB[Area == "Reference"], dist = "lnormAlt",
#'       digits = 2, plot.type = "Tukey", points.col = "blue",
#'       add.line = TRUE))
#'
#'
#'   # The lognormal distribution appears to be an adequate fit.
#'   # Now look at a Q-Q plot assuming a gamma distribution.
#'   #----------------------------------------------------------
#'
#'   dev.new()
#'   with(EPA.94b.tccb.df,
#'     qqPlot(TcCB[Area == "Reference"], dist = "gamma",
#'       estimate.params = TRUE, digits = 2, points.col = "blue",
#'       add.line = TRUE))
#'
#'   dev.new()
#'   with(EPA.94b.tccb.df,
#'     qqPlot(TcCB[Area == "Reference"], dist = "gamma",
#'       digits = 2, plot.type = "Tukey", points.col = "blue",
#'       add.line = TRUE))
#'
#'   # Alternative Parameterization
#'
#'   dev.new()
#'   with(EPA.94b.tccb.df,
#'     qqPlot(TcCB[Area == "Reference"], dist = "gammaAlt",
#'       estimate.params = TRUE, digits = 2, points.col = "blue",
#'       add.line = TRUE))
#'
#'   dev.new()
#'   with(EPA.94b.tccb.df,
#'     qqPlot(TcCB[Area == "Reference"], dist = "gammaAlt",
#'       digits = 2, plot.type = "Tukey", points.col = "blue",
#'       add.line = TRUE))
#'
#'   #-------------------------------------------------------------------------------------
#'
#'   # Generate 20 observations from a gamma distribution with parameters
#'   # shape=2 and scale=2, then create a normal (Gaussian) Q-Q plot for these data.
#'   # (Note: the call to set.seed simply allows you to reproduce this example.)
#'
#'   set.seed(357)
#'   dat <- rgamma(20, shape=2, scale=2)
#'   dev.new()
#'   qqPlot(dat, add.line = TRUE)
#'
#'   # Now assume a gamma distribution and estimate the parameters
#'   #------------------------------------------------------------
#'
#'   dev.new()
#'   qqPlot(dat, dist = "gamma", estimate.params = TRUE, add.line = TRUE)
#'
#'   # Clean up
#'   #---------
#'   rm(dat)
#'   graphics.off()
#' }
#' @rawRd
#' \keyword{distribution}
#' @rawRd
#' \keyword{hplot}

qqPlot <-
function (x, y = NULL, distribution = "norm", param.list = list(mean = 0, 
    sd = 1), estimate.params = plot.type == "Tukey Mean-Difference Q-Q", 
    est.arg.list = NULL, plot.type = "Q-Q", plot.pos.con = NULL, 
    plot.it = TRUE, equal.axes = qq.line.type == "0-1" || estimate.params, 
    add.line = FALSE, qq.line.type = "least squares", duplicate.points.method = "standard", 
    points.col = 1, line.col = 1, line.lwd = par("cex"), line.lty = 1, 
    digits = .Options$digits, ..., main = NULL, xlab = NULL, 
    ylab = NULL, xlim = NULL, ylim = NULL) 
{
    if (!is.vector(x, mode = "numeric") || is.factor(x)) 
        stop("'x' must be a numeric vector")
    x.name <- deparse(substitute(x))
    if ((bad.obs <- sum(!(x.ok <- is.finite(x)))) > 0) {
        is.not.finite.warning(x)
        x <- x[x.ok]
        warning(paste(bad.obs, "observations with NA/NaN/Inf in 'x' removed."))
    }
    nx <- length(x)
    plot.type <- match.arg(plot.type, c("Q-Q", "Tukey Mean-Difference Q-Q"))
    duplicate.points.method <- match.arg(duplicate.points.method, 
        c("standard", "jitter", "number"))
    qq.line.type <- match.arg(qq.line.type, c("least squares", 
        "0-1", "robust"))
    gen.gp.list <- checkGraphicsPars(...)$gen.gp.list
    ret.list <- list()
    if (!is.null(y)) {
        if (!is.vector(y, mode = "numeric")) 
            stop("'y' must be a numeric vector")
        y.name <- deparse(substitute(y))
        if ((bad.obs <- sum(!(y.ok <- is.finite(y)))) > 0) {
            is.not.finite.warning(y)
            y <- y[y.ok]
            warning(paste(bad.obs, "observations with NA/NaN/Inf in 'y' removed."))
        }
        if (is.null(plot.pos.con)) {
            plot.pos.con <- 0.5
        }
        else {
            if (!is.vector(plot.pos.con, mode = "numeric") || 
                length(plot.pos.con) != 1 || plot.pos.con < 0 || 
                plot.pos.con > 1) 
                stop("'plot.pos.con' must be a numeric scalar between 0 and 1")
        }
        q.x <- sort(x)
        q.y <- sort(y)
        ny <- length(y)
        if (nx != ny) {
            ppoints.x <- ppoints(nx, a = plot.pos.con)
            ppoints.y <- ppoints(ny, a = plot.pos.con)
            if (nx > ny) {
                q.x <- approx(ppoints.x, q.x, xout = ppoints.y, 
                  rule = 2)$y
            }
            else {
                q.y <- approx(ppoints.y, q.y, xout = ppoints.x, 
                  rule = 2)$y
            }
        }
        if (plot.it) {
            if (plot.type == "Q-Q") {
                if (is.null(xlab)) 
                  xlab <- paste("Quantiles of", x.name)
                if (is.null(ylab)) 
                  ylab <- paste("Quantiles of", y.name)
                if (is.null(main)) 
                  main <- paste("Q-Q Plot of\n", y.name, "vs.", 
                    x.name)
            }
            else {
                if (is.null(xlab)) 
                  xlab <- "Mean of Quantiles"
                if (is.null(ylab)) 
                  ylab <- paste(y.name, "Quantiles -", x.name, 
                    "Quantiles")
                if (is.null(main)) 
                  main <- paste("Tukey Mean-Difference Q-Q Plot for\n", 
                    x.name, "and", y.name)
            }
        }
    }
    else {
        check.da.list <- check.distribution.args(distribution, 
            check.params = FALSE)
        distribution <- check.da.list$dist.abb
        special <- any(distribution == c("lnorm", "lnorm3", "zmlnorm", 
            "zmnorm"))
        if (!estimate.params) {
            if (distribution != "norm" && !special && missing(param.list)) 
                stop(paste("When 'estimate.params=F' you must supply", 
                  "the argument 'param.list'"))
            check.da.list <- check.distribution.args(distribution, 
                param.list)
        }
        dist.name <- check.da.list$dist.name
        n.dist.params <- check.da.list$n.dist.params
        dist.params.names <- check.da.list$dist.params.names
        zm <- any(distribution == c("zmnorm", "zmlnorm", "zmlnormAlt"))
        if (is.null(plot.pos.con)) {
            plot.pos.con <- switch(distribution, norm = , lnorm = , 
                lnormAlt = , lnorm3 = , zmnorm = , zmlnorm = , 
                zmlnormAlt = 0.375, evd = 0.44, 0.4)
        }
        else {
            if (!is.vector(plot.pos.con, mode = "numeric") || 
                length(plot.pos.con) != 1 || plot.pos.con < 0 || 
                plot.pos.con > 1) 
                stop("'plot.pos.con' must be a numeric scalar between 0 and 1")
        }
        if (estimate.params) {
            if (EnvStats::Distribution.df[distribution, "Estimation.Method(s)"] == 
                "") 
                stop(paste("No estimation method available for the", 
                  dist.name, "Distribution"))
            ename <- paste("e", distribution, sep = "")
            est.param.vec <- do.call(ename, c(list(x = x), est.arg.list))$parameters
            if (dist.params.names[n.dist.params] == "ncp") {
                warning(paste("No estimation method available for", 
                  "Non-Central Distributions.\n "))
                est.param.vec <- c(est.param.vec, 0)
            }
            if (zm) 
                est.param.vec <- est.param.vec[1:n.dist.params]
            param.list <- as.list(est.param.vec)
            names(param.list) <- dist.params.names
        }
        else param.list <- check.da.list$param.list
        distribution.x <- ifelse(special, "norm", distribution)
        dist.name.x <- ifelse(special, "Normal", dist.name)
        if (distribution == "zmlnormAlt") {
            distribution.x <- "lnormAlt"
            dist.name.x <- "Lognormal"
        }
        q.y <- switch(distribution, lnorm = sort(log(x)), lnorm3 = sort(log(x - 
            param.list$threshold)), zmlnorm = sort(log(x[x > 
            0])), zmnorm = , zmlnormAlt = sort(x[x != 0]), sort(x))
        if (zm) 
            nx <- length(q.y)
        param.list.x <- param.list
        if (special) {
            switch(distribution, lnorm = {
            }, lnorm3 = , zmlnorm = param.list.x <- param.list[c("meanlog", 
                "sdlog")], zmnorm = param.list.x <- param.list[c("mean", 
                "sd")])
            names(param.list.x) <- c("mean", "sd")
        }
        if (distribution == "zmlnormAlt") {
            param.list.x <- param.list[c("mean", "cv")]
            names(param.list.x) <- c("mean", "cv")
        }
        qname <- paste("q", distribution.x, sep = "")
        q.x <- do.call(qname, c(list(ppoints(nx, a = plot.pos.con)), 
            param.list.x))
        dist.params.names.x <- names(param.list.x)
        if (plot.it) {
            qlab <- switch(distribution, lnorm = paste("Log[", 
                x.name, "]", sep = ""), lnorm3 = paste("Log[", 
                x.name, "-", format(param.list$threshold, digits = digits), 
                "]"), zmnorm = paste("Non-Zero Values of", x.name), 
                zmlnorm = paste("Log [", x.name, "> 0 ]"), zmlnormAlt = paste(x.name, 
                  "> 0"), x.name)
            if (any(distribution == c("beta", "chisq", "f")) && 
                param.list$ncp == 0) {
                x.string <- paste(dist.name, "(", paste(paste(dist.params.names[-n.dist.params], 
                  signif(unlist(param.list[-n.dist.params]), 
                    digits), sep = " = "), collapse = ", "), 
                  ")", sep = "")
            }
            else {
                x.string <- paste(dist.name.x, "(", paste(paste(dist.params.names.x, 
                  signif(unlist(param.list.x), digits), sep = " = "), 
                  collapse = ", "), ")", sep = "")
            }
            if (plot.type == "Q-Q") {
                if (is.null(xlab)) {
                  xlab <- paste("Quantiles of", x.string)
                }
                if (is.null(ylab)) 
                  ylab <- paste("Quantiles of", qlab)
                if (is.null(main)) 
                  main <- paste(dist.name.x, "Q-Q Plot for", 
                    qlab)
            }
            else {
                if (is.null(xlab)) 
                  xlab <- "Mean of Observed and Fitted Quantiles"
                if (is.null(ylab)) 
                  ylab <- "Observed - Fitted Quantiles"
                if (is.null(main)) 
                  main <- paste("Tukey Mean-Difference Q-Q Plot for ", 
                    qlab, "\nFitted to ", x.string, " Distribution", 
                    sep = "")
            }
        }
    }
    if (plot.type == "Q-Q") {
        if (plot.it) {
            if (is.null(xlim) && is.null(ylim) && equal.axes) {
                xlim <- range(q.x, q.y)
                ylim <- xlim
            }
            else {
                if (is.null(xlim)) 
                  xlim <- range(q.x)
                if (is.null(ylim)) 
                  ylim <- range(q.y)
            }
            plot(q.x, q.y, type = "n", ..., xlab = xlab, ylab = ylab, 
                xlim = xlim, ylim = ylim, main = main)
            arg.list <- c(list(x = q.x, y = q.y, method = duplicate.points.method), 
                gen.gp.list, list(col = points.col))
            do.call("points.w.dups", arg.list)
            if (add.line) 
                switch(qq.line.type, `least squares` = {
                  arg.list <- c(list(a = lm(q.y ~ q.x)), gen.gp.list, 
                    list(col = line.col, lwd = line.lwd, lty = line.lty))
                  do.call("abline", arg.list)
                }, `0-1` = {
                  arg.list <- c(list(a = 0, b = 1), gen.gp.list, 
                    list(col = line.col, lwd = line.lwd, lty = line.lty))
                  do.call("abline", arg.list)
                }, robust = {
                  arg.list <- c(list(x = q.x, y = q.y), gen.gp.list, 
                    list(col = line.col, lwd = line.lwd, lty = line.lty))
                  do.call("qqLine", arg.list)
                })
        }
        ret.list <- list(x = q.x, y = q.y)
    }
    else {
        q.mean <- (q.x + q.y)/2
        q.diff <- q.y - q.x
        if (plot.it) {
            if (is.null(ylim)) {
                rqmo2 <- diff(range(q.mean))/2
                mqd <- median(q.diff)
                ylim.min <- min(min(q.diff), mqd - rqmo2)
                ylim.max <- max(max(q.diff), mqd + rqmo2)
                ylim <- c(ylim.min, ylim.max)
            }
            if (is.null(xlim)) 
                xlim <- range(q.mean)
            plot(q.mean, q.diff, type = "n", ..., xlim = xlim, 
                ylim = ylim, xlab = xlab, ylab = ylab, main = main)
            arg.list <- c(list(x = q.mean, y = q.diff, method = duplicate.points.method), 
                gen.gp.list, list(col = points.col))
            do.call("points.w.dups", arg.list)
            if (add.line) {
                arg.list <- c(list(h = 0), gen.gp.list, list(col = line.col, 
                  lwd = line.lwd, lty = line.lty))
                do.call("abline", arg.list)
            }
        }
        ret.list <- list(x = q.mean, y = q.diff)
    }
    invisible(ret.list)
}

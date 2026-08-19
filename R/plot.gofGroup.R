#' Plot Results of Group Goodness-of-Fit Test
#' @description
#' Plot the results of calling the function \code{\link{gofGroupTest}},
#'   which returns an object of class \code{"gofGroup"} when performing a
#'   goodness-of-fit test to determine whether data in a set of
#'   groups appear to all come from the same probability distribution
#'   (with possibly different parameters for each group).
#'   Five different kinds of plots are available.
#'
#'   The function \code{plot.gofGroup} is automatically called by \code{\link{plot}}
#'   when given an object of class \cr
#'   \code{"gofGroup"}.  The names of other functions
#'   associated with goodness-of-fit test are listed under \link{Goodness-of-Fit Tests}.
#' @usage
#' \method{plot}{gofGroup}(x, plot.type = "Summary",
#'     captions = list(QQ = NULL, MDQQ = NULL, ScoresQQ = NULL, ScoresMDQQ = NULL,
#'       Results = NULL),
#'     x.labels = list(QQ = NULL, MDQQ = NULL, ScoresQQ = NULL, ScoresMDQQ = NULL),
#'     y.labels = list(QQ = NULL, MDQQ = NULL, ScoresQQ = NULL, ScoresMDQQ = NULL),
#'     same.window = FALSE, ask = same.window & plot.type == "All", add.line = TRUE,
#'     digits = ifelse(plot.type == "Summary", 2, .Options$digits), test.result.font = 1,
#'     test.result.cex = ifelse(plot.type == "Summary", 0.9, 1) * par("cex"),
#'     test.result.mar = c(0, 0, 3, 0) + 0.1, individual.p.values = FALSE,
#'     cex.main = ifelse(plot.type == "Summary", 1.2, 1.5) * par("cex"),
#'     cex.axis = ifelse(plot.type == "Summary", 0.9, 1) * par("cex"),
#'     cex.lab = ifelse(plot.type == "Summary", 0.9, 1) * par("cex"),
#'     main = NULL, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, add.om.title = TRUE,
#'     oma = if (plot.type == "Summary" & add.om.title) c(0, 0, 5, 0) else c(0, 0, 0, 0),
#'     om.title = NULL, om.font = 2, om.cex.main = 1.5 * par("cex"), om.line = 1, ...)
#' @rawRd
#' \arguments{
#'   \item{x}{
#'   an object of class \code{"gofGroup"}.  See \code{\link{gofGroup.object}} for details.
#' }
#'   \item{plot.type}{
#'   character string indicating what kind of plot to create.  Only one particular
#'   plot type will be created, unless \code{plot.type="All"}, in which case all plots
#'   will be created sequentially.  The possible values of \code{plot.type} are:
#'   \code{"Summary"} (the default), \code{"Q-Q Plot"} \code{"Tukey M-D Q-Q Plot"},
#'   \code{"Scores Q-Q Plot"}, \code{"Scores Tukey M-D Q-Q Plot"},
#'   \code{"Test Results"}, and \code{"All"}.
#'   See the DETAILS section for more information.
#' }
#'   \item{captions}{
#'   a list with 1 to 5 components with the names \code{"QQ"}, \code{"MDQQ"},
#'   \code{"ScoresQQ"}, \cr
#'   \code{"ScoresMDQQ"}, and/or \code{"Results"}.  Each component either has
#'   the value \code{NULL} or else it is a character string containing the title for that
#'   particular kind of plot.  When the component has the value \code{NULL} (the default),
#'   a default title is used.  This argument is useful when you are creating more than
#'   one kind of plot with a single call to \code{plot.gofGroup} (i.e., when
#'   \code{plot.type="Summary"} or \code{plot.type="All"}) and you want to specify titles
#'   different from the default ones.  If you are creating only one kind of plot,
#'   then you can just use the \code{main} argument to specify a title different
#'   from the default one.
#' }
#'   \item{x.labels}{
#'   a list of 1 to 4 components with the names \code{"QQ"}, \code{"MDQQ"},
#'   \code{"ScoresQQ"}, and/or \code{"ScoresMDQQ"}.
#'   Each component either has the value
#'   \code{NULL} or else it is a character string containing the label for the
#'   \eqn{x}-axis for that particular kind of plot.  When the component has the value
#'   \code{NULL} (the default), a default \eqn{x}-axis label is used.
#'   This argument is useful when you are creating more than
#'   one kind of plot with a single call to \code{plot.gofGroup} (i.e., when
#'   \code{plot.type="Summary"} or \code{plot.type="All"})
#'   and you want to specify \eqn{x}-axis
#'   labels different from the default ones.  If you are creating only one plot,
#'   then you can just use the \code{xlab} argument to
#'   specify an \eqn{x}-axis label different from the default one.
#' }
#'   \item{y.labels}{
#'   a list of 1 to 4 components with the names \code{"QQ"}, \code{"MDQQ"},
#'   \code{"ScoresQQ"}, and/or \code{"ScoresMDQQ"}. Each component either has the value
#'   \code{NULL} or else it is a character string containing the label for the
#'   \eqn{y}-axis for that particular kind of plot.  When the component has the value
#'   \code{NULL} (the default), a default \eqn{y}-axis label is used.
#'   This argument is useful when you are creating more than
#'   one kind of plot with a single call to \code{plot.gofGroup} (i.e., when
#'   \code{plot.type="Summary"} or \code{plot.type="All"})
#'   and you want to specify \eqn{y}-axis
#'   labels different from the default ones.  If you are creating only one plot,
#'   then you can just use the \code{ylab} argument to
#'   specify a \eqn{y}-axis label different from the default one.
#' }
#'   \item{same.window}{
#'   logical scalar indicating whether to produce all plots in the same graphics
#'   window (\code{same.window=TRUE}), or to create a new graphics
#'   window for each separate plot (\code{same.window=FALSE}; the default).
#'   The argument is relevant only when \code{plot.type="All"}.
#' }
#'   \item{ask}{
#'   logical scalar supplied to the function \code{\link{devAskNewPage}}, indicating
#'   whether to prompt the user before creating a new plot within a single graphics
#'   window.  The default value is \code{FALSE} unless \code{same.window=TRUE} and
#'   \code{plot.type == "All"}.
#' }
#'   \item{add.line}{
#'   logical scalar indicating whether to add a line to the plot.  If \code{add.line=TRUE}
#'   and \code{plot.type="Q-Q Plot"} or \code{plot.type="Scores Q-Q Plot"},
#'   a 0-1 line is added to the plot.
#'   If \code{add.line=TRUE} and \code{plot.type="Tukey M-D Q-Q Plot"} or
#'   \code{plot.type="Scores Tukey M-D Q-Q Plot"}, a horizontal
#'   line at \eqn{y=0} is added to the plot.  The default value is \code{add.line=TRUE}.
#'   This argument is ignored if \code{plot.type="Test Results"}.
#' }
#'
#'
#'
#' \bold{Arguments associated with \code{plot.type="Test Results"}} \cr
#'
#'   \item{digits}{
#'   scalar indicating how many significant digits to print for the test results
#'   when \code{plot.type="Summary"} or \code{plot.type="Test Results"}.  If \cr
#'   \code{plot.type == "Summary"}, the default value is
#'   \code{digits=2}, otherwise it is \cr
#'   \code{.Options$digits} (i.e., the current setting of \code{options("digits")}).
#' }
#'   \item{individual.p.values}{
#'   logical scalar indicating whether to display the p-values associated with
#'   each individual group.  The default value is \code{individual.p.values=FALSE}.
#' }
#'   \item{test.result.font}{
#'   numeric scalar indicating which font to use to print out the test results.
#'   The default value is \code{test.result.font=1}.  See the description of the
#'   \code{font} argument in the help file for \code{\link{par}} for more information.
#'   You may get better results if you use a font number that corresponds to a fixed
#'   font (e.g., courier).
#' }
#'   \item{test.result.cex}{
#'   numeric scalar indicating the value of \code{cex} to use to print out the
#'   test results.  The default value is \code{0.9*par("cex")} when
#'   \code{plot.type="Summary"}, otherwise it is \code{par("cex")}.
#'   See the description of the \code{cex} argument in the help file for
#'   \code{\link{par}} for more information.
#' }
#'   \item{test.result.mar}{
#'   numeric vector indicating the value of \code{mar} to use to print out the
#'   test results.  The default value is \code{test.result.mar=c(0, 0, 3, 0)+0.1}.
#'   See the description of the \code{mar} argument in the help file for
#'   \code{\link{par}} for more information.
#' }
#'
#'
#'
#' \bold{Arguments associated with \code{plot.type="Summary"}} \cr
#'
#'   \item{add.om.title}{
#'   logical scalar indicating whether to add a title in the outer margin when \cr
#'   \code{plot.type="Summary"}.  The default value is \code{add.om.title=TRUE}.
#' }
#'   \item{om.title}{
#'   character string containing the outer margin title.  The default value is \cr
#'   \code{om.title=NULL}, which will result in a default title.
#' }
#'   \item{om.font}{
#'   numeric scalar indicating the font to use for the outer margin.  The default
#'   value is \code{om.font=2}.
#' }
#'   \item{om.cex.main}{
#'   numeric scalar indicating the value of \code{cex} for the outer margin title.
#'   The default value is \code{1.5 * par("cex")}.
#' }
#'   \item{om.line}{
#'   numeric scalar indicating the line to place the outer margin title on.  The
#'   default value is \code{om.line=1}.
#' }
#'
#'
#'
#' \bold{Graphics parameters:} \cr
#'
#'   \item{cex.main, cex.axis, cex.lab, main, xlab, ylab, xlim, ylim, oma, \dots}{
#'   additional graphics parameters.  See the help file for \code{\link{par}}.
#' }
#' }
#' @rawRd
#' \details{
#'   The function \code{plot.gofGroup} is a method for the generic function
#'   \code{\link{plot}} for the class \code{"gofGroup"} (see
#'   \code{\link{gofGroup.object}}).
#'   It can be invoked by calling \code{\link{plot}} and giving it an object of
#'   class \code{"gofGroup"} as the first argument, or by calling
#'   \code{plot.gofGroup} directly, regardless of the class of the object given
#'   as the first argument to \code{plot.gofGroup}.
#'
#'   Plots associated with the goodness-of-fit test are produced on the current graphics
#'   device.  These can be one or all of the following:
#'   \itemize{
#'   \item \code{plot.type="Q-Q Plot"}.
#'     Q-Q Plot of observed p-values vs. quantiles from a
#'     \link[stats:Uniform]{Uniform [0,1] distribution}.
#'     See the help file for \code{\link{qqPlot}}.
#'   \item \code{plot.type="Tukey M-D Q-Q Plot"}.
#'     Tukey mean-difference Q-Q plot for observed p-values and
#'     quantiles from a \link[stats:Uniform]{Uniform [0,1] distribution}.
#'     See the help file for \code{\link{qqPlot}}.
#'   \item \code{plot.type="Scores Q-Q Plot"}.
#'     Q-Q Plot of Normal scores vs. quantiles from a
#'     \link[stats:Normal]{Normal(0,1) distribution} or
#'     Q-Q Plot of Chisquare scores vs. quantiles from a
#'     \link[stats:Chisquare]{Chisquare distribution} with 2 degrees of freedom.
#'     See the help file for \code{\link{qqPlot}}.
#'   \item \code{plot.type="Scores Tukey M-D Q-Q Plot"}.
#'     Tukey mean-difference Q-Q plot based on Normal scores or
#'     Chisquare scores.
#'     See the help file for \code{\link{qqPlot}}.
#'   \item Results of the goodness-of-fit test (\code{plot.type="Test Results"}).
#'     See the help file for \code{\link{print.gofGroup}}.
#'   }
#'   See the help file for \code{\link{gofGroupTest}} for more information.
#' }
#' @rawRd
#' \value{
#'   \code{plot.gofGroup} invisibly returns the first argument, \code{x}.
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
#'   \code{\link{gofGroupTest}}, \code{\link{gofGroup.object}},
#'   \code{\link{print.gofGroup}},
#'   \link{Goodness-of-Fit Tests}, \code{\link{plot}}.
#' }
#' @rawRd
#' \examples{
#'   # Create an object of class "gofGroup" then plot it.
#'
#'   # Example 10-4 of USEPA (2009, page 10-20) gives an example of
#'   # simultaneously testing the assumption of normality for nickel
#'   # concentrations (ppb) in groundwater collected at 4 monitoring
#'   # wells over 5 months.  The data for this example are stored in
#'   # EPA.09.Ex.10.1.nickel.df.
#'
#'   EPA.09.Ex.10.1.nickel.df
#'   #   Month   Well Nickel.ppb
#'   #1      1 Well.1       58.8
#'   #2      3 Well.1        1.0
#'   #3      6 Well.1      262.0
#'   #...
#'   #18     6 Well.4       85.6
#'   #19     8 Well.4       10.0
#'   #20    10 Well.4      637.0
#'
#'
#'   # Test for a normal distribution at each well:
#'   #--------------------------------------------
#'
#'   gofGroup.obj <- gofGroupTest(Nickel.ppb ~ Well,
#'     data = EPA.09.Ex.10.1.nickel.df)
#'
#'   dev.new()
#'   plot(gofGroup.obj)
#'
#'   # Make your own titles for the summary plot
#'   #------------------------------------------
#'   dev.new()
#'   plot(gofGroup.obj, captions = list(QQ = "Q-Q Plot",
#'     ScoresQQ = "Scores Q-Q Plot", Results = "Results"),
#'     om.title = "Summary Plot")
#'
#'   # Just the Q-Q Plot
#'   #------------------
#'   dev.new()
#'   plot(gofGroup.obj, plot.type="Q-Q")
#'
#'
#'   # Make your own title for the Q-Q Plot
#'   #-------------------------------------
#'   dev.new()
#'   plot(gofGroup.obj, plot.type="Q-Q", main = "Q-Q Plot")
#'
#'   #==========
#'
#'   # Clean up
#'   #---------
#'   rm(gofGroup.obj)
#'   graphics.off()
#' }
#' @rawRd
#' \keyword{plot}

plot.gofGroup <-
function (x, plot.type = "Summary", captions = list(QQ = NULL, 
    MDQQ = NULL, ScoresQQ = NULL, ScoresMDQQ = NULL, Results = NULL), 
    x.labels = list(QQ = NULL, MDQQ = NULL, ScoresQQ = NULL, 
        ScoresMDQQ = NULL), y.labels = list(QQ = NULL, MDQQ = NULL, 
        ScoresQQ = NULL, ScoresMDQQ = NULL), same.window = FALSE, 
    ask = same.window & plot.type == "All", add.line = TRUE, 
    digits = ifelse(plot.type == "Summary", 2, .Options$digits), 
    test.result.font = 1, test.result.cex = ifelse(plot.type == 
        "Summary", 0.9, 1) * par("cex"), test.result.mar = c(0, 
        0, 3, 0) + 0.1, individual.p.values = FALSE, cex.main = ifelse(plot.type == 
        "Summary", 1.2, 1.5) * par("cex"), cex.axis = ifelse(plot.type == 
        "Summary", 0.9, 1) * par("cex"), cex.lab = ifelse(plot.type == 
        "Summary", 0.9, 1) * par("cex"), main = NULL, xlab = NULL, 
    ylab = NULL, xlim = NULL, ylim = NULL, add.om.title = TRUE, 
    oma = if (plot.type == "Summary" & add.om.title) c(0, 0, 
        5, 0) else c(0, 0, 0, 0), om.title = NULL, om.font = 2, 
    om.cex.main = 1.5 * par("cex"), om.line = 1, ...) 
{
    gof.obj <- x
    plot.type <- match.arg(plot.type, c("Summary", "All", "Q-Q Plot", 
        "Tukey M-D Q-Q Plot", "Scores Q-Q Plot", "Scores Tukey M-D Q-Q Plot", 
        "Test Results"))
    check.gp.list <- checkGraphicsPars(...)
    gp.arg.list <- check.gp.list$gp.arg.list
    gen.gp.list <- check.gp.list$gen.gp.list
    data.name <- gof.obj$data.name
    data.name.string <- data.name
    parent.of.data <- gof.obj$parent.of.data
    if (!is.null(parent.of.data)) 
        data.name.string <- paste(data.name, "in", parent.of.data)
    grouping.variable <- gof.obj$grouping.variable
    user.main <- main
    user.xlab <- xlab
    user.ylab <- ylab
    user.xlim <- xlim
    user.ylim <- ylim
    if (!missing(captions)) {
        if (!is.list(captions)) 
            stop("The argument 'captions' must be a list")
        len <- length(captions)
        if (len < 1 | len > 5) 
            stop("The argument 'captions' must be a list with 1 to 5 components")
        if (!all(sapply(captions, length) == 1) || !all(sapply(captions, 
            is.character))) 
            stop("All components of the argument 'captions' must be character strings")
        names.vec <- names(captions)
        if (!all(names.vec %in% c("QQ", "MDQQ", "ScoresQQ", "ScoresMDQQ", 
            "Results"))) 
            stop(paste("All components of the argument 'captions'", 
                "must have names, and the name must be one of", 
                "\"QQ\", \"MDQQ\", \"ScoresQQ\", \"ScoresMDQQ\", or \"Results\""))
        if (length(unique(names.vec)) != length(names.vec)) 
            stop(paste("The names of the components for the argument 'captions'", 
                "must be unique"))
        old.captions <- captions
        captions <- list(QQ = NULL, MDQQ = NULL, ScoresQQ = NULL, 
            ScoresMDQQ = NULL, Results = NULL)
        captions[names.vec] <- old.captions
    }
    if (!missing(x.labels)) {
        if (!is.list(x.labels)) 
            stop("The argument 'x.labels' must be a list")
        len <- length(x.labels)
        if (len < 1 | len > 4) 
            stop("The argument 'x.labels' must be a list with 1 to 4 components")
        if (!all(sapply(x.labels, length) == 1) || !all(sapply(x.labels, 
            is.character))) 
            stop("All components of the argument 'x.labels' must be character strings")
        names.vec <- names(x.labels)
        if (!all(names.vec %in% c("QQ", "MDQQ", "ScoresQQ", "ScoresMDQQ"))) 
            stop(paste("All components of the argument 'x.labels'", 
                "must have names, and the name must be one of", 
                "\"QQ\", \"MDQQ\", \"ScoresQQ\", or \"ScoresMDQQ\""))
        if (length(unique(names.vec)) != length(names.vec)) 
            stop(paste("The names of the components for the argument 'x.labels'", 
                "must be unique"))
        old.x.labels <- x.labels
        x.labels <- list(QQ = NULL, MDQQ = NULL, ScoresQQ = NULL, 
            ScoresMDQQ = NULL)
        x.labels[names.vec] <- old.x.labels
    }
    if (!missing(y.labels)) {
        if (!is.list(y.labels)) 
            stop("The argument 'y.labels' must be a list")
        len <- length(y.labels)
        if (len < 1 | len > 4) 
            stop("The argument 'y.labels' must be a list with 1 to 4 components")
        if (!all(sapply(y.labels, length) == 1) || !all(sapply(y.labels, 
            is.character))) 
            stop("All components of the argument 'y.labels' must be character strings")
        names.vec <- names(y.labels)
        if (!all(names.vec %in% c("QQ", "MDQQ", "ScoresQQ", "ScoresMDQQ"))) 
            stop(paste("All components of the argument 'y.labels'", 
                "must have names, and the name must be one of", 
                "\"QQ\", \"MDQQ\", \"ScoresQQ\", or \"ScoresMDQQ\""))
        if (length(unique(names.vec)) != length(names.vec)) 
            stop(paste("The names of the components for the argument 'y.labels'", 
                "must be unique"))
        old.y.labels <- y.labels
        y.labels <- list(QQ = NULL, MDQQ = NULL, ScoresQQ = NULL, 
            ScoresMDQQ = NULL)
        y.labels[names.vec] <- old.y.labels
    }
    if (is.element(plot.type, c("Summary", "All", "Q-Q Plot"))) {
        if (plot.type == "All" & same.window) {
            devAskNewPage(ask = ask)
        }
        else if (plot.type == "Summary") {
            o.par1 <- par(c("cex", "mex", "mgp"))
            o.par2 <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1) + 
                0.1, oma = oma)
            par(cex = 0.8 * o.par1$cex, mex = 0.8 * o.par1$mex, 
                mgp = c(2.75, 0.5, 0))
            on.exit(par(c(o.par1, o.par2)))
        }
        if (is.null(user.xlab) & !is.null(x.labels[[1]])) 
            xlab <- x.labels[[1]]
        else xlab <- user.xlab
        if (is.null(user.ylab)) {
            if (!is.null(y.labels[[1]])) 
                ylab <- y.labels[[1]]
            else {
                ylab <- paste("Quantiles of P-values")
            }
        }
        else ylab <- user.ylab
        qqPlot(gof.obj$p.value[1:gof.obj$n.groups], distribution = "unif", 
            param.list = list(min = 0, max = 1), digits = digits, 
            add.line = add.line, qq.line.type = "0-1", ..., cex.axis = cex.axis, 
            cex.lab = cex.lab, xlab = user.xlab, ylab = ylab, 
            main = "", xlim = user.xlim, ylim = user.ylim)
        if (is.null(user.main)) {
            if (!is.null(captions[[1]])) 
                main <- captions[[1]]
            else {
                main <- paste("Quantile-Quantile Plot of P-values for\n", 
                  paste(data.name, "Grouped by", grouping.variable))
            }
        }
        arg.list <- c(list(cex.main = cex.main), gen.gp.list, 
            list(main = main))
        do.call("title", arg.list)
    }
    if (is.element(plot.type, c("All", "Tukey M-D Q-Q Plot"))) {
        if (plot.type == "All" & !same.window) 
            dev.new()
        if (is.null(user.xlab) & !is.null(x.labels[[2]])) 
            xlab <- x.labels[[2]]
        else xlab <- user.xlab
        if (is.null(user.ylab) & !is.null(y.labels[[2]])) 
            ylab <- y.labels[[2]]
        else ylab <- user.ylab
        qqPlot(gof.obj$p.value[1:gof.obj$n.groups], distribution = "unif", 
            param.list = list(min = 0, max = 1), plot.type = "Tukey Mean-Difference Q-Q", 
            digits = digits, add.line = add.line, ..., cex.axis = cex.axis, 
            cex.lab = cex.lab, xlab = user.xlab, ylab = ylab, 
            main = "", xlim = user.xlim, ylim = user.ylim)
        if (is.null(user.main)) {
            if (!is.null(captions[[2]])) 
                main <- captions[[2]]
            else main <- paste("Tukey Mean-Difference Q-Q Plot of P-values for\n", 
                paste(data.name, "Grouped by", grouping.variable))
        }
        arg.list <- c(list(cex.main = cex.main), gen.gp.list, 
            list(main = main))
        do.call("title", arg.list)
    }
    if (is.element(plot.type, c("Summary", "All", "Scores Q-Q Plot"))) {
        method <- gof.obj$method
        n.char <- nchar(method)
        score.type <- ifelse(substr(method, n.char - 13, n.char - 
            1) == "Normal Scores", "Normal", "Chi-square")
        if (plot.type == "All" & !same.window) 
            dev.new()
        if (is.null(user.xlab) & !is.null(x.labels[[3]])) 
            xlab <- x.labels[[3]]
        else xlab <- user.xlab
        if (is.null(user.ylab)) {
            if (!is.null(y.labels[[3]])) 
                ylab <- y.labels[[3]]
            else {
                ylab <- paste("Quantiles of the", score.type, 
                  "Scores")
            }
        }
        else ylab <- user.ylab
        if (score.type == "Normal") {
            qqPlot(gof.obj$group.scores, distribution = "norm", 
                param.list = list(mean = 0, sd = 1), digits = digits, 
                add.line = add.line, qq.line.type = "0-1", ..., 
                cex.axis = cex.axis, cex.lab = cex.lab, xlab = user.xlab, 
                ylab = ylab, main = "", xlim = user.xlim, ylim = user.ylim)
        }
        else {
            qqPlot(gof.obj$group.scores, distribution = "chisq", 
                param.list = list(df = 2), digits = digits, add.line = add.line, 
                qq.line.type = "0-1", ..., cex.axis = cex.axis, 
                cex.lab = cex.lab, xlab = user.xlab, ylab = ylab, 
                main = "", xlim = user.xlim, ylim = user.ylim)
        }
        if (is.null(user.main)) {
            if (!is.null(captions[[3]])) 
                main <- captions[[3]]
            else {
                main <- paste("Quantile-Quantile Plot of ", score.type, 
                  " Scores for\n", paste(data.name, "Grouped by", 
                    grouping.variable), sep = "")
            }
        }
        arg.list <- c(list(cex.main = cex.main), gen.gp.list, 
            list(main = main))
        do.call("title", arg.list)
    }
    if (is.element(plot.type, c("All", "Scores Tukey M-D Q-Q Plot"))) {
        method <- gof.obj$method
        n.char <- nchar(method)
        score.type <- ifelse(substr(method, n.char - 13, n.char - 
            1) == "Normal Scores", "Normal", "Chi-square")
        if (plot.type == "All" & !same.window) 
            dev.new()
        if (is.null(user.xlab) & !is.null(x.labels[[4]])) 
            xlab <- x.labels[[4]]
        else xlab <- user.xlab
        if (is.null(user.ylab) & !is.null(y.labels[[4]])) 
            ylab <- y.labels[[4]]
        else ylab <- user.ylab
        if (score.type == "Normal") {
            qqPlot(gof.obj$group.scores, distribution = "norm", 
                param.list = list(mean = 0, sd = 1), plot.type = "Tukey Mean-Difference Q-Q", 
                add.line = add.line, digits = digits, ..., cex.axis = cex.axis, 
                cex.lab = cex.lab, main = "", xlab = user.xlab, 
                ylab = user.ylab, xlim = user.xlim, ylim = user.ylim)
        }
        else {
            qqPlot(gof.obj$group.scores, distribution = "chisq", 
                param.list = list(df = 2), plot.type = "Tukey Mean-Difference Q-Q", 
                add.line = add.line, digits = digits, ..., cex.axis = cex.axis, 
                cex.lab = cex.lab, main = "", xlab = user.xlab, 
                ylab = user.ylab, xlim = user.xlim, ylim = user.ylim)
        }
        if (is.null(user.main)) {
            if (!is.null(captions[[2]])) 
                main <- captions[[2]]
            else main <- paste("Tukey Mean-Difference Q-Q Plot of ", 
                score.type, " Scores for\n", paste(data.name, 
                  "Grouped by", grouping.variable), sep = "")
        }
        arg.list <- c(list(cex.main = cex.main), gen.gp.list, 
            list(main = main))
        do.call("title", arg.list)
    }
    if (is.element(plot.type, c("Summary", "All", "Test Results"))) {
        if (plot.type == "All" & !same.window) 
            dev.new()
        par(usr = c(0, 1, 0, 1))
        o.mar <- par(mar = test.result.mar)
        plot(0:1, 0:1, type = "n", axes = FALSE, main = "")
        if (is.null(user.main)) {
            if (!is.null(captions[[5]])) 
                main <- captions[[5]]
            else main <- paste("Results of", gof.obj$method)
        }
        arg.list <- c(list(cex.main = cex.main), gen.gp.list, 
            list(main = main))
        do.call("title", arg.list)
        o.par <- par(cex = test.result.cex, font = test.result.font)
        char.ht <- par("cxy")[2] * test.result.cex
        text(0, 1, "Hypothesized\nDistribution:", adj = c(0, 
            1))
        text(1, 1 - char.ht, gof.obj$distribution, adj = c(1, 
            1))
        mf <- 3
        if (!is.null(gof.obj$n.param.est) && gof.obj$n.param.est > 
            0) {
            text(0, 1 - mf * char.ht, "Estimated Parameters:", 
                adj = c(0, 1))
            text(1, 1 - mf * char.ht, paste(format(names(gof.obj$distribution.parameters), 
                justify = "left"), " = ", format(format(gof.obj$distribution.parameters, 
                digits = digits, nsmall = 0)), "\n", sep = "", 
                collapse = ""), adj = c(1, 1))
            mf <- mf + 2 + length(gof.obj$distribution.parameters) - 
                1
        }
        text(0, 1 - mf * char.ht, "Data:", adj = 0)
        text(1, 1 - mf * char.ht, gof.obj$data.name, adj = 1)
        mf <- mf + 2
        text(0, 1 - mf * char.ht, "Grouped With:", adj = 0)
        text(1, 1 - mf * char.ht, grouping.variable, adj = 1)
        mf <- mf + 2
        if (!is.null(gof.obj$subset.expression)) {
            text(0, 1 - mf * char.ht, "Subset With:", adj = 0)
            text(1, 1 - mf * char.ht, gof.obj$subset.expression, 
                adj = 1)
            mf <- mf + 2
        }
        if (!is.null(gof.obj$parent.of.data)) {
            text(0, 1 - mf * char.ht, "Data Source:", adj = 0)
            text(1, 1 - mf * char.ht, gof.obj$parent.of.data, 
                adj = 1)
            mf <- mf + 2
        }
        text(0, 1 - mf * char.ht, "Number of Groups:", adj = 0)
        text(1, 1 - mf * char.ht, gof.obj$n.groups, adj = 1)
        mf <- mf + 2
        text(0, 1 - mf * char.ht, "Test Statistic:", adj = 0)
        text(1, 1 - mf * char.ht, paste(format(names(gof.obj$statistic), 
            justify = "left"), " = ", format(format(gof.obj$statistic, 
            digits = digits, nsmall = 0)), sep = "", collapse = ""), 
            adj = 1)
        mf <- mf + 2 + length(gof.obj$statistic) - 1
        if (!is.null(gof.obj$parameters)) {
            text(0, 1 - mf * char.ht, "Test Statistic Parmeter:", 
                adj = 0)
            text(1, 1 - mf * char.ht, paste(format(names(gof.obj$parameters), 
                justify = "left"), format(gof.obj$parameters, 
                digits = digits, nsmall = 0), sep = " = "), adj = 1)
            mf <- mf + 2
        }
        if (individual.p.values) {
            index <- 1:gof.obj$n.groups
            text(0, 1 - mf * char.ht, "Individual P-values:", 
                adj = c(0, 1))
            text(1, 1 - mf * char.ht, paste(format(names(gof.obj$p.value[index]), 
                justify = "left"), " = ", format(format(gof.obj$p.value[index], 
                digits = digits, nsmall = 0)), "\n", sep = "", 
                collapse = ""), adj = c(1, 1))
            mf <- mf + 2 + gof.obj$n.groups
        }
        index <- 1 + gof.obj$n.groups
        text(0, 1 - mf * char.ht, "P-value:", adj = 0)
        text(1, 1 - mf * char.ht, format(gof.obj$p.value[index], 
            digits = digits, nsmall = 0), adj = 1)
        par(c(o.mar, o.par))
    }
    if (plot.type == "Summary" & add.om.title) {
        if (is.null(om.title)) {
            string1 <- paste("Results of", gof.obj$method, "for", 
                gof.obj$distribution, "Distribution for")
            string2 <- paste(data.name.string, "Grouped with ", 
                grouping.variable)
            om.title <- paste(string1, string2, sep = "\n")
        }
        mtext(om.title, side = 3, line = om.line, outer = TRUE, 
            cex = om.cex.main, font = om.font)
    }
    invisible(gof.obj)
}

geom_stripchart <-
function (..., seed = 47, paired = FALSE, paired.lines = paired,
    group = NULL, x.nudge = if (paired && paired.lines) c(-0.3,
        0.3) else 0.3, text.box = FALSE, location = "mean", ci = "normal",
    digits = 1, digit.type = "round", nsmall = ifelse(digit.type ==
        "round", digits, 0), jitter.params = list(), point.params = list(),
    line.params = list(), location.params = list(), errorbar.params = list(),
    n.text = TRUE, n.text.box = text.box, n.text.params = list(),
    location.scale.text = TRUE, location.scale.text.box = text.box,
    location.scale.text.params = list(), test.text = FALSE, test.text.box = text.box,
    test = ifelse(location == "mean", "parametric", "nonparametric"),
    test.text.params = list())
{
    location <- match.arg(location, c("mean", "median"))
    ci <- match.arg(ci, c("normal", "boot"))
    test <- match.arg(test, c("parametric", "nonparametric"))
    params <- list(...)
    if (paired && is.null(group))
        stop("When paired=TRUE, you must supply the argument 'group'.")
    if (paired && paired.lines) {
        point.params <- {
            temp <- modifyList(params, list(pch = 1))
            modifyList(temp, point.params)
        }
        line.params <- {
            temp <- modifyList(params, list(mapping = ggplot2::aes_(group = as.name(group)),
                color = "gray"))
            modifyList(temp, line.params)
        }
    }
    else {
        jitter.params <- {
            temp <- modifyList(params, list(pch = 1, width = 0.15,
                height = 0))
            modifyList(temp, jitter.params)
        }
    }
    location.params <- {
        temp <- modifyList(params, list(size = 2, position = ggplot2::position_nudge(x = x.nudge)))
        modifyList(temp, location.params)
    }
    errorbar.params <- {
        temp <- modifyList(params, list(fun.args = list(conf.int = ifelse(location ==
            "mean", 0.95, 0.5)), size = 0.75, width = 0.075,
            position = ggplot2::position_nudge(x = x.nudge)))
        modifyList(temp, errorbar.params)
    }
    n.text.params <- {
        temp <- list(y.pos = NULL, y.expand.factor = 0.1, text.box = n.text.box,
            alpha = 1, angle = 0, color = "black", family = "",
            fontface = "plain", hjust = 0.5, lineheight = 1.2,
            size = 4, vjust = 0.5)
        if (n.text.box) {
            temp <- c(temp, list(label.padding = ggplot2::unit(0.25,
                "lines"), label.r = ggplot2::unit(0.15, "lines"),
                label.size = 0.25))
        }
        temp <- modifyList(params, temp)
        modifyList(temp, n.text.params)
    }
    location.scale.text.params <- {
        temp <- list(y.pos = NULL, y.expand.factor = 0.2, digits = digits,
            digit.type = digit.type, nsmall = nsmall, text.box = location.scale.text.box,
            alpha = 1, angle = 0, color = "black", family = "",
            fontface = "plain", hjust = 0.5, lineheight = 1.2,
            size = 4, vjust = 0.5)
        if (location.scale.text.box) {
            temp <- c(temp, list(label.padding = ggplot2::unit(0.25,
                "lines"), label.r = ggplot2::unit(0.15, "lines"),
                label.size = 0.25))
        }
        temp <- modifyList(params, temp)
        modifyList(temp, location.scale.text.params)
    }
    test.text.params <- {
        temp <- list(y.pos = NULL, y.expand.factor = 0.35, test = test,
            paired = paired, test.arg.list = list(), two.lines = TRUE,
            p.value.digits = 3, p.value.digit.type = digit.type,
            location.digits = digits, location.digit.type = digit.type,
            nsmall = nsmall, text.box = test.text.box, alpha = 1,
            angle = 0, color = "black", family = "", fontface = "plain",
            hjust = 0.5, lineheight = 1.2, size = 4, vjust = 0.5)
        if (test.text.box) {
            temp <- c(temp, list(label.padding = ggplot2::unit(0.25,
                "lines"), label.r = ggplot2::unit(0.15, "lines"),
                label.size = 0.25))
        }
        if (paired) {
            temp <- c(list(mapping = ggplot2::aes_(group = as.name(group))),
                temp)
        }
        temp <- modifyList(params, temp)
        modifyList(temp, test.text.params)
    }
    set.seed(seed)
    if (paired && paired.lines) {
        point <- do.call(ggplot2::geom_point, point.params)
        line <- do.call(ggplot2::geom_line, line.params)
    }
    else {
        jitter <- do.call(ggplot2::geom_jitter, jitter.params)
    }
    loc <- do.call(ggplot2::stat_summary, modifyList(list(fun = location,
        geom = "point"), location.params))
    if (location == "mean") {
        if (ci == "normal")
            fun.data <- "mean_cl_normal"
        else fun.data <- "mean_cl_boot"
    }
    else {
        fun.data <- "median_hilow"
    }
    errorbar <- do.call(ggplot2::stat_summary, modifyList(list(fun.data = fun.data,
        geom = "errorbar"), errorbar.params))
    stripchart.list <- list(ggplot2::theme(legend.position = "none"))
    if (paired && paired.lines) {
        stripchart.list <- c(stripchart.list, point, line)
    }
    else {
        stripchart.list <- c(stripchart.list, jitter)
    }
    stripchart.list <- c(stripchart.list, loc, errorbar)
    if (n.text) {
        n.text <- do.call(stat_n_text, n.text.params)
        stripchart.list <- c(stripchart.list, n.text)
    }
    if (location.scale.text) {
        text.fcn <- ifelse(location == "mean", "stat_mean_sd_text",
            "stat_median_iqr_text")
        location.scale.text <- do.call(text.fcn, location.scale.text.params)
        stripchart.list <- c(stripchart.list, location.scale.text)
    }
    if (test.text) {
        test.text <- do.call(stat_test_text, test.text.params)
        stripchart.list <- c(stripchart.list, test.text)
    }
    stripchart.list
}

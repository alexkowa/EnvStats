stripChart.default <-
function (x, method = "stack", seed = 47, jitter = 0.1 * cex, 
    offset = 1/2, vertical = TRUE, group.names, group.names.cex = cex, 
    drop.unused.levels = TRUE, add = FALSE, at = NULL, xlim = NULL, 
    ylim = NULL, ylab = NULL, xlab = NULL, dlab = "", glab = "", 
    log = "", pch = 1, col = par("fg"), cex = par("cex"), points.cex = cex, 
    axes = TRUE, frame.plot = axes, show.ci = TRUE, location.pch = 16, 
    location.cex = cex, conf.level = 0.95, min.n.for.ci = 2, 
    ci.offset = 3/ifelse(n > 2, (n - 1)^(1/3), 1), ci.bar.ends = TRUE, 
    ci.bar.ends.size = 0.5 * cex, ci.bar.gap = FALSE, n.text = "bottom", 
    n.text.line = ifelse(n.text == "bottom", 2, 0), n.text.cex = cex, 
    location.scale.text = "top", location.scale.digits = 1, location.scale.text.line = ifelse(location.scale.text == 
        "top", 0, 3.5), location.scale.text.cex = cex * 0.8 * 
        ifelse(n > 6, max(0.4, 1 - (n - 6) * 0.06), 1), p.value = FALSE, 
    p.value.digits = 3, p.value.line = 2, p.value.cex = cex, 
    group.difference.ci = p.value, group.difference.conf.level = 0.95, 
    group.difference.digits = location.scale.digits, ci.and.test = "parametric", 
    ci.arg.list = NULL, test.arg.list = NULL, alternative = "two.sided", 
    ...) 
{
    method <- pmatch(method, c("overplot", "jitter", "stack"))[1L]
    if (is.na(method) || method == 0L) 
        stop("invalid plotting method")
    if (method == "jitter") 
        set.seed(seed)
    n.text <- match.arg(n.text, c("bottom", "top", "none"))
    if (!vertical & n.text != "none") 
        stop("You must set vertical=T when n.text != \"none\"")
    location.scale.text <- match.arg(location.scale.text, c("none", 
        "top", "bottom"))
    if (!vertical & location.scale.text != "none") 
        stop("You must set vertical=T when location.scale.text != \"none\"")
    if (show.ci && !(length(conf.level) == 1 & is.numeric(conf.level) & 
        conf.level > 0 & conf.level < 1)) 
        stop("'conf.level' must be a single number between 0 and 1")
    if (min.n.for.ci != round(min.n.for.ci) || min.n.for.ci < 
        2) 
        stop("'min.n.for.ci' must be an integer greater than 1")
    groups <- if (is.list(x)) 
        x
    else if (is.numeric(x)) 
        list(x)
    if (0L == (n <- length(groups))) 
        stop("invalid first argument")
    if (drop.unused.levels) {
        groups.N <- sapply(groups, length)
        groups <- groups[groups.N > 0]
        n <- length(groups)
    }
    if (!missing(group.names)) {
        group.names <- as.character(group.names)
        if (length(group.names) != n) 
            stop(paste("'group.names' must have the same number of elements as the number of groups.", 
                "Note that drop.unused.levels =", drop.unused.levels))
        attr(groups, "names") <- group.names
    }
    else if (is.null(attr(groups, "names"))) 
        attr(groups, "names") <- 1L:n
    group.names <- names(groups)
    if (is.null(at)) 
        at <- 1L:n
    else if (length(at) != n) 
        stop(gettextf("'at' must have length equal to the number %d of groups", 
            n), domain = NA)
    if (is.null(dlab)) 
        dlab <- deparse(substitute(x))
    if (!add) {
        dlim <- c(NA, NA)
        for (i in groups) dlim <- range(dlim, i[is.finite(i)], 
            na.rm = TRUE)
        glim <- c(1L, n)
        if (method == 1L & show.ci) 
            glim <- glim + if (n == 1L) 
                c(-1, 1)
            else c(0, 0.5)
        if (method == 2L) {
            glim <- glim + jitter * if (n == 1L) 
                c(-5, 5)
            else c(-2, 2)
        }
        else if (method == 3L) {
            glim <- glim + if (n == 1L) 
                c(-1, 1)
            else c(0, 0.5)
        }
        if (is.null(xlim)) 
            xlim <- if (vertical) 
                glim
            else dlim
        if (is.null(ylim)) 
            ylim <- if (vertical) 
                dlim
            else glim
        plot(xlim, ylim, type = "n", ann = FALSE, axes = FALSE, 
            log = log, ...)
        if (frame.plot) 
            box(...)
        if (vertical) {
            if (axes) {
                if (n > 1L) 
                  axis(1, at = at, labels = group.names, cex.axis = group.names.cex, 
                    ...)
                Axis(x, side = 2, ...)
            }
            if (is.null(ylab)) 
                ylab <- dlab
            if (is.null(xlab)) 
                xlab <- glab
        }
        else {
            if (axes) {
                Axis(x, side = 1, ...)
                if (n > 1L) 
                  axis(2, at = at, labels = group.names, cex.axis = group.names.cex, 
                    ...)
            }
            if (is.null(xlab)) 
                xlab <- dlab
            if (is.null(ylab)) 
                ylab <- glab
        }
        title(xlab = xlab, ylab = ylab, ...)
    }
    csize <- cex * if (vertical) 
        xinch(par("cin")[1L])
    else yinch(par("cin")[2L])
    n.vec <- rep.int(as.numeric(NA), n)
    location.vec <- rep.int(as.numeric(NA), n)
    scale.vec <- location.vec
    ci.mat <- matrix(as.numeric(NA), nrow = n, ncol = 3)
    ci.and.test <- match.arg(ci.and.test, c("parametric", "nonparametric"))
    if (ci.and.test == "parametric") {
        ci.fcn <- "t.test"
        location.fcn <- "mean"
        scale.fcn <- "sd"
    }
    else {
        ci.fcn <- "wilcox.test"
        location.fcn <- "median"
        scale.fcn <- "iqr"
    }
    if (ci.and.test == "nonparametric") {
        if (is.null(ci.arg.list) || all(is.na(pmatch(names(ci.arg.list), 
            "conf.int")))) {
            ci.arg.list <- c(ci.arg.list, list(conf.int = TRUE))
        }
        else {
            index <- (1:length(ci.arg.list))[!is.na(pmatch(names(ci.arg.list), 
                "conf.int"))]
            if (!unlist(ci.arg.list[index])) 
                ci.arg.list[[index]] <- TRUE
        }
    }
    dimnames(ci.mat) <- list(group.names, c("LCL", "UCL", "Conf.Level"))
    if (show.ci) {
        length.ci.offset <- length(ci.offset)
        if (!is.vector(ci.offset, mode = "numeric") || !all(is.finite(ci.offset)) || 
            any(ci.offset < .Machine$double.eps) || !(length.ci.offset %in% 
            c(1, n))) 
            stop(paste("'ci.offset' must be a positive scalar, or else a vector of", 
                "positive numbers with length equal to the number of groups"))
        if (length.ci.offset == 1) 
            ci.offset <- rep(ci.offset, n)
    }
    for (i in 1L:n) {
        x <- groups[[i]]
        y <- rep.int(at[i], length(x))
        if (method == 2L) 
            y <- y + stats::runif(length(y), -jitter, jitter)
        else if (method == 3L) {
            xg <- split(x, factor(x))
            xo <- lapply(xg, seq_along)
            x <- unlist(xg, use.names = FALSE)
            y <- rep.int(at[i], length(x)) + (unlist(xo, use.names = FALSE) - 
                1) * offset * csize
        }
        if (vertical) 
            points(y, x, col = col[(i - 1L)%%length(col) + 1L], 
                pch = pch[(i - 1L)%%length(pch) + 1L], cex = points.cex, 
                ...)
        else points(x, y, col = col[(i - 1L)%%length(col) + 1L], 
            pch = pch[(i - 1L)%%length(pch) + 1L], cex = points.cex, 
            ...)
        x <- x[is.finite(x)]
        n.vec[i] <- sum(is.finite(x))
        if (n.text != "none") {
            side <- ifelse(n.text == "top", 3, 1)
            mtext(paste("n=", n.vec[i], sep = ""), side = side, 
                line = n.text.line, at = at[i], cex = n.text.cex)
        }
        if (n.vec[i] == 0) 
            next
        location.vec[i] <- do.call(location.fcn, args = list(x = x))
        scale.vec[i] <- do.call(scale.fcn, args = list(x = x))
        if (location.scale.text != "none") {
            side <- ifelse(location.scale.text == "top", 3, 1)
            dum <- format(round(c(location.vec[i], scale.vec[i]), 
                location.scale.digits))
            if (ci.and.test == "parametric") {
                string1 <- "Mean="
                string2 <- "\nSD   ="
            }
            else {
                string1 <- "Median="
                string2 <- "\nIQR     ="
            }
            mtext(paste(string1, dum[1], string2, dum[2], sep = ""), 
                side = side, line = location.scale.text.line, 
                at = at[i], cex = location.scale.text.cex)
        }
        if (n.vec[i] >= 2) {
            sd.x <- sd(x)
            if (sd.x > 0) 
                ci.vec <- do.call(ci.fcn, args = c(list(x = x, 
                  conf.level = conf.level), ci.arg.list))$conf.int
            else ci.vec <- c(NA, NA)
            ci.mat[i, c("LCL", "UCL")] <- ci.vec
            ci.mat[i, "Conf.Level"] <- conf.level
        }
        if (show.ci) {
            if (vertical) {
                if (sd.x > 0 & n.vec[i] >= min.n.for.ci) 
                  errorBar(x = at[i] + ci.offset[i] * csize, 
                    y = location.vec[i], lower = ci.mat[i, "LCL"], 
                    upper = ci.mat[i, "UCL"], incr = FALSE, bar.ends = ci.bar.ends, 
                    gap = ci.bar.gap, add = TRUE, horizontal = FALSE, 
                    col = col[(i - 1L)%%length(col) + 1L], bar.ends.size = ci.bar.ends.size)
                points(at[i] + ci.offset[i] * csize, location.vec[i], 
                  pch = location.pch, col = col[(i - 1L)%%length(col) + 
                    1L], cex = location.cex)
            }
            else {
                if (sd.x > 0 & n.vec[i] >= min.n.for.ci) 
                  errorBar(x = location.vec[i], y = at[i] + ci.offset[i] * 
                    csize, lower = ci.mat[i, "LCL"], upper = ci.mat[i, 
                    "UCL"], incr = FALSE, bar.ends = ci.bar.ends, 
                    gap = ci.bar.gap, add = TRUE, horizontal = TRUE, 
                    col = col[(i - 1L)%%length(col) + 1L], bar.ends.size = ci.bar.ends.size)
                points(location.vec[i], at[i] + ci.offset[i] * 
                  csize, pch = location.pch, col = col[(i - 1L)%%length(col) + 
                  1L], cex = location.cex)
            }
        }
    }
    return.mat <- cbind(N = n.vec, Mean = location.vec, SD = scale.vec, 
        ci.mat)
    if (ci.and.test == "nonparametric") 
        dimnames(return.mat)[[2]][2:3] <- c("Median", "IQR")
    dimnames(return.mat)[[1]] <- group.names
    ret.list <- list(group.centers = at, group.stats = return.mat)
    if (p.value) {
        if (n == 1L) {
            p.val <- NA
            p.val.string <- ""
        }
        else if (any(sapply(groups, sd) > 0)) {
            if (n == 2) {
                if (ci.and.test == "nonparametric") {
                  test.fcn <- "wilcox.test"
                  if (is.null(test.arg.list) || all(is.na(pmatch(names(ci.arg.list), 
                    "conf.int")))) {
                    test.arg.list <- c(test.arg.list, list(conf.int = TRUE))
                  }
                  else {
                    index <- (1:length(test.arg.list))[!is.na(pmatch(names(test.arg.list), 
                      "conf.int"))]
                    if (!unlist(test.arg.list[index])) 
                      test.arg.list[[index]] <- TRUE
                  }
                  p.val.string <- "Wilcoxon p-value"
                }
                else {
                  test.fcn <- "t.test"
                  if (is.null(test.arg.list) || all(is.na(pmatch(names(test.arg.list), 
                    "var.equal")))) {
                    test.arg.list <- c(test.arg.list, list(var.equal = TRUE))
                    p.val.string <- "t-test p-value"
                  }
                  else {
                    index <- (1:length(test.arg.list))[!is.na(pmatch(names(test.arg.list), 
                      "var.equal"))]
                    if (!unlist(test.arg.list[index])) 
                      p.val.string <- "Welch t-test p-value"
                  }
                }
                alternative <- match.arg(alternative, c("two.sided", 
                  "less", "greater"))
                if (alternative != "two.sided") 
                  p.val.string <- paste(p.val.string, " (alternative='", 
                    alternative, "')", sep = "")
                test.list <- do.call(test.fcn, args = c(list(x = groups[[2]], 
                  y = groups[[1]], alternative = alternative, 
                  conf.level = group.difference.conf.level), 
                  test.arg.list))
                p.val <- test.list$p.value
                ci <- test.list$conf.int
            }
            else {
                y <- unlist(groups)
                group.n <- sapply(groups, length)
                group <- factor(rep(1:n, times = group.n))
                if (ci.and.test == "parametric") {
                  p.val.string <- "Anova p-value"
                  dum.aov <- aov(y ~ group)
                  dum.mat <- unclass(summary(dum.aov))[[1]]
                  p.val <- dum.mat[1, "Pr(>F)"]
                }
                else {
                  p.val.string <- "Kruskal-Wallis p-value"
                  test.list <- kruskal.test(y ~ group)
                  p.val <- test.list$p.value
                }
            }
            if (p.value & !is.na(p.val)) {
                p.val.to.show <- round(p.val, p.value.digits)
                p.val.to.show <- ifelse(p.val.to.show == 0, paste("<", 
                  format(5 * 10^-(p.value.digits + 1), scientific = FALSE)), 
                  paste("=", p.val.to.show))
                string <- paste(p.val.string, p.val.to.show)
                if (n == 2 & group.difference.ci) {
                  string1 <- ifelse(ci.and.test == "parametric", 
                    "% CI for Difference in Means: [", "% CI for Difference in Locations: [")
                  string <- paste(string, ";  ", round(100 * 
                    group.difference.conf.level, 0), string1, 
                    round(ci[1], group.difference.digits), ", ", 
                    round(ci[2], group.difference.digits), "]", 
                    sep = "")
                }
                mtext(string, line = p.value.line, cex = p.value.cex)
            }
        }
        else {
            p.val.string <- "p-value"
            p.val <- NA
            ci <- c(LCL = NA, UCL = NA)
            if (p.value) 
                warning("Constant values within each group, so between-group test not possible")
        }
        attr(p.val, "type") <- p.val.string
        ret.list <- c(ret.list, list(group.difference.p.value = p.val))
        if (n == 2) 
            ret.list <- c(ret.list, list(group.difference.conf.int = ci))
    }
    invisible(ret.list)
}

gofTest.default <-
function (y, x = NULL, test = ifelse(is.null(x), "sw", "ks"), 
    distribution = "norm", est.arg.list = NULL, alternative = "two.sided", 
    n.classes = NULL, cut.points = NULL, param.list = NULL, estimate.params = ifelse(is.null(param.list), 
        TRUE, FALSE), n.param.est = NULL, correct = NULL, digits = .Options$digits, 
    exact = NULL, ws.method = "normal scores", warn = TRUE, keep.data = TRUE, 
    data.name = NULL, data.name.x = NULL, parent.of.data = NULL, 
    subset.expression = NULL, ...) 
{
    test <- match.arg(test, c("sw", "sf", "ppcc", "ad", "cvm", 
        "lillie", "skew", "chisq", "ks", "ws", "proucl.ad.gamma", 
        "proucl.ks.gamma"))
    if (is.null(data.name)) 
        data.name <- deparse(substitute(y))
    y <- as.vector(unlist(y))
    if (!is.numeric(y) | length(y) == 0) 
        stop("'y' must be non-empty an all elements of 'y' must be a numeric")
    if ((bad.obs.y <- sum(!(y.ok <- is.finite(y)))) > 0) {
        if (warn) {
            warning(paste(bad.obs.y, "observations with NA/NaN/Inf in 'y' removed."))
        }
        y <- y[y.ok]
    }
    if (length(y) == 0) 
        stop("All observations in 'y' are NA/Nan/Inf")
    if (is.null(x)) {
        data.name.x <- NULL
    }
    else {
        if (test != "ks") 
            stop("When both 'x' and 'y' are supplied, you must set test='ks'")
        if (is.null(data.name.x)) 
            data.name.x <- deparse(substitute(x))
        x <- as.vector(unlist(x))
        if (!is.numeric(x) | length(x) == 0) 
            stop("'x' must be non-empty an all elements of 'x' must be a numeric")
        names(data.name.x) <- "x"
        names(data.name) <- "y"
        if ((bad.obs.x <- sum(!(ok <- is.finite(x)))) > 0) {
            if (warn) {
                warning(paste(bad.obs.x, "observations with NA/NaN/Inf in 'x' removed."))
            }
            x <- x[ok]
        }
        if (length(x) == 0) 
            stop("All observations in 'x' are NA/Nan/Inf")
    }
    if (test %in% c("sw", "sf", "ppcc", "ad", "cvm", "lillie")) {
        distribution <- check.distribution.args(distribution, 
            check.params = FALSE)$dist.abb
        if (distribution == "lnorm3" && test != "sw") 
            stop("When distribution = 'lnorm3' you must set test = 'sw'")
    }
    else if (test == "skew") {
        distribution <- match.arg(distribution, c("norm", "lnorm", 
            "lnormAlt", "zmnorm", "zmlnorm", "zmlnormAlt"))
    }
    else if (test == "chisq" | (test == "ks" & is.null(x))) {
        if (estimate.params) 
            check.da.list <- check.distribution.args(distribution, 
                check.params = FALSE)
        else {
            if (is.null(param.list)) 
                stop(paste("When 'estimate.params=FALSE' you must supply", 
                  "the argument 'param.list'"))
            check.da.list <- check.distribution.args(distribution, 
                param.list)
        }
        distribution <- check.da.list$dist.abb
        n.dist.params <- check.da.list$n.dist.params
    }
    else if (test %in% c("proucl.ad.gamma", "proucl.ks.gamma")) {
        distribution <- match.arg(distribution, c("gamma", "gammaAlt"))
    }
    if (test == "chisq") {
        not.miss.n.classes <- !missing(n.classes) & !is.null(n.classes)
        not.miss.cut.points <- !missing(cut.points) & !is.null(cut.points)
        if (not.miss.n.classes & not.miss.cut.points) 
            stop("When test='chisq' you cannot supply both 'n.classes' and 'cut.points'")
    }
    ws.method <- match.arg(ws.method, c("normal scores", "chi-square scores"))
    if (test == "ws" & missing(alternative)) 
        alternative <- "greater"
    else alternative <- match.arg(alternative, c("two.sided", 
        "less", "greater"))
    if (test %in% c("sw", "sf", "ppcc", "ad", "cvm", "lillie") & 
        !(distribution %in% c("norm", "lnorm", "lnormAlt", "lnorm3", 
            "zmnorm", "zmlnorm", "zmlnormAlt"))) {
        efcn <- paste("e", distribution, sep = "")
        if (EnvStats::Distribution.df[distribution, "Type"] != 
            "Continuous" || !exists(efcn, where = "package:EnvStats")) 
            stop(paste("When test=", test, ", the argument 'distribution' ", 
                "must indicate a continuous distribution, and ", 
                "there must exist a function to estimate the parameters", 
                sep = ""))
        test.name <- paste(test, "GeneralGofTest", sep = "")
    }
    else if (test == "proucl.ad.gamma") {
        test.name <- "adGammaProUCLGofTest"
    }
    else if (test == "proucl.ks.gamma") {
        test.name <- "ksGammaProUCLGofTest"
    }
    else test.name <- paste(test, "GofTest", sep = "")
    if (test == "ks") {
        if (is.null(x)) 
            ks.arg.list <- list(x = y, alternative = alternative, 
                exact = exact, distribution = distribution, param.list = param.list, 
                estimate.params = estimate.params, est.arg.list = est.arg.list, 
                n.param.est = n.param.est, digits = digits, data.name.x = data.name)
        else ks.arg.list <- list(x = x, y = y, alternative = alternative, 
            exact = exact, digits = digits, data.name.x = data.name.x, 
            data.name.y = data.name)
    }
    arg.list <- switch(test.name, swGofTest = , sfGofTest = , 
        ppccGofTest = , adGofTest = , cvmGofTest = , lillieGofTest = , 
        swGeneralGofTest = , sfGeneralGofTest = , ppccGeneralGofTest = , 
        adGeneralGofTest = , cvmGeneralGofTest = , lillieGeneralGofTest = list(x = y, 
            distribution = distribution, est.arg.list = est.arg.list), 
        skewGofTest = list(x = y, distribution = distribution, 
            est.arg.list = est.arg.list, alternative = alternative), 
        chisqGofTest = list(x = y, n.classes = n.classes, cut.points = cut.points, 
            distribution = distribution, param.list = param.list, 
            estimate.params = estimate.params, est.arg.list = est.arg.list, 
            n.param.est = n.param.est, correct = correct, digits = digits), 
        ksGofTest = ks.arg.list, wsGofTest = list(x = y, method = ws.method, 
            alternative = alternative), adGammaProUCLGofTest = , 
        ksGammaProUCLGofTest = list(x = y, distribution = distribution))
    ret.list <- do.call(test.name, args = arg.list)
    if (!keep.data) {
        ret.list <- ret.list[names(ret.list) != "data"]
        oldClass(ret.list) <- "gof"
    }
    ret.list$data.name <- c(data.name.x, data.name)
    if (!is.null(parent.of.data)) 
        ret.list$parent.of.data <- parent.of.data
    if (!is.null(subset.expression)) 
        ret.list$subset.expression <- subset.expression
    if (!is.null(x)) {
        bad.obs <- c(x = bad.obs.x, y = bad.obs.y)
    }
    else {
        bad.obs <- bad.obs.y
    }
    ret.list$bad.obs <- bad.obs
    ret.list
}

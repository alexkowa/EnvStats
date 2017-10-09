envstatsDistChoose <-
function (y, alpha = 0.05, method = "sw", choices = c("norm", 
    "gamma", "lnorm"), est.arg.list = NULL, data.name = NULL, 
    parent.of.data = NULL, subset.expression = NULL) 
{
    if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 
        0 || alpha >= 1) 
        stop("'alpha' must be a numeric scalar between 0 and 1")
    string <- paste("at ", 100 * alpha, "% Significance Level", 
        sep = "")
    method <- match.arg(method, c("sw", "sf", "ppcc", "ad", "cvm", 
        "lillie"))
    n.choices <- length(choices)
    if (!is.character(choices) || n.choices < 2) 
        stop("The argument 'choices' must be a character vector with at least 2 elements")
    dist.names <- rep("", n.choices)
    dist.types <- rep("", n.choices)
    for (i in 1:n.choices) {
        check.list <- check.distribution.args(choices[i], check.params = FALSE)
        choices[i] <- check.list$dist.abb
        dist.names[i] <- check.list$dist.name
        dist.types[i] <- check.list$dist.type
    }
    index <- !(dist.types %in% c("Continuous", "Mixed"))
    if (any(index)) {
        stop(paste("All choices for distributions must be for", 
            "Continuous or Mixed distributions.  This is not true for", 
            choices[index]))
    }
    if (!is.null(est.arg.list) && (!is.list(est.arg.list) || 
        !all(names(est.arg.list) %in% choices))) {
        stop(paste("The argument 'est.arg.list' must be a list, and", 
            "each component must have the name of the specified", 
            "distribution for which the argument(s) of that component apply.", 
            "For example, est.arg.list = list(gamma = list(method = 'bcmle'))"))
    }
    decision <- ""
    distribution.parameters <- NULL
    estimation.method <- NULL
    sample.size <- length(y)
    choice.list <- vector(mode = "list", length = n.choices)
    names(choice.list) <- choices
    p.value.vec <- rep(as.numeric(NA), n.choices)
    for (i in 1:n.choices) {
        choices.i <- choices[i]
        choice.list[[i]] <- gofTest(y, distribution = choices.i, 
            test = method, est.arg.list = est.arg.list[[choices.i]], 
            keep.data = FALSE, data.name = data.name, parent.of.data = parent.of.data, 
            subset.expression = subset.expression)
        p.value.vec[i] <- choice.list[[i]]$p.value
    }
    max.p.value <- max(p.value.vec)
    if (max.p.value <= alpha) 
        decision <- "Nonparametric"
    else {
        index <- which(p.value.vec == max.p.value)
        decision <- dist.names[index]
        distribution.parameters <- choice.list[[index]]$distribution.parameters
        estimation.method <- choice.list[[index]]$estimation.method
    }
    ret.list <- list(choices = dist.names, method = switch(method, 
        sw = "Shapiro-Wilk", sf = "Shapiro-Francia", ppcc = "PPCC", 
        ad = "Anderson-Darling", cvm = "Cramer-von Mises", lillie = "Lilliefors"), 
        decision = decision, alpha = alpha, distribution.parameters = distribution.parameters, 
        estimation.method = estimation.method, sample.size = sample.size, 
        test.results = choice.list)
    ret.list
}

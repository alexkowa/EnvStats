calibrate <-
function (formula, data, max.order = 4, p.crit = 0.05, weights, 
    subset, na.action, method = "qr", model = FALSE, x = FALSE, 
    y = FALSE, contrasts = NULL, ...) 
{
    fun.call <- match.call()
    fun.call[[1]] <- as.name("lm")
    fun.args <- is.element(arg.names(fun.call), names(formals(lm)))
    fun.call <- fun.call[c(TRUE, fun.args)]
    fit <- eval(fun.call)
    fit.terms <- terms(fit)
    name.pred <- attr(fit.terms, "term.labels")
    pred <- model.frame(fit)[, name.pred]
    if (any(duplicated(pred))) {
        aov.table <- anovaPE(fit)
        index <- grep("Lack of Fit", row.names(aov.table))
        lof.p <- aov.table[index, "Pr(>F)"]
        E <- 1
        try.new <- lof.p < p.crit
        while (try.new) {
            E <- E + 1
            formula.new <- eval(parse(text = paste(". ~  . + I(", 
                name.pred, "^", eval(E), ")", sep = "", collapse = "")))
            fit <- update(fit, formula = formula.new)
            aov.table <- anovaPE(fit)
            index <- grep("Lack of Fit", row.names(aov.table))
            lof.p <- aov.table[index, "Pr(>F)"]
            try.new <- (E < max.order) && (lof.p < p.crit)
        }
    }
    else {
        fit.new <- fit
        try.new <- TRUE
        E <- 1
        while (try.new) {
            fit.old <- fit.new
            E <- E + 1
            formula.new <- eval(parse(text = paste(". ~  . + I(", 
                name.pred, "^", eval(E), ")", sep = "", collapse = "")))
            fit.new <- update(fit.old, formula = formula.new)
            partial.F.p <- anova(fit.old, fit.new)[2, "Pr(>F)"]
            try.new <- (E < max.order) && (partial.F.p < p.crit)
        }
        if (E == max.order && partial.F.p < p.crit) 
            fit.old <- fit.new
        fit <- fit.old
    }
    if (!x) 
        fit <- update(fit, x = TRUE)
    fit
}

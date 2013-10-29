anovaPE <-
function (object) 
{
    if (data.class(object) != "lm") 
        stop("The argument \"object\" must be of class \"lm\".")
    name.x <- attr(terms(object), "term.labels")
    name.x.1 <- name.x[1]
    num.x <- length(name.x)
    if (num.x >= 2) {
        if (length(grep(name.x.1, name.x)) < num.x) 
            stop("There can only be one predictor variable in the model")
    }
    x <- model.frame(object)[, name.x.1]
    dcx <- data.class(x)
    if (!((dcx == "AsIs" & is.numeric(x)) || dcx == "numeric")) 
        stop("The single predictor variable must be numeric.")
    if (!any(duplicated(x))) 
        stop(paste("There must be at least two replicate values", 
            "for at least one value of the predictor variable."))
    new.object <- update(object, formula = formula(paste(". ~ . + as.factor(", 
        name.x[1], ")", sep = "")), singular = TRUE)
    aov.object <- summary.aov(new.object)[[1]]
    rn <- row.names(aov.object)
    rn[1 + num.x] <- "Lack of Fit"
    rn[2 + num.x] <- "Pure Error"
    row.names(aov.object) <- format(rn)
    aov.object
}

detectionLimitCalibrate <-
function (object, coverage = 0.99, simultaneous = FALSE) 
{
    if (!inherits(object, "lm")) 
        stop("The argument 'object' must inherit from the class 'lm'.")
    if (is.null(object$x)) 
        stop(paste("The argument 'object' must contain", "a component called 'x' that contains the model matrix.", 
            " That is, it must be the result of calling the lm", 
            "funciton with x=T"))
    x.name <- attr(object$terms, "term.labels")[1]
    dum.df <- data.frame(0)
    names(dum.df) <- x.name
    pred.y.at.x.eq.0 <- predict(object, newdata = dum.df, se.fit = TRUE)
    upl.y.at.x.eq.0 <- pointwise(pred.y.at.x.eq.0, coverage = coverage, 
        simultaneous = simultaneous, individual = TRUE)$upper
    fcn.to.min <- function(x.weird, y.weird, object.weird, x.name.weird, 
        coverage.weird, simultaneous.weird) {
        dum.df <- data.frame(x.weird)
        names(dum.df) <- x.name.weird
        pred.list <- predict(object.weird, newdata = dum.df, 
            se.fit = TRUE)
        sum((y.weird - pointwise(pred.list, coverage = coverage.weird, 
            simultaneous = simultaneous.weird, individual = TRUE)$lower)^2)
    }
    dl <- nlminb(start = 0, objective = fcn.to.min, lower = 0, 
        control = list(x.tol = .Machine$double.eps * 1e+08), 
        y.weird = upl.y.at.x.eq.0, object.weird = object, x.name.weird = x.name, 
        coverage.weird = coverage, simultaneous.weird = simultaneous)$par
    attr(dl, "coverage") <- coverage
    attr(dl, "simultaneous") <- simultaneous
    dl
}

detectionLimitCalibrate <-
function (object, coverage = 0.99, simultaneous = TRUE) 
{
    if (!inherits(object, "calibrate")) 
        stop("The argument 'object' must inherit from the class 'calibrate'.")
    if (is.null(object$x)) 
        stop(paste("The argument 'object' must contain", "a component called 'x' that contains the model matrix.", 
            " That is, it must be the result of calling the lm", 
            "funciton with x=TRUE"))
    x.name <- attr(object$terms, "term.labels")[1]
    dum.df <- data.frame(0) 
    #Test if the Intercept is negative. Usually the signal 
    #corresponding to the presence of an analyte shouldn't be negative
    #If the intecept is negative calculate the x value corresponding to 0 signal
    if (!is.na(object$coefficients["(Intercept)"])){ #test for forced origin 
      if (object$coefficients["(Intercept)"]< 0){
        dum.df<-data.frame(inversePredictCalibrate(object, obs.y=0,coverage = coverage,intervals = FALSE))['pred.x']
      }
    }
    names(dum.df) <- x.name
    pred.y.at.x.eq.0 <- predict(object, newdata = dum.df, se.fit = TRUE)
    upl.y.at.x.eq.0 <- pointwise(pred.y.at.x.eq.0, coverage = coverage, 
        simultaneous = simultaneous, individual = TRUE)$upper
    #One of the detection limits accepted by some Authorities 
    #corresponds to the 50% probability to be wrong  
    #considering not present an analyte (beta error).
    #I suggest to include this value on data returned by the function.
    dl.at.50.perc<-data.frame(inversePredictCalibrate(object,obs.y=upl.y.at.x.eq.0,coverage = coverage,intervals = FALSE))['pred.x']
    fcn.to.min <- function(x.weird, y.weird, object.weird, x.name.weird, 
        coverage.weird, simultaneous.weird) {
        dum.df <- data.frame(x.weird)
        names(dum.df) <- x.name.weird
        pred.list <- predict(object.weird, newdata = dum.df, 
            se.fit = TRUE)
        sum((y.weird - pointwise(pred.list, coverage = coverage.weird, 
            simultaneous = simultaneous.weird, individual = TRUE)$lower)^2) 
    }
    #start should be at least equal to the x value corresponding to 
    #detection limit at 50% beta error
    dl <- nlminb(start = dl.at.50.perc, objective = fcn.to.min, lower = 0, 
        control = list(x.tol = .Machine$double.eps * 1e+08), 
        y.weird = upl.y.at.x.eq.0, object.weird = object, x.name.weird = x.name, 
        coverage.weird = coverage, simultaneous.weird = simultaneous)$par
    #I have marked as comment the lines with the addition of 
    #the Detection limit 50% beta error because I don't know if
    #there would be compatibility problems with the in use version of
    #this function
    
    #dl.at.50.perc <- as.numeric(dl.at.50.perc)
    #dl.vec <- c(upl.y.at.x.eq.0, dl.at.50.perc, dl)
    #names(dl.vec) <- c("Decision Limit (Signal)", "Detection Limit 50% beta error","Detection Limit (Concentration)")
    
    dl.vec <- c(upl.y.at.x.eq.0, dl)
    names(dl.vec) <- c("Decision Limit (Signal)", "Detection Limit (Concentration)")
    attr(dl.vec, "coverage") <- coverage
    attr(dl.vec, "simultaneous") <- simultaneous
    dl.vec
}

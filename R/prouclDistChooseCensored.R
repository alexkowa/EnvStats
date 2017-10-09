prouclDistChooseCensored <-
function (x, censored, censoring.side = "left", alpha = 0.05, 
    data.name = NULL, censoring.name = NULL) 
{
    sample.size <- length(x)
    percent.censored <- 100 * sum(censored)/sample.size
    censoring.levels <- sort(unique(x[censored]))
    ret.list <- prouclDistChoose(y = x[!censored], alpha = alpha, 
        data.name = data.name)
    ret.list$method <- "ProUCL"
    proucl.sample.size <- ret.list$sample.size
    ret.list <- c(ret.list[c("choices", "method", "decision", 
        "alpha", "distribution.parameters", "estimation.method")], 
        list(sample.size = sample.size, censoring.side = censoring.side, 
            censoring.levels = censoring.levels, percent.censored = percent.censored, 
            proucl.sample.size = proucl.sample.size), ret.list[c("test.results", 
            "data.name")], list(censoring.name = censoring.name))
    ret.list
}

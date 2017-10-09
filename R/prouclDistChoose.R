prouclDistChoose <-
function (y, alpha = 0.05, data.name = NULL, parent.of.data = NULL, 
    subset.expression = NULL) 
{
    if (!is.numeric(alpha) || length(alpha) != 1 || !(alpha %in% 
        c(0.01, 0.05, 0.1))) 
        stop("The argument 'alpha' must be a numeric scalar equal to 0.01, 0.05, or 0.10")
    string <- paste("at ", 100 * alpha, "% Significance Level", 
        sep = "")
    decision <- ""
    distribution.parameters <- NULL
    estimation.method <- NULL
    sample.size <- length(y)
    norm.sw.list <- gofTest(y, distribution = "norm", test = "sw", 
        keep.data = FALSE, data.name = data.name, parent.of.data = parent.of.data, 
        subset.expression = subset.expression)
    norm.sw.p <- norm.sw.list$p.value
    norm.lillie.list <- gofTest(y, distribution = "norm", test = "lillie", 
        keep.data = FALSE, data.name = data.name, parent.of.data = parent.of.data, 
        subset.expression = subset.expression)
    norm.lillie.p <- norm.lillie.list$p.value
    if (all(c(norm.sw.p, norm.lillie.p) < alpha)) {
        norm.text <- paste("Data Not Normal", string)
    }
    else {
        if (any(c(norm.sw.p, norm.lillie.p) < alpha)) {
            norm.text <- paste("Data appear Approximate Normal", 
                string)
        }
        else {
            norm.text <- paste("Data appear Normal", string)
        }
        decision <- "Normal"
        distribution.parameters <- norm.sw.list$distribution.parameters
        estimation.method <- norm.sw.list$estimation.method
    }
    gamma.ad.list <- gofTest(y, distribution = "gamma", test = "proucl.ad.gamma", 
        keep.data = FALSE, data.name = data.name, parent.of.data = parent.of.data, 
        subset.expression = subset.expression)
    gamma.ad.p <- gamma.ad.list$p.value
    gamma.ks.list <- gofTest(y, distribution = "gamma", test = "proucl.ks.gamma", 
        keep.data = FALSE, data.name = data.name, parent.of.data = parent.of.data, 
        subset.expression = subset.expression)
    gamma.ks.p <- gamma.ks.list$p.value
    if (alpha == 0.01) {
        if (all(c(gamma.ad.p, gamma.ks.p) == "< 0.01")) {
            gamma.text <- paste("Data Not Gamma Distributed", 
                string)
        }
        else {
            if (any(c(gamma.ad.p, gamma.ks.p) == "< 0.01")) {
                gamma.text <- paste("Data appear Approximate Gamma Distributed", 
                  string)
            }
            else {
                gamma.text <- paste("Data appear Gamma Distributed", 
                  string)
            }
            if (decision == "") {
                decision <- "Gamma"
                distribution.parameters <- gamma.ad.list$distribution.parameters
                estimation.method <- gamma.ad.list$estimation.method
            }
        }
    }
    else if (alpha == 0.05) {
        if (all(c(gamma.ad.p, gamma.ks.p) %in% c("< 0.01", "0.01 <= p < 0.05"))) {
            gamma.text <- paste("Data Not Gamma Distributed", 
                string)
        }
        else {
            if (any(c(gamma.ad.p, gamma.ks.p) %in% c("< 0.01", 
                "0.01 <= p < 0.05"))) {
                gamma.text <- paste("Data appear Approximate Gamma Distributed", 
                  string)
            }
            else {
                gamma.text <- paste("Data appear Gamma Distributed", 
                  string)
            }
            if (decision == "") {
                decision <- "Gamma"
                distribution.parameters <- gamma.ad.list$distribution.parameters
                estimation.method <- gamma.ad.list$estimation.method
            }
        }
    }
    else {
        if (all(c(gamma.ad.p, gamma.ks.p) %in% c("< 0.01", "0.01 <= p < 0.05", 
            "0.05 <= p < 0.10"))) {
            gamma.text <- paste("Data Not Gamma Distributed", 
                string)
        }
        else {
            if (any(c(gamma.ad.p, gamma.ks.p) %in% c("< 0.01", 
                "0.01 <= p < 0.05", "0.05 <= p < 0.10"))) {
                gamma.text <- paste("Data appear Approximate Gamma Distributed", 
                  string)
            }
            else {
                gamma.text <- paste("Data appear Gamma Distributed", 
                  string)
            }
            if (decision == "") {
                decision <- "Gamma"
                distribution.parameters <- gamma.ad.list$distribution.parameters
                estimation.method <- gamma.ad.list$estimation.method
            }
        }
    }
    lnorm.sw.list <- gofTest(y, distribution = "lnorm", test = "sw", 
        keep.data = FALSE, data.name = data.name, parent.of.data = parent.of.data, 
        subset.expression = subset.expression)
    lnorm.sw.p <- lnorm.sw.list$p.value
    lnorm.lillie.list <- gofTest(y, distribution = "lnorm", test = "lillie", 
        keep.data = FALSE, data.name = data.name, parent.of.data = parent.of.data, 
        subset.expression = subset.expression)
    lnorm.lillie.p <- lnorm.lillie.list$p.value
    if (all(c(lnorm.sw.p, lnorm.lillie.p) < alpha)) {
        lnorm.text <- paste("Data Not Lognormal", string)
        if (decision == "") 
            decision <- "Nonparametric"
    }
    else {
        if (any(c(lnorm.sw.p, lnorm.lillie.p) < alpha)) {
            lnorm.text <- paste("Data appear Approximate Lognormal", 
                string)
        }
        else {
            lnorm.text <- paste("Data appear Lognormal", string)
        }
        if (decision == "") {
            decision <- "Lognormal"
            distribution.parameters <- lnorm.sw.list$distribution.parameters
            estimation.method <- lnorm.sw.list$estimation.method
        }
    }
    test.list = list(norm = list(norm.sw.list, norm.lillie.list, 
        text = norm.text), gamma = list(gamma.ad.list, gamma.ks.list, 
        text = gamma.text), lnorm = list(lnorm.sw.list, lnorm.lillie.list, 
        text = lnorm.text))
    names(test.list$norm) <- c(norm.sw.list$method, norm.lillie.list$method, 
        "text")
    names(test.list$gamma) <- c(gamma.ad.list$method, gamma.ks.list$method, 
        "text")
    names(test.list$lnorm) <- c(lnorm.sw.list$method, lnorm.lillie.list$method, 
        "text")
    ret.list <- list(choices = c("Normal", "Gamma", "Lognormal"), 
        method = "ProUCL", decision = decision, alpha = alpha, 
        distribution.parameters = distribution.parameters, estimation.method = estimation.method, 
        sample.size = sample.size, test.results = test.list)
    ret.list
}

StatTestText <- ggplot2::ggproto("StatTestText", ggplot2::Stat,
  required_aes = c("x", "y", "group"),

  setup_params = function(data, params) {
    if(!is.null(params$y.pos))
      return(params)

    range.y <- range(data$y, na.rm = TRUE)
    pos <- range.y[2] + diff(range.y) * params$y.expand.factor
      params$y.pos <- pos
    params
  },

  compute_panel = function(data, scales, y.pos, y.expand.factor,
    test, paired, test.arg.list, two.lines, p.value.digits, p.value.digit.type,
    location.digits, location.digit.type, nsmall) {

    unique.x <- unique(data$x)
    n.groups <- length(unique.x)

    if(n.groups <= 2) {
      if(n.groups == 1) {
# One-sample test

        if(test == "parametric") {

          test.arg.list <- modifyList(
            list(x = data$y, alternative = "two.sided", mu = 0, conf.level = 0.95),
            test.arg.list)

          p.val.string <- "t-test p-value"

          mu <- test.arg.list$mu
          if(mu != 0)
              p.val.string <- paste0(p.val.string, " (mu=", mu, ")")

          alternative <- test.arg.list$alternative
          if (alternative != "two.sided")
            p.val.string <- paste0(p.val.string,
              " (alternative='", alternative, "')")

          test.list <- do.call("t.test", test.arg.list)
          p.val <- test.list$p.value
          ci <- test.list$conf.int

          if(p.value.digit.type == "round") {
              p.val.to.show <- round(p.val, p.value.digits)
              p.val.to.show <- ifelse(p.val.to.show == 0, paste("<",
                  format(5 * 10^-(p.value.digits + 1), scientific = FALSE)),
                  paste("=", p.val.to.show))
          }
          else p.val.to.show <- signif(p.val, digits = p.value.digits)

          string <- paste(p.val.string, p.val.to.show)
          string2 <- "% CI for Mean: ["

          if(location.digit.type == "round") {
            ci <- round(ci, location.digits)
          }
          else {
            ci <- signif(ci, location.digits)
          }
          ci <- format(ci, nsmall = nsmall)

          string.sep <- ifelse(two.lines, "\n", ";  ")
          string <- paste0(string, string.sep, round(100 * test.arg.list$conf.level, 0),
              string2, ci[1], ", ", ci[2], "]")
        }
        else {

          test.arg.list <- modifyList(
            list(x = data$y, alternative = "two.sided", mu = 0, exact = NULL, correct = TRUE,
                conf.int = TRUE, conf.level = 0.95),
            test.arg.list)

          p.val.string <- "Wilcoxon p-value"

          mu <- test.arg.list$mu
          if(mu != 0)
              p.val.string <- paste0(p.val.string, " (mu=", mu, ")")

          alternative <- test.arg.list$alternative
          if (alternative != "two.sided")
            p.val.string <- paste0(p.val.string,
              " (alternative='", alternative, "')")

          test.list <- do.call("wilcox.test", test.arg.list)
          p.val <- test.list$p.value
          ci <- test.list$conf.int

          if(p.value.digit.type == "round") {
              p.val.to.show <- round(p.val, p.value.digits)
              p.val.to.show <- ifelse(p.val.to.show == 0, paste("<",
                  format(5 * 10^-(p.value.digits + 1), scientific = FALSE)),
                  paste("=", p.val.to.show))
          }
          else p.val.to.show <- signif(p.val, digits = p.value.digits)

          string <- paste(p.val.string, p.val.to.show)
          string2 <- "% CI for Pseudomedian: ["

          if(location.digit.type == "round") {
            ci <- round(ci, location.digits)
          }
          else {
            ci <- signif(ci, location.digits)
          }
          ci <- format(ci, nsmall = nsmall)

          string.sep <- ifelse(two.lines, "\n", ";  ")
          string <- paste0(string, string.sep,
              round(100 * attr(test.list$conf.int, "conf.level"), 0),
              string2, ci[1], ", ", ci[2], "]")
        }
      }
      else {
# Two-sample test
        if(test == "parametric") {
          if(paired){
            data.pair <- as.data.frame(do.call("cbind",split(-data$y,data$x)))
            colnames(data.pair) <- c("y1","y2")
            test.arg.list <- modifyList(
              list(formula = formula(Pair(y1,y2)~1), data = data.pair,
                   alternative = "two.sided", mu = 0,
                   var.equal = TRUE, conf.level = 0.95),
              test.arg.list)

          }else{
            test.arg.list <- modifyList(
              list(formula = formula(I(-y) ~ x), data = data,
                   alternative = "two.sided", mu = 0,
                   var.equal = TRUE, conf.level = 0.95),
              test.arg.list)
          }


          p.val.string <- "t-test p-value"

          if(paired)
            p.val.string <- paste("Paired", p.val.string)
          else {
            if(!test.arg.list$var.equal)
              p.val.string <- paste("Welch", p.val.string)
          }

          mu <- test.arg.list$mu
          if(mu != 0)
              p.val.string <- paste0(p.val.string, " (mu=", mu, ")")

          alternative <- test.arg.list$alternative
          if (alternative != "two.sided")
            p.val.string <- paste0(p.val.string,
              " (alternative='", alternative, "')")

          test.list <- do.call("t.test", test.arg.list)
          p.val <- test.list$p.value
          ci <- test.list$conf.int

          if(p.value.digit.type == "round") {
              p.val.to.show <- round(p.val, p.value.digits)
              p.val.to.show <- ifelse(p.val.to.show == 0, paste("<",
                  format(5 * 10^-(p.value.digits + 1), scientific = FALSE)),
                  paste("=", p.val.to.show))
          }
          else p.val.to.show <- signif(p.val, digits = p.value.digits)

          string <- paste(p.val.string, p.val.to.show)
          string2 <- "% CI for Difference in Means: ["

          if(location.digit.type == "round") {
            ci <- round(ci, location.digits)
          }
          else {
            ci <- signif(ci, location.digits)
          }
          ci <- format(ci, nsmall = nsmall)

          string.sep <- ifelse(two.lines, "\n", ";  ")
          string <- paste0(string, string.sep, round(100 * test.arg.list$conf.level, 0),
              string2, ci[1], ", ", ci[2], "]")
        }
        else {
          if(paired){
            data.pair <- as.data.frame(do.call("cbind",split(-data$y,data$x)))
            colnames(data.pair) <- c("y1","y2")
            test.arg.list <- modifyList(
              list(formula = formula(Pair(y1,y2)~1), data = data.pair,
                   alternative = "two.sided", mu = 0,
                   exact = NULL, correct = TRUE, conf.int = TRUE,
                   conf.level = 0.95),
              test.arg.list)

          }else{
            test.arg.list <- modifyList(
              list(formula = formula(I(-y) ~ x), data = data,
                   alternative = "two.sided", mu = 0,
                   exact = NULL, correct = TRUE, conf.int = TRUE, conf.level = 0.95),
              test.arg.list)
          }

          p.val.string <- "Wilcoxon p-value"

          if(paired)
            p.val.string <- paste("Paired", p.val.string)

          mu <- test.arg.list$mu
          if(mu != 0)
              p.val.string <- paste0(p.val.string, " (mu=", mu, ")")

          alternative <- test.arg.list$alternative
          if (alternative != "two.sided")
            p.val.string <- paste0(p.val.string,
              " (alternative='", alternative, "')")

          test.list <- do.call("wilcox.test", test.arg.list)
          p.val <- test.list$p.value
          ci <- test.list$conf.int

          if(p.value.digit.type == "round") {
              p.val.to.show <- round(p.val, p.value.digits)
              p.val.to.show <- ifelse(p.val.to.show == 0, paste("<",
                  format(5 * 10^-(p.value.digits + 1), scientific = FALSE)),
                  paste("=", p.val.to.show))
          }
          else p.val.to.show <- signif(p.val, digits = p.value.digits)

          string <- paste(p.val.string, p.val.to.show)
          string2 <- ifelse(paired,
              "% CI for Pseudomedian of Paired Differences: [",
              "% CI for Difference in Locations: ["
          )

          if(location.digit.type == "round") {
            ci <- round(ci, location.digits)
          }
          else {
            ci <- signif(ci, location.digits)
          }
          ci <- format(ci, nsmall = nsmall)

          string.sep <- ifelse(two.lines, "\n", ";  ")
          string <- paste0(string, string.sep,
              round(100 * attr(test.list$conf.int, "conf.level"), 0),
              string2, ci[1], ", ", ci[2], "]")
        }
      }
    }
    else {
# ANOVA
        if(test == "parametric") {
            p.val.string <- "ANOVA p-value"
            dum.mat <- unclass(summary(aov(y ~ x, data = data)))[[1]]
            p.val <- dum.mat[1, "Pr(>F)"]
        }
        else {
            p.val.string <- "Kruskal-Wallis p-value"
            test.list <- kruskal.test(y ~ x, data = data)
            p.val <- test.list$p.value
        }

        if(p.value.digit.type == "round") {
            p.val.to.show <- round(p.val, p.value.digits)
            p.val.to.show <- ifelse(p.val.to.show == 0, paste("<",
                format(5 * 10^-(p.value.digits + 1), scientific = FALSE)),
                paste("=", p.val.to.show))
        }
        else p.val.to.show <- signif(p.val, digits = p.value.digits)
        string <- paste(p.val.string, p.val.to.show)
    }

    data.frame(x = mean(unique.x), y = y.pos, label = string)
  }
)

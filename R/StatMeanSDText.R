StatMeanSDText <- ggplot2::ggproto("StatMeanSDText", ggplot2::Stat,

  required_aes = c("x", "y"), 

  setup_params = function(data, params) {
    if(!is.null(params$y.pos)) 
      return(params)

    range.y <- range(data$y, na.rm = TRUE)
    pos <- range.y[2] + diff(range.y) * params$y.expand.factor
    params$y.pos <- pos
    params
  },
  
  compute_panel = function(data, scales, y.pos, y.expand.factor, digits, digit.type, nsmall, family) {

    mean.sd.tibble <- dplyr::summarise(dplyr::group_by(data, x), 
        Mean = mean(y, na.rm = TRUE), 
        SD = sd(y, na.rm = TRUE)
    )
    Mean <- do.call(digit.type, list(x = unlist(mean.sd.tibble[, "Mean"]), digits = digits))
    SD   <- do.call(digit.type, list(x = unlist(mean.sd.tibble[, "SD"]),   digits = digits))

    dum.mat <- format(rbind(Mean, SD), nsmall = nsmall)
    lab <- paste0("Mean=", dum.mat[1, ], 
        ifelse(family == "mono", "\nSD  =", "\nSD   ="), 
        dum.mat[2, ])

    data.frame(x = unlist(mean.sd.tibble[, "x"]), y = y.pos, label = lab)
  }

)

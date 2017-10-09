StatMedianIQRText <- ggplot2::ggproto("StatMedianIQRText", ggplot2::Stat,

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

    median.iqr.tibble <- dplyr::summarise(dplyr::group_by(data, x), 
        Median = median(y, na.rm = TRUE), 
        IQR = iqr(y, na.rm = TRUE)
    )
    Median <- do.call(digit.type, list(x = unlist(median.iqr.tibble[, "Median"]), digits = digits))
    IQR    <- do.call(digit.type, list(x = unlist(median.iqr.tibble[, "IQR"]),    digits = digits))


    dum.mat <- format(rbind(Median, IQR), nsmall = nsmall)
    lab <- paste0("Median=", dum.mat[1, ], 
        ifelse(family == "mono", "\nIQR   =", "\nIQR    ="), 
        dum.mat[2, ])

    data.frame(x = unlist(median.iqr.tibble[, "x"]), y = y.pos, label = lab)
  }

)

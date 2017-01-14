StatNText <- ggplot2::ggproto("StatNText", ggplot2::Stat,

  required_aes = c("x", "y"), 

  setup_params = function(data, params) {
    if(!is.null(params$y.pos)) 
      return(params)

    range.y <- range(data$y, na.rm = TRUE)
    pos <- range.y[1] - diff(range.y) * params$y.expand.factor
    params$y.pos <- pos
    params
  },
  
  compute_panel = function(data, scales, y.pos, y.expand.factor) {
    n.tibble <- dplyr::summarise(dplyr::group_by(data, x), N = sum(!is.na(y)))

    n.vec <- unlist(n.tibble[, "N"])
    lab <- paste0("n=", n.vec)

    data.frame(x = unlist(n.tibble[, "x"]), y = y.pos, label = lab)
  }
)

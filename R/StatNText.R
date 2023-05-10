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
    n.vec <- as.vector(by(data$y, data$x, function(x) length(na.omit(x))))
    lab <- paste0("n=", n.vec)

    data.frame(x = as.vector(by(data$x, data$x, function(x)x[1])), y = y.pos, label = lab)
  }
)

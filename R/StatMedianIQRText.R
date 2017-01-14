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

stat_median_iqr_text <- function(mapping = NULL, data = NULL, geom = ifelse(text.box, "label", "text"), 
	position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,  
	y.pos = NULL, y.expand.factor = 0.2, digits = 1, digit.type = "round", 
	nsmall = ifelse(digit.type == "round", digits, 0), text.box = FALSE,  
	alpha = 1, angle = 0, color = "black", family = "", fontface = "plain", 
	hjust = 0.5, label.padding = ggplot2::unit(0.25, "lines"), 
	label.r = ggplot2::unit(0.15, "lines"), label.size = 0.25, 
	lineheight = 1.2, size = 4, vjust = 0.5, ...) {

	geom <- match.arg(geom, c("label", "text"))
	digit.type <- match.arg(digit.type, c("round", "signif"))

	params <- list(y.pos = y.pos, y.expand.factor = y.expand.factor,  
		digits = digits, digit.type = digit.type, nsmall = nsmall,
		alpha = alpha, angle = angle, color = color, family = family, 
		fontface = fontface, hjust = hjust, lineheight = lineheight, 
		size = size, vjust = vjust)
	if(geom == "label") {
		params <- c(params, 
			list(label.padding = label.padding, label.r = label.r, label.size = label.size, 
				na.rm = na.rm, ...)
		)
	}
	else {
		params <- c(params, na.rm = na.rm, ...)
	}


	ggplot2::layer(stat = StatMedianIQRText, data = data, mapping = mapping, geom = geom, 
		position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
		params = params)
}

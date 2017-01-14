stat_n_text <-
function (mapping = NULL, data = NULL, geom = ifelse(text.box, 
    "label", "text"), position = "identity", na.rm = FALSE, show.legend = NA, 
    inherit.aes = TRUE, y.pos = NULL, y.expand.factor = 0.1, 
    text.box = FALSE, alpha = 1, angle = 0, color = "black", 
    family = "", fontface = "plain", hjust = 0.5, label.padding = ggplot2::unit(0.25, 
        "lines"), label.r = ggplot2::unit(0.15, "lines"), label.size = 0.25, 
    lineheight = 1.2, size = 4, vjust = 0.5, ...) 
{
    geom <- match.arg(geom, c("label", "text"))
    params <- list(y.pos = y.pos, y.expand.factor = y.expand.factor, 
        alpha = alpha, angle = angle, color = color, family = family, 
        fontface = fontface, hjust = hjust, lineheight = lineheight, 
        size = size, vjust = vjust)
    if (geom == "label") {
        params <- c(params, list(label.padding = label.padding, 
            label.r = label.r, label.size = label.size, na.rm = na.rm, 
            ...))
    }
    else {
        params <- c(params, na.rm = na.rm, ...)
    }
    ggplot2::layer(stat = StatNText, data = data, mapping = mapping, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = params)
}

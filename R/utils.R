
#' Return a qualitative color palette depending on the number of values
#' of its `var` argument

qualitative_palette <- function(var, label, type = c("color", "fill")) {

  type <- match.arg(type)

  var <- var[!is.na(var)]
  n_values <- dplyr::n_distinct(var)

  if (n_values <= 9) {
    if (type == "color") {
      return(scale_color_brewer(label, palette = "Set1", na.value = "grey50"))
    }
    if (type == "fill") {
      return(scale_fill_brewer(label, palette = "Set1", na.value = "grey50"))
    }

  }

  if (n_values <= 12) {
    if (type == "color") {
      return(scale_color_brewer(label, palette = "Paired", na.value = "grey50"))
    }
    if (type == "fill") {
      return(scale_fill_brewer(label, palette = "Paired", na.value = "grey50"))
    }
  }

  ## n_values > 12
  if (type == "color") {
    return(ggsci::scale_color_d3(palette = "category20", name = label, na.value = "white"))
  }
  if (type == "fill") {
    return(ggsci::scale_fill_d3(palette = "category20", name = label, na.value = "white"))
  }


}

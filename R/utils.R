
#' Return a qualitative color palette depending on the number of values
#' of its `var` argument

qualitative_palette <- function(var, label) {
  var <- var[!is.na(var)]
  n_values <- dplyr::n_distinct(var)

  if (n_values <= 9) {
    return(scale_color_brewer(label, palette = "Set1", na.value = "grey50"))
  }
  if (n_values <= 12) {
    return(scale_color_brewer(label, palette = "Paired", na.value = "grey50"))
  }
  return(gsci::scale_color_d3(palette = "category20", name = label, na.value = "white"))
}

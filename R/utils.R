#' Return a qualitative color palette depending on the number of values
#' of its `var` argument
#'
#' @param var vector to compute palette values from
#' @param label title of generated paletee
#' @param type type of color palette
#'
#' @importFrom ggsci scale_color_d3 scale_fill_d3

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

#' Get leaves from a node
#'
#' @param tree tree object, such as a `clusters` element of a `umapscan` object
#' @param node node to find leaves from
#' @param parent optional parent of `node` (should not be used directly, only for recursive call).
#'
#' `parent` is used during computation to differentiate different 'Noise' nodes.
#'
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble

get_leaves <- function(tree, node = "", parent = NA) {

  if (node == "<Noise>" && is.na(parent)) {
    stop("Can't get leaves from a <Noise> node.")
  }

  children <- tree$id[tree$parent == node]

  if (length(children) == 0) {
    return(tibble::tibble(parent = parent, id = node))
  }
  leaves <- tibble::tibble(parent = character(0), id = character(0))
  for(child in children) {
    leaves <- dplyr::bind_rows(leaves, get_leaves(tree, child, parent = node))
  }
  return(leaves)
}


#' Get members from a node name
#'
#' @param us an umapscan object
#' @param id cluster id to get members from
#'
#' @import dplyr

get_members <- function(us, id) {

  if (id == "") {
    return(1:nrow(us$data))
  }

  if (id == "<Noise>") {
    stop("Can't get members from a <Noise> node.")
  }

  us$clusters %>%
    filter(.data$id == .env$id) %>%
    pull(.data$members) %>%
    unlist
}

#' Get members from a 'Noise' child of a node
#'
#' @param us an umapscan object
#' @param node node to get child 'Noise' members from
#'
#' @import dplyr

get_noise_members <- function(us, node) {

  if (node == "<Noise>") {
    stop("Can't get members from a <Noise> node.")
  }

  us$clusters %>%
    filter(.data$parent == node, .data$id == "<Noise>") %>%
    pull(.data$members) %>%
    unlist
}

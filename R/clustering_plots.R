#' Plot clusters of an umapscan object
#'
#' @param us umapscan object
#' @param parent name of the parent cluster
#' @param noise_inherit_parent if TRUE, 'Noise' points are given their parent cluster
#'   membership
#' @param alpha point transparency for clusters plot
#' @param ellipses if TRUE, plot confidence ellipses around clusters
#' @param fixed if TRUE, force coord_fixed on plot
#' @param labels if TRUE, use cluster label instead of id if available
#'
#' @seealso
#' [compute_clusters()], [describe_clusters()]
#' @export
#'
#' @examples
#' library(dplyr)
#' iris_num <- iris %>% select_if(is.numeric)
#' us <- new_umapscan(iris_num, n_neighbors = 25, min_dist = 0.1, seed = 1337)
#' us <- compute_clusters(us, minPts = 3, eps = 0.5, graph = FALSE)
#' plot_clusters(us, alpha = 1)
#' us <- compute_clusters(us, minPts = 3, eps = 0.45, graph = FALSE, parent = "3")
#' plot_clusters(us, alpha = 0.5, ellipses = FALSE, parent = "3")
#' plot_clusters(us)
#'
#' @import ggplot2
#' @importFrom tidyr drop_na

plot_clusters <- function(us, parent = "", noise_inherit_parent = FALSE, alpha = 1, ellipses = TRUE, fixed = FALSE, labels = TRUE) {

  clust <- get_clusters_membership(us, parent,
    noise_inherit_parent = noise_inherit_parent, labels = labels)
  if (all(is.na(clust))) stop("No defined clusters in umapscan object.")
  d_clust <- us$umap[!is.na(clust),]
  clust <- clust[!is.na(clust)]
  clust[clust == "<Noise>"] <- NA
  d_clust$cluster <- clust
  color_scale <- qualitative_palette(clust, label = "Cluster")

  g <- ggplot(d_clust, aes(x = .data$.umap_x, y = .data$.umap_y, color = factor(.data$cluster))) +
    geom_point(size = 1, alpha = alpha) +
    color_scale +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    xlab("") +
    ylab("")

  if (ellipses) {
    d_ellipses <- d_clust %>% tidyr::drop_na(.data$cluster)
    g <- g + stat_ellipse(data = d_ellipses, na.rm = TRUE)
  }

  if (fixed) {
    g <- g + coord_fixed()
  }

  g
}


#' Describe clusters of an umapscan object
#'
#' @param us umapscan object to describe clusters
#' @param parent name of the parent cluster
#' @param labels if TRUE, use cluster label instead of id if available
#' @param type plot type, either `"boxplot"`, `"ridges"`, `"barplot"` or `"keyness"`
#' @param position if type = "barplot", `position` argument to add to `geom_bar`
#' @param keyness_measure if type = "keyness", passed as `measure` argument to `quanteda::textstat_keyness`
#' @param n_terms if type = "keyness", number of terms to show
#' @param text_size if type = "keyness", plot text size
#' @param free_scale if type = "keyness", don't use the same scale on each barplot
#' @param show_negative if TRUE and type = "keyness", show negative keyness features

#'
#' @details
#' "boxplot" and "ridges" are suitable for continuous variables, "barplot" is better for
#' binary categorical ones. "keyness" is suitable when the original data is a document-feature
#' matrix.
#'
#' @seealso
#' [compute_clusters()], [get_cluster_data()], [get_clusters_membership()],
#' [rename_cluster()]
#'
#'
#'
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom ggridges geom_density_ridges
#' @import dplyr
#'
#' @examples
#' library(dplyr)
#' iris_num <- iris %>% select_if(is.numeric)
#' us <- new_umapscan(iris_num, n_neighbors = 25, min_dist = 0.1, seed = 1337)
#' us <- compute_clusters(us, minPts = 3, eps = 0.5)
#' describe_clusters(us)
#' us <- compute_clusters(us, minPts = 3, eps = 0.45, parent = "3")
#' describe_clusters(us, type = "ridge")
#' describe_clusters(us, parent = "3")

describe_clusters <- function(
  us, parent = "", labels = TRUE,
  type = c("boxplot", "ridges", "barplot", "keyness"),
  position = "fill",
  keyness_measure = c("chi2", "lr", "exact", "pmi"),
  n_terms = 20,
  text_size = 10,
  free_scale = TRUE,
  show_negative = TRUE) {

  type <- match.arg(type)
  keyness_measure <- match.arg(keyness_measure)

  clusters <- get_clusters_membership(us, parent, noise_inherit_parent = FALSE, labels = labels)
  clusters[clusters == "<Noise>"] <- NA
  select <- !is.na(clusters)
  d <- us$data %>% dplyr::filter(select)
  clusters <- clusters[select]

  if (type != "keyness") {
    d$cluster <- clusters

    d_long <- d %>%
      tidyr::pivot_longer(-.data$cluster) %>%
      dplyr::mutate(
        cluster = factor(.data$cluster)
      )
  }

  if (type == "boxplot") {

    fill_scale <- qualitative_palette(d_long$cluster, "Cluster", type = "fill")

    g <- ggplot(d_long, aes(x = .data$cluster, y = .data$value, fill = .data$cluster)) +
      geom_boxplot(outlier.shape = NA) +
      fill_scale +
      facet_wrap(~name, scales = "free_y") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      xlab("") +
      ylab("")
  }

  if (type == "ridges") {

    fill_scale <- qualitative_palette(d_long$cluster, "Cluster", type = "fill")

    g <- ggplot(d_long, aes(x = .data$value, y = .data$cluster, fill = .data$cluster)) +
      ggridges::geom_density_ridges(alpha = .7) +
      fill_scale +
      facet_wrap(~name, scales = "free_x") +
      guides(fill = guide_legend(override.aes = list(alpha = 1)))
  }

  if (type == "barplot") {

    fill_scale <- qualitative_palette(d_long$value, "Value", type = "fill")

    g <- ggplot(d_long, aes(x = .data$cluster, fill = factor(.data$value))) +
      geom_bar(position = position) +
      fill_scale +
      facet_wrap(~name, scales = "free_y") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      xlab("") +
      ylab("")

  }

  if (type == "keyness") {

    g <- describe_clusters_keyness(
      d, clusters,
      keyness_measure = keyness_measure,
      n_terms = n_terms,
      text_size = text_size,
      free_scale = free_scale,
      show_negative = show_negative
    )

    return(invisible(g))

  }

  g

}




## Keyness-based clusters description

describe_clusters_keyness <- function(
  d, clusters,
  keyness_measure = c("chi2", "lr", "exact", "pmi"),
  n_terms = 20,
  text_size = 10,
  free_scale = TRUE,
  show_negative = TRUE) {

  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package \"gridExtra\" needed for this function to work. Please install it.",
      call. = FALSE)
  }

  tabs <- keyness_stats(
    clusters,
    d,
    keyness_measure = keyness_measure,
    n_terms = n_terms,
    show_negative = show_negative
  )

  stat_col <- switch(
    keyness_measure,
    "chi2" = "chi2",
    "lr" = "G2",
    "exact" = "or",
    "pmi" = "pmi"
  )

  if (free_scale) {
    maxs <- purrr::map_dbl(tabs, ~{max(abs(.x[[stat_col]]), na.rm = TRUE)})
    range <- c(0, max(maxs))
  } else {
    range <- NULL
  }

  plots <- purrr::imap(tabs, function(tab, name) {
    keyness_barplot(tab, range = range, title = name,
      stat_col = stat_col, n_terms = n_terms, text_size = text_size)
  })

  lay <- matrix(seq_along(tabs), nrow = 1)
  g <- gridExtra::grid.arrange(grobs = plots, layout_matrix = lay)

}


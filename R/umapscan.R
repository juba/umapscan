#' Compute UMAP embeddings from a numerical data frame
#'
#' @param d a numerical matrix or data frame, or quanteda dfm
#' @param n_neighbors `n_neighbors` argument passed to [uwot::umap()]
#' @param min_dist `min_dist` argument passed to [uwot::umap()]
#' @param metric `metric` argument passed to [uwot::umap()]
#' @param scale scaling to be applied to the variables. Value passed to [uwot::umap()].
#' @param seed integer seed passed to [base::set.seed()] before umap embeddings for
#'   reproductibility
#' @param data_sup a data.frame of supplementary variables not used for UMAP
#'   embeddings but for results visualisation
#' @param ... other arguments passed to [uwot::umap()]
#'
#' @return
#' A list of class `umapscan` with the following components :
#' - `umap` : results of UMAP embeddings
#' - `data` : original numerical dataset
#' - `data_sup` : supplementary dataset
#' - `clusters` : a tree-like hierarchy of clusters
#' - `call` : function call
#' - `seed` : the seed used for random numbers for all umap and dbscan operations
#'
#' @seealso [uwot::umap()]
#'
#' @export
#' @importFrom uwot umap
#' @importFrom tibble tibble
#'
#' @examples
#' library(dplyr)
#' iris_num <- iris %>% select_if(is.numeric)
#' iris_sup <- iris %>% select(Species)
#' new_umapscan(iris_num, data_sup = iris_sup)


new_umapscan <- function(
  d,
  n_neighbors = 15,
  min_dist = 0.01,
  metric = "euclidean",
  scale = FALSE,
  seed,
  data_sup = NULL,
  ...
) {

  # quanteda dfm input
  if (inherits(d, "dfm")) {
    if (!requireNamespace("quanteda", quietly = TRUE)) {
      stop("Package \"quanteda\" needed for this function to work. Please install it.",
        call. = FALSE)
    }
    ## convert to matrix first to avoid the creation of a possibly
    ## duplicated "document" column
    d <- data.frame(quanteda::convert(d, "matrix"))
  }
  # matrix input
  if (inherits(d, "matrix")) {
    d <- as.data.frame.matrix(d)
  }
  # else
  if (!inherits(d, "data.frame")) {
    stop("d must be a numerical matrix or data frame.")
  }
  # data_sup
  if (!is.null(data_sup)) {
    if (!inherits(data_sup, "data.frame")) stop("data_sup must be a data frame.")
    if (nrow(data_sup) != nrow(d)) stop("d and data_sup must have the same number of rows.")
  }
  # check if numerical
  if (!all(apply(d, 2, is.numeric))) {
    warning("some columns are not numerical.")
  }

  d_comp <- d

  if (missing(seed)) { seed <- round(stats::runif(1) * 100000) }

  set.seed(seed, kind = "default", normal.kind = "default", sample.kind = "default")
  umap <- uwot::umap(
    d_comp,
    n_neighbors = n_neighbors,
    min_dist = min_dist,
    metric = metric,
    scale = scale,
    n_sgd_threads = 0,
    approx_pow = TRUE,
    ...
  )
  umap <- tibble::tibble(
    .umap_x = umap[,1],
    .umap_y = umap[,2],
  )

  clusters <- tibble::tibble(
    parent = character(0),
    id = character(0),
    n = integer(0),
    members = list(),
    level = integer(0),
    label = character(0)
  )

  res <- list(
    umap = umap,
    data = d,
    clusters = clusters,
    data_sup = data_sup,
    call = match.call(),
    seed = seed
  )

  class(res) <- c(class(res), "umapscan")

  res
}


#' Print an umapscan object
#'
#' @param x umapscan object to be printed
#' @param ... arguments passed to other methods
#'
#' @export
#' @importFrom data.tree as.Node

print.umapscan <- function(x, ...) {

  cat("\nCall: ", paste(deparse(x$call), sep = "\n", collapse = "\n"),
    "\n\n", sep = "")
  dims <- dim(x$data)
  cat("UMAP embeddings of a ", dims[1], "x", dims[2], " data frame\n", sep = "")

  if (!is.null(x$data_sup)) {
    dims_sup <- dim(x$data_sup)
    cat ("with a ", dims_sup[1], "x", dims_sup[2],
      " data frame of supplementary data\n", sep = "")
  }

  cat("\nClusters :")
  if (nrow(x$clusters) > 0) {
    cat("\n\n")
    print(data.tree::as.Node(x$clusters, mode = "network"), "label", "n")
  } else {
    cat(" <none>\n\n")
  }

  invisible(x)
}



#' Plot an umapscan object
#'
#' @param x umapscan object to be plotted
#' @param color variable to highlight points (`tidylang` style). Must be a variable
#'   of the `data` or `data_sup` elements of `us`
#' @param palette `viridis` (for continuous `color`) or `brewer` (for categorical
#'   `color`) palette name
#' @param label legend label
#' @param ellipses if TRUE and `color` is categorical, add repartition ellipses
#' @param alpha points transparency
#' @param fixed if TRUE, add [ggplot2::coord_fixed()] to plot
#' @param ... arguments passed to other methods
#'
#' @examples
#' library(dplyr)
#' iris_num <- iris %>% select_if(is.numeric)
#' iris_sup <- iris %>% select(Species)
#' us <- new_umapscan(iris_num, data_sup = iris_sup, n_neighbors = 25, min_dist = 0.1)
#' plot(us, color = Species, ellipses = TRUE)
#' plot(us, color = Sepal.Width, palette = "viridis", alpha = 0.5, fixed = TRUE)
#'
#' @export
#' @import ggplot2
#' @import dplyr

plot.umapscan <- function(
  x, color, palette, label = NULL, ellipses = FALSE, alpha = 0.2, fixed = FALSE, ...
) {

  if (!inherits(x, "umapscan")) stop("`x` must be an object of class umapscan.")

  d <- dplyr::bind_cols(x$umap, x$data, x$data_sup)

  g <- ggplot(d, aes(x = .data$.umap_x, y = .data$.umap_y)) +
    xlab("") +
    ylab("")

  if (fixed) {
    g <- g + coord_fixed()
  }

  if (missing(color)) {
    g <- g + geom_point(alpha = alpha)
    return(g)
  }

  g <- g + geom_point(aes(color = {{color}}), alpha = alpha)

  col_var <- d %>%
    mutate(.umapscan_col_var = {{color}}) %>%
    pull(.data$.umapscan_col_var)

  if (is.numeric(col_var)) {
    if (missing(palette)) palette <- "inferno"
    g <- g + scale_color_viridis_c(label, option = palette)
  } else {
    if (missing(palette)) {
      scale <- qualitative_palette(col_var, label)
    } else {
      scale <- scale_color_brewer(label, palette = palette)
    }
    g <- g + scale
  }

  if (ellipses) {
    g <- g + stat_ellipse(aes(color = {{color}}))
  }

  g

}



#' Display an umapscan clustering as a leaflet map.
#'
#' @param us umapscan object to be plotted
#' @param point_labels a character vector of point labels displayed
#'   in a popup on hover. Can contain HTML.
#' @param labels if TRUE, us labels instead of identifiers to identify clusters
#'
#' @export
#' @importFrom purrr map
#' @importFrom shiny HTML
#' @import leaflet

map_plot <- function(us, point_labels = NULL, labels = TRUE) {

  ## Generate data for each clustering level
  levels <- purrr::map(1:max(us$clusters$level), function(level) {

    clusters <- clust_members(us, max_level = level, labels = labels)
    clusters[clusters == "<Noise>"] <- NA
    suppressWarnings({
      pal <- leaflet::colorFactor(
        palette = "Paired", domain = clusters, na.color = "white"
      )
    })

    clusters_labels <- us$umap %>%
      mutate(clust = clusters) %>%
      tidyr::drop_na(.data$clust) %>%
      group_by(.data$clust) %>%
      summarise(.umap_x = mean(.data$.umap_x), .umap_y = mean(.data$.umap_y))

    list(clusters = clusters, palette = pal, clusters_labels = clusters_labels)
  })

  ## Map object
  map <- leaflet::leaflet(
    us$umap,
    options = leaflet::leafletOptions(preferCanvas = TRUE)
  )

  ## Data point labels
  if (!is.null(point_labels)) {
    point_labels <- purrr::map(point_labels, shiny::HTML)
  }

  # Add group layers
  current_group <- 0
  for (level in levels) {
    current_group <- current_group + 1
    group <- paste("Level", current_group)
    suppressWarnings({
      map <- map %>%
        ## Add data points
        leaflet::addCircleMarkers(
          lng = ~.umap_x, lat = ~.umap_y,
          stroke = FALSE,
          radius = 5,
          fillColor = level$palette(level$clusters),
          fillOpacity = 0.5,
          label = point_labels,
          group = group
        ) %>%
        ## Add cluster points
        leaflet::addCircleMarkers(
          data = level$clusters_labels,
          lng = ~.umap_x, lat = ~.umap_y,
          label = ~clust,
          labelOptions = leaflet::labelOptions(permanent = TRUE, direction = "top"),
          fillColor = ~level$palette(clust),
          fillOpacity = 0.9,
          radius = 10,
          weight = 2,
          opacity = 1,
          color = "white",
          group = group
        ) %>%
        leaflet::hideGroup(group)
    })
  }

  map <- map %>%
    leaflet::showGroup("Level 1") %>%
    leaflet::addLayersControl(
      baseGroups = paste("Level", seq_along(levels)),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  map

}






